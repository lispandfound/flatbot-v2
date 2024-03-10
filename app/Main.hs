{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad (guard, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Attoparsec.Text (takeText)
import Data.Attoparsec.Text qualified as AP
import Data.Functor (($>))
import Data.List (intersperse)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock
import Data.Time.LocalTime (LocalTime (localTimeOfDay), TimeOfDay (..), ZonedTime (zonedTimeToLocalTime), getCurrentTimeZone, localTimeToUTC, utcToLocalZonedTime)
import Database.SQLite.Simple (Connection, execute_, open)
import Debt
import DueDate
import Fmt
import Reminder (getRemindersAtOrBefore)
import Reminder qualified as R
import System.Environment (getEnv)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (updateMessageText)

data Model = Model
  { dbConnection :: Connection
  }

type Amount = Double

data Action
  = AddDebt Chat User [User] Amount Text
  | SplitDebt Chat User [User] Amount Text
  | TallyChat Chat
  | DebtHistory Chat User User
  | SettleDebts Chat User User
  | SendHelp Chat
  | SendSetup Chat
  | AddReminder Chat User DueDate Text
  | DeleteReminder Integer
  | PickReminder Chat
  deriving (Show)

flatbot :: Model -> BotApp Model Action
flatbot model =
  BotApp
    { botInitialModel = model,
      botAction = updateToAction,
      botHandler = handleAction,
      botJobs =
        [ BotJob
            { botJobSchedule = "* * * * *",
              botJobTask = nagUsers
            },
          BotJob
            { botJobSchedule = "* * * * *",
              botJobTask = remindUsers
            }
        ]
    }

data FlatbotConfig = FlatbotConfig
  { token :: Token,
    dbPath :: String
  }

data Command = Owes | Tally | Split | Settle | Help | History | Remind | Unremind deriving (Eq)

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = asum . map f

substring :: Int -> Int -> Text -> Text
substring start len = Text.take len . Text.drop start

updateCommand :: Update -> Maybe Command
updateCommand update = do
  msg <- updateMessage update
  text <- updateMessageText update
  entities <- messageEntities msg
  findMaybe
    ( \ent -> case messageEntityType ent of
        MessageEntityBotCommand -> parseCommand (substring (messageEntityOffset ent) (messageEntityLength ent) text)
        _ -> Nothing
    )
    entities
  where
    parseCommand :: Text -> Maybe Command
    parseCommand "/owes" = Just Owes
    parseCommand "/tally" = Just Tally
    parseCommand "/settle" = Just Settle
    parseCommand "/help" = Just Help
    parseCommand "/split" = Just Split
    parseCommand "/history" = Just History
    parseCommand "/remind" = Just Remind
    parseCommand "/unremind" = Just Unremind
    parseCommand _ = Nothing

updateMentions :: Update -> [User]
updateMentions update = fromMaybe [] $ do
  msg <- updateMessage update
  entities <- messageEntities msg
  pure $ mapMaybe messageEntityUser entities

strippedMessageText :: Update -> Text
strippedMessageText update = Text.pack . map snd . filter (\(i, _) -> all (i `outside`) entityBounds) . zip [0 ..] . Text.unpack . fromMaybe "" . updateMessageText $ update
  where
    entityBounds = map (\ent -> (messageEntityOffset ent, entityEnd ent)) entities
    outside i (lb, ub) = i < lb || i > ub
    entities = fromMaybe [] (updateMessage update >>= messageEntities)
    entityEnd ent = messageEntityOffset ent + messageEntityLength ent

updateFrom :: Update -> Maybe User
updateFrom upd = updateMessage upd >>= messageFrom

updateChat :: Update -> Maybe Chat
updateChat = fmap messageChat . updateMessage

roundCents :: Double -> Double
roundCents = (/ 100) . fromInteger . round . (* 100)

handleCallbackQuery :: Update -> Maybe Action
handleCallbackQuery update = do
  query <- updateCallbackQuery update
  data_ <- callbackQueryData query
  either (const Nothing) Just $ AP.parseOnly callBackParser data_
  where
    callBackParser = DeleteReminder <$> ("DeleteReminder " *> AP.decimal)

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ =
  handleCallbackQuery update
    <|> case (updateChat update, updateCommand update, updateFrom update, updateMentions update, strippedMessageText update) of
      (Just chat, Just Owes, Just receivable, payables, msg) | not (null payables) -> either (const Nothing) (\(amount, reason) -> Just $ AddDebt chat receivable payables amount reason) (parseDebt msg)
      (Just chat, Just Split, Just receivable, payables, msg) | not (null payables) -> either (const Nothing) (\(amount, reason) -> Just $ SplitDebt chat receivable payables amount reason) (parseDebt msg)
      (Just chat, Just Tally, _, _, _) -> Just $ TallyChat chat
      (Just chat, Just Settle, Just payable, [receivable], _) -> Just $ SettleDebts chat payable receivable
      (Just chat, Just Help, _, _, _) -> Just $ SendHelp chat
      (Just chat, Just History, Just receivable, [payable], _) -> Just $ DebtHistory chat receivable payable
      (Just chat, Just Remind, _, [remindee], text) -> case AP.parse duedate (Text.toLower text) of
        AP.Done rest dd -> Just $ AddReminder chat remindee dd (Text.strip . Text.drop (Text.length text - Text.length rest) $ text)
        _ -> Nothing
      (Just chat, Just Unremind, _, _, _) -> Just (PickReminder chat)
      _ | isJust (updateMyChatMember update) -> Just (SendSetup (chatMemberUpdatedChat . fromJust . updateMyChatMember $ update))
      _ -> Nothing
  where
    parseDebt = AP.parseOnly debtParser
    spaces = void $ AP.takeTill (/= ' ')
    currency = roundCents <$> (optional "$" *> AP.double)
    debtParser = do
      spaces
      amount <- currency
      guard $ amount > 0
      spaces
      reason <- takeText
      return (amount, reason)

createDebt :: Chat -> User -> User -> Double -> Text -> Debt
createDebt chat receivable payable amount reason = makeDebt chatId_ receivableId (userFirstName receivable) payableId (userFirstName payable) amount reason
  where
    ChatId chatId_ = chatId chat
    UserId receivableId = userId receivable
    UserId payableId = userId payable

helpMessage :: Text
helpMessage =
  Text.unlines
    [ "Hi, resurrected flatbot here! Here is how to use me:",
      "",
      "To say that Daniel owes me $100, type:",
      "<pre>/owes @Daniel $100</pre>",
      "The <code>@</code> is important, it will pop up with people to select when you do this.",
      "",
      "If you paid a $100 power bill and want to split that evenly between yourself and Daniel and Josh",
      "<pre>/split @Daniel @Josh $100</pre>",
      "This will record a debt of $33.33 owed to you from Daniel and Josh",
      "",
      "To get a detailed history of every outstanding debt owed between yourself and Daniel:",
      "<pre>/history @Daniel</pre>",
      "",
      "To get the current tally of all debts in the chat, type:",
      "<pre>/tally</pre>",
      "",
      "To settle debts (both debts payable from you, and receivable to you) with Daniel, use:",
      "<pre>/settle @Daniel</pre>",
      "Again the <code>@</code> is important."
    ]

currencyF :: Double -> Builder
currencyF d = if df < 0 then "-$" else "$" +| commaizeF (abs df) |+ "." +| padRightF 2 '0' cents
  where
    df :: Integer
    df = floor d
    cents :: Integer
    cents = round (d * 100) `mod` 100

userNameF :: User -> Builder
userNameF user = "" +| userFirstName user |+ ""

listWithF :: (a -> Builder) -> [a] -> Builder
listWithF f = mconcat . intersperse ", " . map f

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  AddDebt chat receivable payables amount reason ->
    model
      <# let debts = map (\payable -> createDebt chat receivable payable amount reason) payables
          in do
               liftIO $ mapM_ (insertDebt (dbConnection model)) debts
               pure $ userReplyMessage receivable payables amount reason
  SplitDebt chat receivable payables amount reason ->
    let splitAmount = roundCents $ amount / fromIntegral (length payables + 1)
        debts = map (\payable -> createDebt chat receivable payable splitAmount reason) payables
     in model <# do
          liftIO $ mapM_ (insertDebt (dbConnection model)) debts
          pure $ userReplyMessage receivable payables splitAmount reason
  TallyChat chat ->
    model <# do
      let ChatId chatId_ = chatId chat
       in do
            debts <- liftIO $ getChatDebts (dbConnection model) chatId_
            let tallied = tallyDebts debts
             in do
                  if tallied == mempty then pure "No debts are currently owed anywhere!" else pure $ tallyReplyMessage tallied
  SettleDebts chat payable receivable ->
    model
      <# let ChatId chatId_ = chatId chat
          in do
               settlementAmount <- liftIO $ tallyDebt (dbConnection model) chatId_ (unwrapUserId receivable) (unwrapUserId payable)
               liftIO . markDebtsRepaid (dbConnection model) chatId_ (unwrapUserId payable) . unwrapUserId $ receivable
               pure $ settlementMessage payable receivable (abs settlementAmount)
  DebtHistory chat receivable payable ->
    let ChatId chatId_ = chatId chat
        UserId payableId = userId payable
        UserId receivableId = userId receivable
     in model <# do
          debts <- liftIO $ getDebtsBetween (dbConnection model) chatId_ receivableId payableId
          if null debts
            then pure $ userNameF receivable |+ " and " +| userNameF payable |+ " have no outstanding debts."
            else pure . foldMap (\d -> debtMessage (receivableUserName d) (payableUserName d) (amount d) (if reason d /= "" then Just (reason d) else Nothing)) $ debts
  AddReminder chat remindee dd reason ->
    model <# do
      liftIO $ do
        now <- getCurrentLocalTime
        zone <- getCurrentTimeZone
        let nextDate = nextLocalDueDate now dd
            nextDateUTC = localTimeToUTC zone nextDate
            period = recurrencePeriod dd
            reminder = R.mkReminder (unwrapChatId chat) (unwrapUserId remindee) (userFirstName remindee) reason nextDateUTC period
        R.insertReminder (dbConnection model) reminder
      return $ reminderMessage remindee dd reason
  PickReminder chat ->
    model <# do
      reminderList <- liftIO $ R.getReminders (dbConnection model) (unwrapChatId chat)
      let msg =
            (toEditMessage "Select a reminder to delete")
              { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup (reminderKeyboard reminderList)
              }
      replyOrEdit msg
  DeleteReminder rid_ ->
    model <# do
      liftIO $ R.deleteReminder (dbConnection model) rid_
      editUpdateMessageText ("Reminder deleted" :: Text)
  SendHelp chat -> model <# sendToChat (unwrapChatId chat) helpMessage
  SendSetup chat -> model <# sendToChat (unwrapChatId chat) helpMessage
  where
    unwrapUserId user = let (UserId id_) = userId user in id_
    unwrapChatId chat = let (ChatId id_) = chatId chat in id_

reminderKeyboard :: [R.Reminder] -> InlineKeyboardMarkup
reminderKeyboard = InlineKeyboardMarkup . map (\r -> [actionButton (reminderButtonMessage r) (DeleteReminder (fromJust $ R.rid r))])

reminderButtonMessage :: R.Reminder -> Text
reminderButtonMessage r = "Remind " +| R.remindeeUserName r |+ " " +| R.reason r |+ ""

pluralF :: (Ord a, Num a) => a -> Builder
pluralF x = if x > 1 then "s" else ""

reminderMessage :: User -> DueDate -> Text -> Text
reminderMessage remindee dd reason = "Will remind " +| userNameF remindee |+ " " +| reason |+ " " +| dueDateF dd
  where
    dueDateF (FixedTime d t) = "on " +| d |+ " at " +| t |+ ""
    dueDateF (NextDay 1 t) = "tomorrow at " +| t |+ ""
    dueDateF (NextDay offset t) = "in " +| offset |+ " day" +| pluralF offset |+ " at " +| t |+ ""
    dueDateF (NextWeekDay dayofweek t) = "next " +|| dayofweek ||+ " at " +| t |+ ""
    dueDateF (NextTime t) = "at " +| t |+ ""
    dueDateF (EveryDay 1 t) = "everyday at " +| t |+ ""
    dueDateF (EveryDay offset t) = "every " +| offset |+ " days at " +| t |+ ""
    dueDateF (EveryWeekDay weekday t) = "every " +|| weekday ||+ " at " +| t |+ ""

userMentionF :: Integer -> Text -> Builder
userMentionF remindeeId remindeeUserName = "<a href=\"tg://user?id=" +| remindeeId |+ "\">" +| remindeeUserName |+ "</a>"

reminderNag :: Integer -> Text -> Text -> Text
reminderNag remindeeId remindeeUserName reason = "Hey " +| userMentionF remindeeId remindeeUserName |+ " I am reminding you " +| reason |+ ""

userReplyMessage :: User -> [User] -> Double -> Text -> Text
userReplyMessage receivable payables amount "" = "Recording that " +| listWithF userNameF payables |+ " owes " +| userNameF receivable |+ " " +| currencyF amount
userReplyMessage receivable payables amount reason = "Recording that " +| listWithF userNameF payables |+ " owes " +| userNameF receivable |+ " " +| currencyF amount +| " for " +| reason |+ ""

settlementMessage :: User -> User -> Double -> Text
settlementMessage payable receivable settlementAmount = userNameF payable |+ " has now settled with " +| userNameF receivable |+ " for a value of " +| currencyF settlementAmount

tallyReplyMessage :: Map ((Integer, Text), (Integer, Text)) Double -> Text
tallyReplyMessage = Map.foldMapWithKey (\((_, receivable), (_, payable)) amount -> debtMessage receivable payable amount Nothing)

debtMessage :: Text -> Text -> Double -> Maybe Text -> Text
debtMessage receivableName payableName amount Nothing = fmtLn $ payableName |+ " owes " +| receivableName |+ " " +| currencyF amount
debtMessage receivableName payableName amount (Just reason) = fmtLn $ payableName |+ " owes " +| receivableName |+ " " +| currencyF amount |+ " for " +| reason |+ ""

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = zonedTimeToLocalTime <$> (getCurrentTime >>= utcToLocalZonedTime)

nagUsers :: Model -> Eff Action Model
nagUsers model = eff nagChats $> model
  where
    nagChats = do
      nowTime <- liftIO $ localTimeOfDay <$> getCurrentLocalTime
      when (todHour nowTime == 12 && todMin nowTime == 0) $ do
        chats <- liftIO $ getUnpaidChatList (dbConnection model)
        mapM_ nagChat chats
    nagChat chatId_ = do
      debtTally <- liftIO $ tallyDebts <$> getChatDebts (dbConnection model) chatId_
      if debtTally == mempty
        then do
          pure ()
        else sendToChat chatId_ (tallyReplyMessage debtTally)

sendToChat :: Integer -> Text -> BotM ()
sendToChat chatId msg = void (runTG $ defSendMessage (SomeChatId . ChatId $ chatId) msg)

sendHTMLToChat :: Integer -> Text -> BotM ()
sendHTMLToChat chatId markup = void (runTG msg)
  where
    msg = let m' = defSendMessage (SomeChatId . ChatId $ chatId) markup in m' {sendMessageParseMode = Just HTML}

remindUsers :: Model -> Eff Action Model
remindUsers model = eff remindUsersInChats $> model
  where
    remindUsersInChats = do
      now <- liftIO getCurrentTime
      reminders <- liftIO $ getRemindersAtOrBefore (dbConnection model) now
      mapM_ remindUser reminders
      liftIO $ R.clearRemindersAtOrBefore (dbConnection model) now
    remindUser r = do
      liftIO $ case (R.rid r, R.period r) of
        (Just rid, Just period) -> R.bumpReminder (dbConnection model) rid (addUTCTime period $ R.nextNag r)
        _ -> pure ()
      sendHTMLToChat (R.chat r) (reminderNag (R.remindee r) (R.remindeeUserName r) (R.reason r))

run :: FlatbotConfig -> IO ()
run config = do
  env <- defaultTelegramClientEnv (token config)
  connection <- open (dbPath config)
  execute_ connection debtTableSchema
  execute_ connection R.reminderTableSchema
  startBot_ (flatbot (Model connection)) env

getConfigVariables :: IO FlatbotConfig
getConfigVariables = do
  token_ <- Token . Text.pack <$> getEnv "TELEGRAM_BOT_TOKEN"
  dbPath_ <- getEnv "FLATBOT_DB_PATH"
  return $ FlatbotConfig token_ dbPath_

main :: IO ()
main = getConfigVariables >>= run
