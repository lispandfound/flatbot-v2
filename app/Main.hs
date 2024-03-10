{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bot.Action
import Bot.Debt qualified as DB
import Bot.Reminder qualified as RB
import Control.Applicative
import Control.Monad (guard, void)
import Data.Attoparsec.Text (takeText)
import Data.Attoparsec.Text qualified as AP
import Data.Debt
import Data.DueDate
import Data.Functor (($>))
import Data.Maybe
import Data.Reminder qualified as R
import Data.Text (Text)
import Data.Text qualified as Text
import Database.SQLite.Simple (Connection, execute_, open)
import System.Environment (getEnv)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (updateMessageText)

data Model = Model
  { dbConnection :: Connection
  }


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

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  AddDebt chat receivable payables amount reason -> model <# DB.addDebt (dbConnection model) chat receivable payables amount reason
  SplitDebt chat receivable payables amount reason -> model <# DB.splitDebt (dbConnection model) chat receivable payables amount reason
  TallyChat chat -> model <# DB.tallyDebt (dbConnection model) chat
  SettleDebts chat payable receivable -> model <# DB.settleDebts (dbConnection model) chat payable receivable
  DebtHistory chat receivable payable -> model <# DB.debtHistory (dbConnection model) chat receivable payable
  AddReminder chat remindee dd reason -> model <# RB.addReminder (dbConnection model) chat remindee dd reason
  PickReminder chat ->
    model <# RB.pickReminder (dbConnection model) chat
  DeleteReminder rid_ ->
    model <# RB.deleteReminder (dbConnection model) rid_
  SendHelp chat -> model <# sendToChat (unwrapChatId chat) helpMessage
  SendSetup chat -> model <# sendToChat (unwrapChatId chat) helpMessage
  where
    unwrapChatId chat = let (ChatId id_) = chatId chat in id_

nagUsers :: Model -> Eff Action Model
nagUsers model = eff (DB.nagChats (dbConnection model)) $> model

sendToChat :: Integer -> Text -> BotM ()
sendToChat chatId msg = void (runTG $ defSendMessage (SomeChatId . ChatId $ chatId) msg)

remindUsers :: Model -> Eff Action Model
remindUsers model = eff (RB.remindUsersInChats (dbConnection model)) $> model

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
