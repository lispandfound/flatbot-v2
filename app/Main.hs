{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad (guard, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Attoparsec.Text qualified as AP
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Database.SQLite.Simple (Connection, execute_, open)
import Debt
import Debug.Trace
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
  | TallyChat Chat
  | SettleDebts Chat User User
  | SendHelp Chat
  | SendSetup Chat

flatbot :: Model -> BotApp Model Action
flatbot model =
  BotApp
    { botInitialModel = model,
      botAction = updateToAction,
      botHandler = handleAction,
      botJobs = []
    }

data FlatbotConfig = FlatbotConfig
  { token :: Token,
    dbPath :: String
  }

data Command = Owes | Tally | Settle | Help

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = foldr ((<|>) . f) Nothing

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

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ = case (updateChat update, updateCommand update, updateFrom update, updateMentions update, strippedMessageText update) of
  (Just chat, Just Owes, Just receivable, payables, msg) | length payables > 0 -> either (const Nothing) (\amount -> Just $ AddDebt chat receivable payables amount "") (parseAmount msg)
  (Just chat, Just Tally, _, _, _) -> Just $ TallyChat chat
  (Just chat, Just Settle, Just payable, [receivable], _) -> Just $ SettleDebts chat payable receivable
  (Just chat, Just Help, _, _, _) -> Just $ SendHelp chat
  _ | isJust (updateMyChatMember update) -> Just (SendSetup (chatMemberUpdatedChat . fromJust . updateMyChatMember $ update))
  _ -> trace (show update) $ Nothing
  where
    parseAmount = AP.parseOnly debtParser
    spaces = void $ AP.takeTill (/= ' ')
    roundCents :: Double -> Double
    roundCents = (/ 100) . fromIntegral . round . (* 100)
    currency = roundCents <$> ("$" *> AP.double)
    debtParser = do
      spaces
      amount <- currency
      guard $ amount > 0
      spaces
      return amount

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
      "To get the current tally of debts, type:",
      "<pre>/tally</pre>",
      "To settle debts (payable to you, and receivable to you) with Daniel, use:",
      "<pre>/settle @Daniel</pre>",
      "Again the <code>@</code> is important."
    ]

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  AddDebt chat receivable payables amount reason ->
    model
      <# let debts = map (\payable -> createDebt chat receivable payable amount reason) payables
          in do
               liftIO $ mapM_ (insertDebt (dbConnection model)) debts
               pure $ userReplyMessage receivable payables amount reason
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
  SendHelp chat -> model <# (void . trace "Sending help!" $ runTG (helpMessageMarkup chat))
  SendSetup chat -> model <# (void $ runTG (helpMessageMarkup chat))
  where
    helpMessageMarkup chat = let m = defSendMessage (SomeChatId $ chatId chat) helpMessage in m {sendMessageParseMode = Just HTML}
    userReplyMessage receivable payables amount _ =
      "Recording that "
        <> Text.intercalate ", " (map userFirstName payables)
        <> " owes "
        <> userFirstName receivable
        <> " $"
        <> (Text.pack . show $ amount)
    settlementMessage payable receivable settlementAmount = userFirstName payable <> " has now settled with " <> userFirstName receivable <> " for a value of " <> (Text.pack . show) settlementAmount
    tallyReplyMessage = Map.foldMapWithKey (\((_, receivable), (_, payable)) amount -> debtMessage receivable payable amount)
    debtMessage receivableName payableName amount = payableName <> " owes " <> receivableName <> " $" <> (Text.pack . show) amount <> "\n"
    unwrapUserId user = let (UserId id_) = userId user in id_

run :: FlatbotConfig -> IO ()
run config = do
  env <- defaultTelegramClientEnv (token config)
  connection <- open (dbPath config)
  execute_ connection debtTableSchema
  startBot_ (flatbot (Model connection)) env

getConfigVariables :: IO FlatbotConfig
getConfigVariables = do
  token_ <- Token . Text.pack <$> getEnv "TELEGRAM_BOT_TOKEN"
  dbPath_ <- getEnv "FLATBOT_DB_PATH"
  return $ FlatbotConfig token_ dbPath_

main :: IO ()
main = getConfigVariables >>= run
