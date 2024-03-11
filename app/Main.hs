{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bot.Action
import Bot.Debt qualified as DB
import Bot.Help qualified as HB
import Bot.Reminder qualified as RB
import Bot.UpdateParser (UpdateParser (..), runUpdateParser, ParserError (getError))
import Control.Applicative
import Control.Monad (void)
import Data.Debt
import Data.Functor (($>))
import Data.Reminder qualified as R
import Data.Text (Text)
import Data.Text qualified as Text
import Database.SQLite.Simple (Connection, execute_, open)
import System.Environment (getEnv)
import Telegram.Bot.API
import Telegram.Bot.Simple

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

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ =
  let parsed = runUpdateParser (DB.addDebtCommand
                                <|> DB.splitDebtCommand
                                <|> DB.tallyChatCommand
                                <|> DB.settleChatCommand
                                <|> DB.historyChatCommand
                                <|> HB.helpCommand
                                <|> HB.debtHelpCallback
                                <|> HB.reminderHelpCallback
                                <|> HB.dateHelpCallback
                                <|> RB.addReminderCommand
                                <|> RB.pickReminderCommand
                                <|> RB.deleteReminderCallback
                               ) update
   in either (getError . fmap ReportError) Just parsed

setupMessage :: Text
setupMessage =
  Text.unlines
    ["Hi, resurrected flatbot here! Type /help to get started!"]

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  AddDebt chat receivable payables (amount, reason) -> model <# DB.addDebt (dbConnection model) chat receivable payables amount reason
  SplitDebt chat receivable payables (amount, reason) -> model <# DB.splitDebt (dbConnection model) chat receivable payables amount reason
  TallyChat chat -> model <# DB.tallyDebt (dbConnection model) chat
  SettleDebts chat payable receivable -> model <# DB.settleDebts (dbConnection model) chat payable receivable
  DebtHistory chat receivable payable -> model <# DB.debtHistory (dbConnection model) chat receivable payable
  AddReminder chat remindee (dd, reason) -> model <# RB.addReminder (dbConnection model) chat remindee dd reason
  PickReminder chat ->
    model <# RB.pickReminder (dbConnection model) chat
  DeleteReminder rid_ ->
    model <# RB.deleteReminder (dbConnection model) rid_
  SendDateHelp -> model <# HB.sendDateHelp
  SendReminderHelp -> model <# HB.sendReminderHelp
  SendDebtHelp -> model <# HB.sendDebtHelp
  SendHelp -> model <# HB.sendHelp
  SendSetup chat -> model <# sendToChat (unwrapChatId chat) setupMessage
  ReportError err -> model <# replyText (Text.pack err)
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
