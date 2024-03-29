module Bot.Action where

import Data.DueDate (DueDate (..))
import Data.Text (Text)
import Telegram.Bot.API (Chat, User)

type Amount = Double

data Action
  = AddDebt Chat User [User] (Amount, Text)
  | SplitDebt Chat User [User] (Amount, Text)
  | TallyChat Chat
  | DebtHistory Chat User User
  | SettleDebts Chat User User
  | SendHelp
  | SendDebtHelp
  | SendReminderHelp
  | SendDateHelp
  | SendSetup Chat
  | AddReminder Chat User (DueDate, Text)
  | DeleteReminder Integer
  | PickReminder Chat
  | ReportError Text
  | BumpReminder Integer
  | BumpReminderPrompt Chat
  deriving (Show)
