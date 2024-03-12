module Data.Reminder (Reminder (..), mkReminder, getReminder, getReminders, deleteReminder, clearRemindersAtOrBefore, reminderTableSchema, bumpReminder, insertReminder, getRemindersAtOrBefore, getPeriodicReminders) where

import Data.Text (Text)
import Data.Time
import Data.Time.Format.ISO8601
import Database.SQLite.Simple

data Reminder = Reminder
  { rid :: Maybe Integer,
    chat :: Integer,
    remindee :: Integer,
    remindeeUserName :: Text,
    reason :: Text,
    nextNag :: UTCTime,
    period :: Maybe NominalDiffTime
  }
  deriving (Show, Eq)

mkReminder :: Integer -> Integer -> Text -> Text -> UTCTime -> Maybe NominalDiffTime -> Reminder
mkReminder = Reminder Nothing

instance FromRow Reminder where
  fromRow = Reminder <$> field <*> field <*> field <*> field <*> field <*> field <*> (fmap fromInteger <$> field)

reminderTableSchema :: Query
reminderTableSchema = "CREATE TABLE IF NOT EXISTS reminders (id INTEGER PRIMARY KEY, chat INTEGER NOT NULL, remindee INTEGER NOT NULL, remindeeUserName  TEXT NOT NULL, reason TEXT NOT NULL, nextNag  TEXT NOT NULL, period INT)"

insertReminder :: Connection -> Reminder -> IO ()
insertReminder conn (Reminder _ chat_ reason_ remindee_ remindeeUserName_ nextNag_ period_) = execute conn q values
  where
    q = "INSERT INTO reminders (chat, remindee, remindeeUserName, reason, nextNag, period) VALUES (?, ?, ?, ?, ?, ?)"
    values = (chat_, reason_, remindee_, remindeeUserName_, iso8601Show nextNag_, fmap seconds period_)
    seconds :: NominalDiffTime -> Integer
    seconds = round . (realToFrac :: NominalDiffTime -> Double)

bumpReminder :: Connection -> Integer -> UTCTime -> IO ()
bumpReminder conn rid newTime = execute conn q (iso8601Show newTime, rid)
  where
    q = "UPDATE reminders SET nextNag = ? WHERE id = ?"

clearRemindersAtOrBefore :: Connection -> UTCTime -> IO ()
clearRemindersAtOrBefore conn time = execute conn q (Only $ iso8601Show time)
  where
    q = "DELETE FROM reminders WHERE nextNag <= ?"

getRemindersAtOrBefore :: Connection -> UTCTime -> IO [Reminder]
getRemindersAtOrBefore conn time = query conn q (Only $ iso8601Show time)
  where
    q = "SELECT id, chat, remindee, remindeeUserName, reason, nextNag, period FROM reminders WHERE nextNag <= ?"

deleteReminder :: Connection -> Integer -> IO ()
deleteReminder conn rid = execute conn q (Only rid)
  where
    q = "DELETE FROM reminders WHERE id = ?"

getReminders :: Connection -> Integer -> IO [Reminder]
getReminders conn chat = query conn q (Only chat)
  where
    q = "SELECT id, chat, remindee, remindeeUserName, reason, nextNag, period FROM reminders WHERE chat = ?"

getPeriodicReminders :: Connection -> Integer -> IO [Reminder]
getPeriodicReminders conn chat = query conn q (Only chat)
  where
    q = "SELECT id, chat, remindee, remindeeUserName, reason, nextNag, period FROM reminders WHERE chat = ? AND period IS NOT NULL AND period > 0"

getReminder :: Connection -> Integer -> IO Reminder
getReminder conn rid_ = head <$> query conn q (Only rid_)
    where q = "SELECT id, chat, remindee, remindeeUserName, reason, nextNag, period FROM reminders WHERE id = ?"
