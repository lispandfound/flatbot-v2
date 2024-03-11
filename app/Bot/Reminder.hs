module Bot.Reminder where

import Bot.Action
import Control.Monad
import Control.Monad.IO.Class
import Data.DueDate
import Data.Formatters
import Data.Maybe
import Data.Reminder (Reminder (..))
import Data.Reminder qualified as R
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.LocalTime (LocalTime, ZonedTime (zonedTimeToLocalTime), getCurrentTimeZone, localTimeToUTC, utcToLocalZonedTime)
import Database.SQLite.Simple
import Fmt
import Telegram.Bot.API
import Telegram.Bot.Simple
import Bot.UpdateParser as UP
import Bot.Action
import Data.Attoparsec.Text (takeText, decimal)


getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = zonedTimeToLocalTime <$> (getCurrentTime >>= utcToLocalZonedTime)

addReminder :: Connection -> Chat -> User -> DueDate -> Text -> BotM Text
addReminder conn chat remindee dd reason = do
  liftIO $ do
    now <- getCurrentLocalTime
    zone <- getCurrentTimeZone
    let nextDate = nextLocalDueDate now dd
        nextDateUTC = localTimeToUTC zone nextDate
        period = recurrencePeriod dd
        reminder = R.mkReminder chatId_ remindeeId (userFirstName remindee) reason nextDateUTC period
    R.insertReminder conn reminder
  return $ reminderMessage remindee dd reason
  where
    (ChatId chatId_) = chatId chat
    (UserId remindeeId) = userId remindee


reminderKeyboard :: [Reminder] -> InlineKeyboardMarkup
reminderKeyboard = InlineKeyboardMarkup . map (\r -> [actionButton (reminderButtonMessage r) (DeleteReminder (fromJust $ rid r))])

reminderButtonMessage :: Reminder -> Text
reminderButtonMessage r = "Remind " +| remindeeUserName r |+ " " +| reason r |+ ""

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

pickReminder :: Connection -> Chat -> BotM ()
pickReminder conn chat = do
  reminderList <- liftIO $ R.getReminders conn chatId_
  let msg =
        (toEditMessage "Select a reminder to delete")
          { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup (reminderKeyboard reminderList)
          }
  replyOrEdit msg
  where
    (ChatId chatId_) = chatId chat

deleteReminder :: Connection -> Integer -> BotM ()
deleteReminder conn rid_ = do
  liftIO $ R.deleteReminder conn rid_
  editUpdateMessageText deleteMsg
  where
    deleteMsg :: Text
    deleteMsg = "Reminder deleted"

reminderNag :: Integer -> Text -> Text -> Text
reminderNag remindeeId remindeeUserName reason = "Hey " +| userMentionF remindeeId remindeeUserName |+ " I am reminding you " +| reason |+ ""

remindUsersInChats :: Connection -> BotM ()
remindUsersInChats conn = do
  now <- liftIO getCurrentTime
  reminders <- liftIO $ R.getRemindersAtOrBefore conn now
  mapM_ (remindUser conn) reminders
  liftIO $ R.clearRemindersAtOrBefore conn now

sendHTMLToChat :: Integer -> Text -> BotM ()
sendHTMLToChat chatId markup = void (runTG msg)
  where
    msg = let m' = defSendMessage (SomeChatId . ChatId $ chatId) markup in m' {sendMessageParseMode = Just HTML}

remindUser :: Connection -> Reminder -> BotM ()
remindUser conn r = do
  liftIO $ case (R.rid r, R.period r) of
    (Just rid, Just period) -> R.bumpReminder conn rid (addUTCTime period $ R.nextNag r)
    _ -> pure ()
  sendHTMLToChat (R.chat r) (reminderNag (R.remindee r) (R.remindeeUserName r) (R.reason r))

reminderErrorMessage :: String
reminderErrorMessage = unlines ["I could understand that /remind command you just gave.",
                                "Try reading the documentation on dates or reminders?",
                                "The syntax is /remind <@person> <date> <reason>",
                                "e.g. /remind @Daniel tuesday evening to take out the bins"]


reminderMentionMessage :: String
reminderMentionMessage = unlines ["You need to mention the person you want to remind, like so:"
                                 , "/remind @Daniel tuesday evening to take out the bins"]

addReminderCommand :: UpdateParser Action
addReminderCommand = command "remind" *> (AddReminder <$> UP.chat <*> overrideError reminderErrorMessage mention <*> overrideError reminderErrorMessage (messageParser reminder))
  where reminder = (,) <$> duedate <*> takeText

pickReminderCommand :: UpdateParser Action
pickReminderCommand = command "unremind" *> (PickReminder <$> UP.chat)

deleteReminderCallback :: UpdateParser Action
deleteReminderCallback = callbackQueryParser ("DeleteReminder " *> (DeleteReminder <$> decimal))
