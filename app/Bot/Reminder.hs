module Bot.Reminder (addReminder, bumpReminderById, bumpReminderCommand, bumpReminderCallback, bumpReminderPrompt, deleteReminderPrompt, deleteReminder, addReminderCommand, pickReminderCommand, deleteReminderCallback, remindUsersInChats) where

import Bot.Action
import Bot.UpdateParser as UP
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class
import Data.Attoparsec.Text (decimal, takeText)
import Data.DueDate
import Data.Formatters
import Data.Maybe (fromJust)
import Data.Reminder (Reminder (..))
import Data.Reminder qualified as R
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock
import Data.Time.LocalTime (LocalTime, ZonedTime (zonedTimeToLocalTime), getCurrentTimeZone, localTimeToUTC, utcToLocalZonedTime)
import Database.SQLite.Simple
import Fmt
import Telegram.Bot.API
import Telegram.Bot.Simple
import Text.Blaze.Html (Markup)
import Text.Blaze.Html5 qualified as H

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = zonedTimeToLocalTime <$> (getCurrentTime >>= utcToLocalZonedTime)

addReminder :: Connection -> Chat -> User -> DueDate -> Text -> BotM Text
addReminder conn chat_ remindee dd reason = do
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
    (ChatId chatId_) = chatId chat_
    (UserId remindeeId) = userId remindee

reminderKeyboard :: (Reminder -> Action) -> [Reminder] -> InlineKeyboardMarkup
reminderKeyboard f = InlineKeyboardMarkup . map (\r -> [actionButton (reminderButtonMessage r) (f r)])

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

pickReminderWith :: (Reminder -> Action) -> [Reminder] -> BotM ()
pickReminderWith f reminderList =
  let msg =
        (toEditMessage "Select a reminder")
          { editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup (reminderKeyboard f reminderList)
          }
   in replyOrEdit msg

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

bumpReminder :: Connection -> Reminder -> BotM ()
bumpReminder conn r = liftIO $ case (R.rid r, R.period r) of
  (Just rid, Just period) -> R.bumpReminder conn rid (addUTCTime period $ R.nextNag r)
  _ -> pure ()

remindUser :: Connection -> Reminder -> BotM ()
remindUser conn r = do
  bumpReminder conn r
  sendHTMLToChat (R.chat r) (reminderNag (R.remindee r) (R.remindeeUserName r) (R.reason r))

reminderErrorMessage :: Markup
reminderErrorMessage = do
  "I could not understand that /remind command you just gave.\n"
  "Try reading the documentation on dates or reminders?\n"
  "The syntax is"
  H.pre "/remind <@person (optional)> <date> <reason>"
  "e.g. /remind @Daniel tuesday evening to take out the bins"

addReminderCommand :: UpdateParser Action
addReminderCommand = command "remind" *> (AddReminder <$> UP.chat <*> (mention <|> sender) <*> overrideError reminderErrorMessage (messageParser reminder))
  where
    reminder = (,) <$> duedate <*> fmap Text.strip takeText

pickReminderCommand :: UpdateParser Action
pickReminderCommand = command "unremind" *> (PickReminder <$> UP.chat)

deleteReminderPrompt :: Connection -> Chat -> BotM ()
deleteReminderPrompt conn chat_ = liftIO (R.getReminders conn chatId_) >>= pickReminderWith (DeleteReminder . fromJust . rid)
  where
    (ChatId chatId_) = chatId chat_

deleteReminderCallback :: UpdateParser Action
deleteReminderCallback = callbackQueryParser ("DeleteReminder " *> (DeleteReminder <$> decimal))

bumpReminderPrompt :: Connection -> Chat -> BotM ()
bumpReminderPrompt conn chat_ = liftIO (R.getPeriodicReminders conn chatId_) >>= pickReminderWith (BumpReminder . fromJust . rid)
  where
    (ChatId chatId_) = chatId chat_

bumpReminderCommand :: UpdateParser Action
bumpReminderCommand = command "bump" *> (BumpReminderPrompt <$> UP.chat)

bumpReminderCallback :: UpdateParser Action
bumpReminderCallback = callbackQueryParser ("BumpReminder " *> (BumpReminder <$> decimal))

bumpReminderById :: Connection -> Integer -> BotM ()
bumpReminderById conn rid_ = liftIO (R.getReminder conn rid_) >>= bumpReminder conn >> editUpdateMessageText bumpMsg
  where
    bumpMsg :: Text
    bumpMsg = "Reminder bumped."
