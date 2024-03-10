-- | Due Date parsing library
module DueDate (DueDate(..), nextLocalDueDate, duedate) where

-- import Data.Attoparsec.Text

-- fixedTime :: NominalDiffTime

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text (Parser, parseOnly, space, choice, (<?>), option)
import Data.Attoparsec.Text qualified as A
import Data.Functor
import Data.Text (Text)
import Data.Time

type DayOffset = Int

data DueDate
  = FixedTime Day TimeOfDay
  | NextDay DayOffset TimeOfDay
  | NextWeekDay DayOfWeek TimeOfDay
  | NextTime TimeOfDay
  | EveryDay DayOffset TimeOfDay
  | EveryWeekDay DayOfWeek TimeOfDay
  deriving (Show, Eq)

data AMPM = AM | PM


nextLocalDueDate :: LocalTime -> DueDate -> LocalTime
nextLocalDueDate _ (FixedTime d t) = LocalTime d t
nextLocalDueDate (LocalTime d _) (NextDay offset t) = LocalTime (addDays (fromIntegral offset) d) t
nextLocalDueDate (LocalTime d _) (NextWeekDay weekday t) = LocalTime (addDays diff d) t
  where diff' = dayOfWeekDiff weekday (dayOfWeek d)
        diff :: Integer
        diff = if diff' == 0 then 6 else diff
nextLocalDueDate (LocalTime d t) (NextTime t') = if t > t' then LocalTime (addDays 1 d) t' else LocalTime d t'
nextLocalDueDate ldt (EveryDay offset t') = nextLocalDueDate ldt (NextDay offset t')
nextLocalDueDate ldt (EveryWeekDay weekday t') = nextLocalDueDate ldt (NextWeekDay weekday t')

spaces :: Parser ()
spaces = void $ some space

-- Parser for time of day
timeOfDay :: Parser TimeOfDay
timeOfDay =
  (specialTime <|> hhmmapm <|> hhmm)
    >>= tod
  where
    hhmm = (,) <$> decimal <*> option 0 (":" *> decimal)
    hhmmapm = do
      hour <- decimal
      minute <- option 0 (":" *> decimal)
      guard $ hour <= 12
      void $ many space
      suff <- ampm
      return (resolveAMPM hour suff, minute)
    resolveAMPM 12 AM = 0
    resolveAMPM hour AM = hour
    resolveAMPM 12 PM = 12
    resolveAMPM hour PM = (hour + 12) `mod` 24
    specialTime = "noon" $> (12, 0) <|> "midnight" $> (0, 0) <|> "evening" $> (18, 0) <|> "morning" $> (8, 0) <|> "afternoon" $> (15, 0)
    ampm = "am" $> AM <|> ("pm" $> PM)
    tod (hour, minute) = if hour <= 24 && minute <= 59 then pure $ TimeOfDay hour minute 0 else fail "Invalid hour, minute combination."

ordinal :: Parser Int
ordinal = decimal <* ("th" <|> "rd" <|> "nd" <|> "st")

fixedDate :: Parser Day
fixedDate = choice [parseDDMMYYYY >>= parseTimeFromDay, parseYYYYMMDD >>= parseTimeFromDay, parseDDMonthYYYY >>= parseTimeFromDay, parseMonthDDYYYY >>= parseTimeFromDay]
  where
    parseDDMMYYYY = do
      day <- decimal
      void "/" <|> void "-"
      month <- decimal
      void "/" <|> void "-"
      year <- fromIntegral <$> decimal
      return (year, month, day)
    parseYYYYMMDD = do
      year <- fromIntegral <$> decimal
      void "/" <|> void "-"
      month <- decimal
      void "/" <|> void "-"
      day <- decimal
      return (year, month, day)
    parseDDMonthYYYY = do
      day <- ordinal <|> decimal
      void " of " <|> spaces
      month <- namedMonth
      void $ optional ","
      spaces
      year <- fromIntegral <$> decimal
      return (year, month, day)

    parseMonthDDYYYY = do
      month <- namedMonth
      void " the " <|> spaces
      day <- ordinal <|> decimal
      void $ optional ","
      spaces
      year <- fromIntegral <$> decimal
      return (year, month, day)
    namedMonth = choice $ zipWith ($>) ["january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"] [1 ..]
    parseTimeFromDay (year, month, day) = maybe (fail "Invalid year, month, day combination") pure $ fromGregorianValid year month day

duedate :: Parser DueDate
duedate = everyWeekDay <|> everyTime <|> fixedTime <|> nextWeekDay <|> nextDay <|> nextTime
  where
        fixedTime = choice [dateTime, timeDate]
        dateTime = do
                day <- fixedDate <?> "day"
                void " at " <|> spaces
                time <- timeOfDay <?> "time"
                return $ FixedTime day time
        timeDate = do
          time <- timeOfDay <?> "time"
          void ", " <|> void " on " <|> void " of " <|> spaces
          day <- fixedDate <?> "day"
          return $ FixedTime day time
        noon = TimeOfDay 12 0 0
        everyTime = do
          p <- period
          void $ optional " at "
          EveryDay p <$> option noon timeOfDay
        period = choice [daily, weekly, monthly, everyPeriod]
          where
                daily = ("everyday" <|> "daily") $> 1
                weekly = "weekly" $> 7
                monthly = "monthly" $> 30
                everyPeriod = "every " *> do
                        mult <- option 1 (decimal <* spaces)
                        p <- "day" $> 1 <|> "week" $> 7 <|> "month" $> 30 <|> "year" $> 365
                        void $ optional "s"
                        return $ p * mult
        nextWeekDay = do
          void $ optional "next "
          day <- weekday
          void " at " <|> void (many space)
          time <- option noon timeOfDay
          return $ NextWeekDay day time
        everyWeekDay = do
          void "every "
          day <- weekday
          void " at " <|> void (many space)
          time <- option noon timeOfDay
          return $ EveryWeekDay day time
        nextTime = NextTime <$> timeOfDay
        weekday = "monday" $> Monday <|> "tuesday" $> Tuesday <|> "wednesday" $> Wednesday <|> "thursday" $> Thursday <|> "friday" $> Friday <|> "saturday" $> Saturday <|> "sunday" $> Sunday
        nextDay = do
          offset <- dayOffset
          spaces
          void $ optional ("at" *> spaces)
          t <- timeOfDay
          return $ NextDay offset t
        dayOffset = "tomorrow" $> 1 <|> inDays <|> inWeeks
        inDays = do
          void "in "
          d <- decimal
          void " day"
          void $ optional "s"
          return d
        inWeeks = do
          void "in "
          d <- decimal
          void " weeks"
          void $ optional "s"
          return $ 7 * d


-- Helper function for parsing integers
decimal :: Parser Int
decimal = decimal' >>= checkValid
  where
    decimal' = A.decimal
    checkValid x
      | x >= 0 = pure x
      | otherwise = fail "Expect positive integer"
