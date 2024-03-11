module Bot.Debt where

import Control.Monad
import Control.Monad.IO.Class
import Data.Debt (Debt (..))
import Data.Debt qualified as Debt
import Data.Formatters
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.LocalTime
import Database.SQLite.Simple
import Fmt
import Telegram.Bot.API
import Telegram.Bot.Simple
import Bot.UpdateParser
import Bot.Action
import Data.Attoparsec.Text
import Control.Applicative

spaces :: Parser ()
spaces = void $ takeTill (/= ' ')

currency :: Parser Double
currency = do
  void . optional $ "$"
  wholePart <- decimal :: Parser Integer
  guard $ wholePart > 0
  (lead, centPart) <- option (mempty, 0) ("." *> ((,) <$> many (char '0') <*> decimal)) :: Parser ([Char], Integer)
  guard $ length lead <= 2 && centPart >= 0 && centPart < 100
  return $ fromIntegral wholePart + fromInteger centPart / (10 ^ ind)
  where ind :: Integer
        ind = 2


debt :: UpdateParser (Double, Text)
debt = messageParser $ do
  spaces
  amount <- currency
  guard (amount > 0)
  reason <- option "" $ " " *> spaces *> takeText
  return (amount, reason)


debtCommandError :: String -> String
debtCommandError cmd = unlines [ "I couldn't understand the " ++ cmd ++ " command you just gave.",
                             "Try reading the help documentation on debts? Type /help to find it.",
                             "You should try something like",
                             cmd ++ " @Daniel $30 power"
                           ]

debtIncorrectMentionsCountError :: String -> String
debtIncorrectMentionsCountError cmd = unlines [
  "You have to mention at least one person who owes you a debt, like so: ",
  cmd ++ " @Daniel $30 power",
  "Maybe try reading the documentation on debts? Type /help to find it."
                                              ]

addDebtCommand :: UpdateParser Action
addDebtCommand = command "owes" *> (AddDebt <$> chat <*> sender <*> overrideError (debtIncorrectMentionsCountError "/owes") mentions <*> overrideError (debtCommandError "/owes") debt)

splitDebtCommand :: UpdateParser Action
splitDebtCommand = command "split" *> (SplitDebt <$> chat <*> sender <*> overrideError (debtIncorrectMentionsCountError "/split") mentions <*> overrideError (debtCommandError "/split") debt)

tallyChatCommand :: UpdateParser Action
tallyChatCommand = command "tally" *> (TallyChat <$> chat)

historyErrorMessage :: String
historyErrorMessage = unlines [
  "You have to mention the person you want to compare your debts with, like so:",
  "/history @Daniel"
                             ]

historyChatCommand :: UpdateParser Action
historyChatCommand = command "history" *> (DebtHistory <$> chat <*> sender <*> overrideError historyErrorMessage mention)

settleErrorMessage :: String
settleErrorMessage = unlines [
  "You have to mention the person you're settling your debts with, like so:",
  "/settle @Daniel"
                             ]
settleChatCommand :: UpdateParser Action
settleChatCommand = command "settle" *> (SettleDebts <$> chat <*> sender <*> overrideError settleErrorMessage mention)


roundCents :: Double -> Double
roundCents = (/ 100) . fromInteger . round . (* 100)

addDebt :: Connection -> Chat -> User -> [User] -> Double -> Text -> BotM Text
addDebt conn chat receivable payables amount reason = do
  liftIO $ mapM_ (Debt.insertDebt conn) debts
  pure $ userReplyMessage receivable payables amount reason
  where
    debts = map (\payable -> createDebt chat receivable payable amount reason) payables

splitDebt :: Connection -> Chat -> User -> [User] -> Double -> Text -> BotM Text
splitDebt conn chat receivable payables amount reason = do
  liftIO $
    mapM_
      (Debt.insertDebt conn)
      debts
  pure $
    userReplyMessage receivable payables splitAmount reason
  where
    splitAmount = roundCents $ amount / fromIntegral (length payables + 1)
    debts = map (\payable -> createDebt chat receivable payable splitAmount reason) payables

tallyDebt :: Connection -> Chat -> BotM Text
tallyDebt conn chat = do
  debts <- liftIO $ Debt.getChatDebts conn chatId_
  let tallied = Debt.tallyDebts debts
  if tallied == mempty then pure "No debts are currently owed anywhere!" else pure $ tallyReplyMessage tallied
  where
    ChatId chatId_ = chatId chat

settleDebts :: Connection -> Chat -> User -> User -> BotM Text
settleDebts conn chat payable receivable = do
  settlementAmount <- liftIO $ Debt.tallyDebt conn chatId_ receivableId payableId
  liftIO $ Debt.markDebtsRepaid conn chatId_ payableId receivableId
  pure $ settlementMessage payable receivable (abs settlementAmount)
  where
    ChatId chatId_ = chatId chat
    UserId payableId = userId payable
    UserId receivableId = userId receivable

debtHistory :: Connection -> Chat -> User -> User -> BotM Text
debtHistory conn chat receivable payable = do
  debts <- liftIO $ Debt.getDebtsBetween conn chatId_ receivableId payableId
  if null debts
    then pure $ userNameF receivable |+ " and " +| userNameF payable |+ " have no outstanding debts."
    else pure . foldMap (\d -> debtMessage (receivableUserName d) (payableUserName d) (amount d) (if reason d /= "" then Just (reason d) else Nothing)) $ debts
  where
    ChatId chatId_ = chatId chat
    UserId payableId = userId payable
    UserId receivableId = userId receivable

createDebt :: Chat -> User -> User -> Double -> Text -> Debt
createDebt chat receivable payable amount reason = Debt.makeDebt chatId_ receivableId (userFirstName receivable) payableId (userFirstName payable) amount reason
  where
    ChatId chatId_ = chatId chat
    UserId receivableId = userId receivable
    UserId payableId = userId payable

userReplyMessage :: User -> [User] -> Double -> Text -> Text
userReplyMessage receivable payables amount "" = "Recording that " +| listWithF userNameF payables |+ " owes " +| userNameF receivable |+ " " +| currencyF amount
userReplyMessage receivable payables amount reason = "Recording that " +| listWithF userNameF payables |+ " owes " +| userNameF receivable |+ " " +| currencyF amount +| " for " +| reason |+ ""

debtMessage :: Text -> Text -> Double -> Maybe Text -> Text
debtMessage receivableName payableName amount Nothing = fmtLn $ payableName |+ " owes " +| receivableName |+ " " +| currencyF amount
debtMessage receivableName payableName amount (Just reason) = fmtLn $ payableName |+ " owes " +| receivableName |+ " " +| currencyF amount |+ " for " +| reason |+ ""

tallyReplyMessage :: Map ((Integer, Text), (Integer, Text)) Double -> Text
tallyReplyMessage = Map.foldMapWithKey (\((_, receivable), (_, payable)) amount -> debtMessage receivable payable amount Nothing)

settlementMessage :: User -> User -> Double -> Text
settlementMessage payable receivable settlementAmount = userNameF payable |+ " has now settled with " +| userNameF receivable |+ " for a value of " +| currencyF settlementAmount

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = zonedTimeToLocalTime <$> (getCurrentTime >>= utcToLocalZonedTime)

sendToChat :: Integer -> Text -> BotM ()
sendToChat chatId msg = void (runTG $ defSendMessage (SomeChatId . ChatId $ chatId) msg)

nagChats :: Connection -> BotM ()
nagChats conn = do
  nowTime <- liftIO $ localTimeOfDay <$> getCurrentLocalTime
  when (todHour nowTime == 12 && todMin nowTime == 0) $ do
    chats <- liftIO $ Debt.getUnpaidChatList conn
    mapM_ (nagChat conn) chats

nagChat :: Connection -> Integer -> BotM ()
nagChat conn chatId_ = do
  debtTally <- liftIO $ Debt.tallyDebts <$> Debt.getChatDebts conn chatId_
  if debtTally == mempty then pure () else sendToChat chatId_ (tallyReplyMessage debtTally)
