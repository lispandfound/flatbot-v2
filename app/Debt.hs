-- | Debt models
module Debt (Debt(..), debtTableSchema, makeDebt, insertDebt, getChatDebts, tallyDebts, markDebtsRepaid, tallyDebt, getUnpaidChatList, getDebtsBetween) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Database.SQLite.Simple
import Data.Maybe (fromMaybe)

data Debt = Debt
  { debtChatId :: Integer,
    receivable :: Integer,
    receivableUserName :: Text, -- TODO: these should be looked up through the Telegram APIs
    payable :: Integer, -- The person owing the debt
    payableUserName :: Text,
    amount :: Double,
    reason :: Text
  }
  deriving (Show, Eq)

newtype Amount = Amount { getAmount :: Double }
newtype Id = Id {getId :: Integer}

instance FromRow Id where
  fromRow = Id <$> field

instance FromRow Amount where
  fromRow = Amount <$> field

type User = (Integer, Text)

instance FromRow Debt where
  fromRow = Debt <$> field <*> field <*> field <*> field <*> field <*> field <*> field

makeDebt :: Integer -> Integer -> Text -> Integer -> Text -> Double -> Text -> Debt
makeDebt = Debt

debtTableSchema :: Query
debtTableSchema = "CREATE TABLE IF NOT EXISTS debt (id INTEGER PRIMARY KEY, chat INTEGER NOT NULL, receivable INTEGER NOT NULL, receivableUserName TEXT NOT NULL, payable INTEGER NOT NULL, payableUserName TEXT NOT NULL, amount REAL NOT NULL, reason TEXT, paid BOOLEAN NOT NULL)"

insertDebt :: Connection -> Debt -> IO ()
insertDebt conn (Debt chatId_ receivable_ receivableUserName_ payable_ payableUserName_ amount_ reason_) = execute conn q values
  where
    q = "INSERT INTO debt (chat, receivable, receivableUserName, payable, payableUserName, amount, reason, paid) VALUES (?, ?, ?, ?, ?, ?, ?, FALSE)"
    values = (chatId_, receivable_, receivableUserName_, payable_, payableUserName_, amount_, reason_)

getChatDebts :: Connection -> Integer -> IO [Debt]
getChatDebts conn chatId = queryNamed conn q [":chat" := chatId]
  where
    q = "SELECT chat, receivable, receivableUserName, payable, payableUserName, amount, reason from debt WHERE chat = :chat AND paid = FALSE"

markDebtsRepaid :: Connection -> Integer -> Integer -> Integer -> IO ()
markDebtsRepaid conn chatId payable receivable = execute conn q (chatId, receivable, payable, receivable, payable)
  where
    q = "UPDATE debt SET paid = TRUE WHERE chat = ? AND (receivable = ? AND payable = ?) OR (payable = ? AND receivable = ?)"

tallyDebt :: Connection -> Integer -> Integer -> Integer -> IO Double
tallyDebt conn chatId receivable payable = do
  rp <- sum . map getAmount <$> query conn q (chatId, receivable, payable)
  pr <- sum . map getAmount <$> query conn q (chatId, payable, receivable)
  return $ pr - rp
  where
    q = "SELECT IFNULL(SUM(amount), 0.0) FROM debt WHERE chat = ? AND payable = ? AND receivable = ? AND PAID = FALSE"

tallyDebts :: [Debt] -> Map (User, User) Double
tallyDebts = Map.filter (> 0) . foldr (\debt -> recordRP debt . recordPR debt) mempty
  where
    recordRP debt = Map.insertWith (+) ((receivable debt, receivableUserName debt), (payable debt, payableUserName debt)) (amount debt)
    recordPR debt = Map.insertWith (+) ((payable debt, payableUserName debt), (receivable debt, receivableUserName debt)) (negate $ amount debt)

getUnpaidChatList :: Connection -> IO [Integer]
getUnpaidChatList conn = map getId <$> query_ conn "SELECT DISTINCT chat FROM debt WHERE PAID = FALSE"

getDebtsBetween :: Connection -> Integer -> Integer -> Integer -> IO [Debt]
getDebtsBetween conn chatId receivableId payableId = query conn q (chatId, receivableId, payableId, payableId, receivableId)
  where q = "SELECT chat, receivable, receivableUserName, payable, payableUserName, amount, reason FROM debt WHERE chat = ? AND (receivable = ? AND payable = ? OR receivable = ? AND payable = ?) AND paid = FALSE"
