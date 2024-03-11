module Bot.UpdateParser where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad (void, (>=>))
import Data.Attoparsec.Text (Parser, parseOnly, string, (<?>), parse)
import Data.Bifunctor
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Control.Monad
import Telegram.Bot.API
  ( CallbackQuery (callbackQueryData),
    Chat,
    Message (messageChat, messageEntities, messageFrom),
    MessageEntity (messageEntityUser),
    Update (updateCallbackQuery, updateMessage),
    User (..),
    messageEntities,
    messageEntityLength,
    messageEntityOffset,
  )
import Telegram.Bot.Simple.UpdateParser (updateMessageText)

type ParserError = Maybe String

newtype UpdateParser a = UpdateParser {runUpdateParser :: Update -> Either ParserError a} deriving (Functor)

instance Applicative UpdateParser where
  pure x = UpdateParser $ (pure . pure) x
  UpdateParser f <*> UpdateParser x = UpdateParser (\u -> f u <*> x u)

instance Alternative UpdateParser where
  empty = UpdateParser (const . Left $ Nothing)
  UpdateParser f <|> UpdateParser g =
    UpdateParser
      ( \u -> case f u of
          Left Nothing -> g u
          r -> r
      )

instance Monad UpdateParser where
  return = pure
  UpdateParser x >>= f = UpdateParser (\u -> x u >>= flip runUpdateParser u . f)

instance MonadFail UpdateParser where
  fail s = UpdateParser $ const (Left . Just $ s)

overrideError :: String -> UpdateParser a -> UpdateParser a
overrideError err parser = UpdateParser (first (const $ Just err) . runUpdateParser parser)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x = maybe (Left x) Right

liftMaybe :: Maybe a -> Either ParserError a
liftMaybe = maybeToEither Nothing

isProbablyHuman :: MessageEntity -> Bool
isProbablyHuman = maybe False (\u -> not (Text.null (userFirstName u) || userIsBot u)) . messageEntityUser


mentions :: UpdateParser [User]
mentions =
  UpdateParser
    ( \update -> maybeToEither (Just "Expecting at least some mentioned users!") $ do
        msg <- updateMessage update
        ent <- messageEntities msg
        let users = mapMaybe messageEntityUser . filter isProbablyHuman $ ent
        guard $ (not . null) users
        return users
    )

mention :: UpdateParser User
mention = do
  ms <- mentions
  case ms of
    [m] -> return m
    _ -> fail "Expected exactly one mentioned user"

messageText :: UpdateParser Text
messageText = UpdateParser (liftMaybe . updateMessageText)

entities :: UpdateParser [MessageEntity]
entities = UpdateParser (liftMaybe . (updateMessage >=> messageEntities))

strippedMessageText :: UpdateParser Text
strippedMessageText = do
  text <- messageText
  ent <- entities
  let cleaned = Text.pack . map snd . filter (\(i, _) -> all (i `outside`) (entityBounds ent)) . zip [0 ..] . Text.unpack $ text
  return cleaned
  where
    entityBounds = map (messageEntityOffset &&& entityEnd)
    outside i (lb, ub) = i < lb || i > ub
    entityEnd ent = messageEntityOffset ent + messageEntityLength ent

messageParser :: Parser a -> UpdateParser a
messageParser p = strippedMessageText >>= UpdateParser . const . first Just . parseOnly p

unstrippedMessageParser :: Parser a -> UpdateParser a
unstrippedMessageParser p = messageText >>= UpdateParser . const . first (const Nothing) . parseOnly p

command :: Text -> UpdateParser ()
command t = unstrippedMessageParser ((void . string $ "/" <> t) <?> ("Command " <> Text.unpack t))

chat :: UpdateParser Chat
chat = UpdateParser (liftMaybe . fmap messageChat . updateMessage)

sender :: UpdateParser User
sender = UpdateParser (liftMaybe . (updateMessage >=> messageFrom))

callbackQuery :: UpdateParser Text
callbackQuery = UpdateParser (liftMaybe . (updateCallbackQuery >=> callbackQueryData))

callbackQueryParser :: Parser a -> UpdateParser a
callbackQueryParser p = callbackQuery >>= UpdateParser . const . first (const Nothing) . parseOnly p
