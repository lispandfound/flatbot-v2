module Bot.UpdateParser where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (Reader, ask, asks, runReader)
import Data.Attoparsec.Text (Parser, parseOnly, string, (<?>))
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
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

newtype ParserError a = ParserError {getError :: Maybe a} deriving (Functor, Eq, Show, Monad, Applicative, Alternative)

instance Semigroup (ParserError a) where
  (<>) = (<|>)

instance Monoid (ParserError a) where
  mempty = ParserError Nothing

type UpdateParser a = ExceptT (ParserError String) (Reader Update) a

-- TODO: All of these can be redone with the mtl monad transformers... perhaps I should do that
runUpdateParser :: UpdateParser a -> Update -> Either (ParserError String) a
runUpdateParser e update = flip runReader update . runExceptT $ e

liftMaybe :: Maybe a -> UpdateParser a
liftMaybe = maybe (throwError mempty) pure

isProbablyHuman :: MessageEntity -> Bool
isProbablyHuman = maybe False (\u -> not (Text.null (userFirstName u) || userIsBot u)) . messageEntityUser

throwParseError :: String -> UpdateParser a
throwParseError = throwError . ParserError . Just

mentions :: UpdateParser [User]
mentions = ask >>= go
  where
    go update = maybe (throwParseError "Expected at least one mentioned user") pure $ do
      msg <- updateMessage update
      ent <- messageEntities msg
      let users = mapMaybe messageEntityUser . filter isProbablyHuman $ ent
      guard $ (not . null) users
      return users

overrideError :: String -> UpdateParser a -> UpdateParser a
overrideError e p = ask >>= (either (const $ throwParseError e) pure . runUpdateParser p)

mention :: UpdateParser User
mention = do
  ms <- mentions
  case ms of
    [m] -> return m
    _ -> throwParseError "Expected exactly one mentioned user"

messageText :: UpdateParser Text
messageText = asks updateMessageText >>= liftMaybe

entities :: UpdateParser [MessageEntity]
entities = asks (updateMessage >=> messageEntities) >>= liftMaybe

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
messageParser p = strippedMessageText >>= either throwParseError pure . parseOnly p

unstrippedMessageParser :: Parser a -> UpdateParser a
unstrippedMessageParser p = messageText >>= either (const $ throwError mempty) pure . parseOnly p

command :: Text -> UpdateParser ()
command t = unstrippedMessageParser ((void . string $ "/" <> t) <?> ("Command " <> Text.unpack t))

chat :: UpdateParser Chat
chat = asks (fmap messageChat . updateMessage) >>= liftMaybe

sender :: UpdateParser User
sender = asks (updateMessage >=> messageFrom) >>= liftMaybe

callbackQuery :: UpdateParser Text
callbackQuery = asks (updateCallbackQuery >=> callbackQueryData) >>= liftMaybe

callbackQueryParser :: Parser a -> UpdateParser a
callbackQueryParser p = callbackQuery >>= either (const $ throwError mempty) pure . parseOnly p
