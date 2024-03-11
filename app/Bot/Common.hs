-- | Common Bot utility functions

module Bot.Common (htmlEditUpdateMessage, replyHtml, sendToChat) where
import Text.Blaze.Html (Markup)
import Data.Text (Text)
import Text.Blaze.Html.Renderer.Text ( renderHtml )
import Data.Text.Lazy (toStrict)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Control.Monad (void)

htmlEditUpdateMessage :: Markup -> BotM ()
htmlEditUpdateMessage el =
  editUpdateMessage $
    (toEditMessage (toStrict . renderHtml $ el))
      { editMessageParseMode = Just HTML
      }

replyHtml :: Text -> BotM ()
replyHtml markup = reply msg
  where msg = (toReplyMessage markup) {
          replyMessageParseMode = Just HTML
                                    }

sendToChat :: Integer -> Text -> BotM ()
sendToChat chatId msg = void (runTG $ defSendMessage (SomeChatId . ChatId $ chatId) msg)
