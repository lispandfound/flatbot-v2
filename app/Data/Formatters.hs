-- | Format specification helpers

module Data.Formatters where

import Data.Text (Text)
import Data.List (intersperse)

import Fmt
import Telegram.Bot.API

currencyF :: Double -> Builder
currencyF d = if df < 0 then "-$" else "$" +| commaizeF (abs df) |+ "." +| (if cents < 10 then padLeftF else padRightF) 2 '0' cents
  where
    df :: Integer
    df = floor d
    cents :: Integer
    cents = round (d * 100) `mod` 100

listWithF :: (a -> Builder) -> [a] -> Builder
listWithF f = mconcat . intersperse ", " . map f

userNameF :: User -> Builder
userNameF user = "" +| userFirstName user |+ ""


pluralF :: (Ord a, Num a) => a -> Builder
pluralF x = if x > 1 then "s" else ""

userMentionF :: Integer -> Text -> Builder
userMentionF remindeeId remindeeUserName = "<a href=\"tg://user?id=" +| remindeeId |+ "\">" +| remindeeUserName |+ "</a>"
