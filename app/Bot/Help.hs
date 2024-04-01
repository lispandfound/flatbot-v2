module Bot.Help where

import Bot.Common
import Bot.Action
import Data.Text qualified as Text
import Telegram.Bot.API
import Telegram.Bot.Simple
import Text.Blaze.Html (Markup)
import Text.Blaze.Html5 qualified as H
import Bot.UpdateParser
import Data.Functor

helpMessage :: EditMessage
helpMessage =
  (toEditMessage "What do you need help with?")
    { editMessageReplyMarkup = Just . SomeInlineKeyboardMarkup . InlineKeyboardMarkup $ buttons
    }
  where
    buttons =
      map
        pure
        [ actionButton "How do I manage debts?" SendDebtHelp,
          actionButton "How do I set reminders?" SendReminderHelp,
          actionButton "I need date formatting help!" SendDateHelp
        ]

nl :: Markup
nl = "\n"

debtHelp :: Markup
debtHelp = do
  H.b "Adding Debts"
  nl
  "To add a debt, use the "
  H.code "/owes"
  " command. To say that Daniel owes me $100 for apples, type: "
  H.pre "/owes @Daniel $100 apples"
  "Typing the "
  H.code "@"
  " is important; it will pop up with a list of people in the chat when you do this."
  nl
  "The general syntax for adding debts is "
  H.code "/owes <person> <amount> <optional reason>"
  nl
  H.b "Splitting Debts"
  nl
  "For bills like power, where you wish to split a bill with a group of people, use "
  H.code "/split"
  ". If Ruby and Izaak both owe you $150 for a power bill, then"
  H.pre "/split @Ruby @Izaak 150 for power bills"
  "splits the power bill three ways between yourself, Ruby and Izaak (so they will owe you $50). The $ before any amount of money is optional for all debt commands."
  nl
  H.b "Tallying Debts"
  nl
  "To tally all the current debts in the chat type:"
  H.pre "/tally"
  "If there are any unpaid debts, the flatbot will remind these users every day at noon."
  nl
  "If you want a detailed list of all debts between yourself and Daniel type:"
  H.pre "/history @Daniel"
  "Don't forget the "
  H.code "@"
  "!"
  nl
  H.b "Settling Debts"
  nl
  "To settle debts (both debts payable from you, and receivable to you) with Daniel, use:"
  H.pre "/settle @Daniel"
  "Again the "
  H.code "@"
  " is important."

reminderHelp :: Markup
reminderHelp = do
  H.b "Setting reminders"
  nl
  "To set a reminder use the "
  H.code "/remind"
  " command. If you needed to remind Daniel to put the bins out every Tuesday evening you could use:"
  H.pre "/remind @Daniel every Tuesday evening to put the bins out"
  "The bot will then ping Daniel every Tuesday at 6pm. The general syntax for adding reminders is "
  H.pre "/remind <person (optional)> <when> <reason>"
  "If you want to remind yourself, just don't mention anyone like so"
  H.pre "/remind every Tuesday evening to put the bins out"
  nl
  "The date format is very flexible, check out \"I need date formatting help!\" for more information on the different ways to format dates."
  nl
  H.b "Deleting reminders"
  nl
  "To delete a reminder, type:"
  H.pre "/unremind"
  "You will then be prompted with a list of reminders. Tap one to delete it.\n"
  H.b "Bumping reminders"
  nl
  "If you have a recurring reminder you want to delay use"
  H.pre "/bump"
  "You could use this if you usually want Daniel to take out the bins, but this week they're being collected on a different day to usual."

dateHelp :: Markup
dateHelp = do
  "You can put dates in almost any format you want. If you want to set a reminder for one specific date, any of the following formats is acceptable"
  nl
  H.pre
    . H.toHtml
    . Text.unlines
    $ [ "- 27/03/2024 or 27-03-2024",
        "- 2024-03-27 or 2024-03-27",
        "- October the 3rd, 2023 or October 3rd 2023, etc..",
        "- 3rd of October, 2023 or 3rd October 2023, etc..",
        "- next Monday, next Tuesday, etc...",
        "- tomorrow, in 5 days, in 1 week, etc"
      ]
  "You can then add a time in the day at the end, such as:"
  H.pre "next Monday at 7pm"
  "times are recognised in 12 hour and 24 hour formats, as well as descriptions like \"morning\", \"noon\", \"afternoon\", \"evening\", etc."
  nl
  "If you want a recurring reminder, here are a few:"
  nl
  H.pre
    . H.toHtml
    . Text.unlines
    $ [ "- every monday at 3pm",
        "- every 2 days at midnight",
        "- weekly at 8pm",
        "- every month at noon"
      ]

helpCommand :: UpdateParser Action
helpCommand = command "help" $> SendHelp

debtHelpCallback :: UpdateParser Action
debtHelpCallback = callbackQueryParser "SendDebtHelp" $> SendDebtHelp

reminderHelpCallback :: UpdateParser Action
reminderHelpCallback = callbackQueryParser "SendReminderHelp" $> SendReminderHelp

dateHelpCallback :: UpdateParser Action
dateHelpCallback = callbackQueryParser "SendDateHelp" $> SendDateHelp

sendDebtHelp :: BotM ()
sendDebtHelp = htmlEditUpdateMessage debtHelp

sendReminderHelp :: BotM ()
sendReminderHelp = htmlEditUpdateMessage reminderHelp

sendDateHelp :: BotM ()
sendDateHelp = htmlEditUpdateMessage dateHelp

sendHelp :: BotM ()
sendHelp = replyOrEdit helpMessage
