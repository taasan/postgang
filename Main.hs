{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where
import Data.Aeson
  ( FromJSON (parseJSON)
  , Value (Object)
  , decode
  , (.:)
  )
import Data.Monoid
  ( (<>)
  )
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Applicative
  ( empty
  , (<$)
  , (<$>)
  , (<*>)
  , (<|>)
  )
import Control.Monad
  ( join
  )
import qualified Data.Attoparsec.Text as P
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable
  ( traverse_
  )
import Data.Time.Calendar
  ( toGregorian
  )
import Data.Time.Clock
  ( UTCTime (utctDay)
  , getCurrentTime
  )
import Text.Printf
  ( printf
  )

data Entry =
  Entry
    { nextDeliveryDays :: [T.Text]
    , isStreetAddressReq :: Bool
    }
  deriving (Show)

instance FromJSON Entry where
  parseJSON (Object v) =
    Entry <$> v .: "nextDeliveryDays" <*> v .: "isStreetAddressReq"
  parseJSON _ = empty

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show, Eq, Enum)

data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  deriving (Show, Eq, Enum)

data DeliveryDay =
  DeliveryDay
    { day :: DayOfWeek
    , dayNum :: Int
    , month :: Month
    }
  deriving (Show)

parseDeliveryDay :: P.Parser DeliveryDay
parseDeliveryDay =
  DeliveryDay
    <$> parseWeekDay
    <*> (P.skipSpace >> P.decimal)
    <*> (P.char '.' >> P.skipSpace >> parseMonth)

parseWeekDay :: P.Parser DayOfWeek
parseWeekDay =
  (Monday <$ P.string "mandag")
    <|> (Tuesday <$ P.string "tirsdag")
    <|> (Wednesday <$ P.string "onsdag")
    <|> (Thursday <$ P.string "torsdag")
    <|> (Friday <$ P.string "fredag")

parseMonth :: P.Parser Month
parseMonth =
  (January <$ P.string "januar")
    <|> (February <$ P.string "februar")
    <|> (March <$ P.string "mars")
    <|> (April <$ P.string "april")
    <|> (May <$ P.string "mai")
    <|> (June <$ P.string "juni")
    <|> (July <$ P.string "juli")
    <|> (August <$ P.string "august")
    <|> (September <$ P.string "september")
    <|> (October <$ P.string "oktober")
    <|> (November <$ P.string "november")
    <|> (December <$ P.string "desember")

main :: IO ()
main = do
  s     <- BL.getContents
  -- today :: (Integer,Int,Int) -- :: (year,month,day)
  today <- toGregorian . utctDay <$> getCurrentTime
  case decode s :: Maybe Entry of
    Just res -> traverse_ T.putStrLn $ ical today res
    _        -> error $ "No parse: " <> BL.unpack s
 where
  monthNo :: Month -> Int
  monthNo m = fromEnum m + 1
  parse :: T.Text -> DeliveryDay
  parse day = case P.parseOnly parseDeliveryDay day of
    Left  e -> error e
    Right d -> d
  getDays :: [T.Text] -> [DeliveryDay]
  getDays = fmap parse
  ical :: (Integer, Int, Int) -> Entry -> [T.Text]
  ical today (Entry days _) = preamble <> (getDays days >>= event today)
  event :: (Integer, Int, Int) -> DeliveryDay -> [T.Text]
  event (thisYear, thisMonth, thisDay) dd@(DeliveryDay dayName d m) =
    let year =
            if thisMonth == 12 && m /= December then thisYear + 1 else thisYear
    in  [ "BEGIN:VEVENT"
        , T.pack $ printf "UID:%s" $ tshow dd
        , T.pack $ printf "SUMMARY:Posten kommer %s" $ show dayName
        , T.pack $ printf "DTSTART:%d%02d%02d" year (fromEnum m + 1) d
        , "DURATION:P1D"
        , T.pack $ printf "DTSTAMP:%d%02d%02dT000000" thisYear thisMonth thisDay
        , "END:VEVENT"
        ]
  preamble :: [T.Text]
  preamble =
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , "PRODID:-//Aasan//Aasan Postgang 1.0//EN"
    , "CALSCALE:GREGORIAN"
    , "METHOD:PUBLISH"
    ]
  tshow :: Show a => a -> T.Text
  tshow = T.pack . show
