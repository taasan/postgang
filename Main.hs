{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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
import Control.Monad.IO.Class
import qualified Data.Attoparsec.Text as P
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable
  ( traverse_
  )
import Data.Maybe
  ( isJust
  )
import Data.Time.Calendar
  ( toGregorian
  )
import Data.Time.Clock
  ( UTCTime (utctDay)
  , getCurrentTime
  )
import Network.HTTP.Req
  ( GET (GET)
  , NoReqBody (NoReqBody)
  , QueryParam (queryParam)
  , defaultHttpConfig
  , header
  , https
  , jsonResponse
  , req
  , responseBody
  , runReq
  , (/:)
  )
import System.Environment
  ( lookupEnv
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

getData :: IO Entry
getData = runReq defaultHttpConfig $ do
  bs <- req
    GET
    (  https "www.posten.no"
    /: "levering-av-post-2020"
    /: "_"
    /: "component"
    /: "main"
    /: "1"
    /: "leftRegion"
    /: "1"
    )
    NoReqBody
    jsonResponse
    options
  liftIO $ pure (responseBody bs)
 where
  options = header "x-requested-with" "XMLHttpRequest"
    <> queryParam "postCode" (Just @Int 7530)

main :: IO ()
main = do
  entry <- getData
  today <- toGregorian . utctDay <$> getCurrentTime
  cgi   <- lookupEnv "GATEWAY_INTERFACE"
  traverse_ T.putStrLn $ if isJust cgi then httpPreamble else []

  traverse_ T.putStrLn $ ical today entry
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
  httpPreamble :: [T.Text]
  httpPreamble = ["Content-Type:text/calendar;charset=utf8", ""]

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
