{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where
import Data.Aeson
  ( FromJSON (parseJSON)
  , Value (Object)
  , (.:)
  )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prelude hiding
  ( show
  )

import Control.Applicative
  ( empty
  , (<|>)
  )
import qualified Data.Attoparsec.Text as P
import Data.Either
  ( isLeft
  , rights
  )
import Data.String
  ( IsString (..)
  )
import Data.Time.Calendar
  ( toGregorian
  )
import Data.Time.Clock
  ( UTCTime (utctDay)
  , getCurrentTime
  )
import Network.HTTP.Conduit
  ( Request (method, requestHeaders)
  , parseUrlThrow
  )
import Network.HTTP.Simple
  ( getResponseBody
  , httpJSON
  , setRequestQueryString
  )
import System.Environment
  ( getArgs
  )
import System.IO
  ( IOMode (WriteMode)
  , withFile
  )
import Text.Printf
  ( printf
  )
import qualified Text.Show as Show
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

-- Generic show
show :: (Show a, IsString b) => a -> b
show x = fromString $ Show.show x

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
  initialRequest <-
    parseUrlThrow
      "https://www.posten.no/levering-av-post-2020/_/component/main/1/leftRegion/1"
  let postCode :: Int
      postCode = 7530
      request =
        setRequestQueryString [("postCode", Just (show postCode))]
          $ initialRequest
              { requestHeaders = [("x-requested-with", "XMLHttpRequest")]
              , method         = "GET"
              }

  response <- httpJSON request
  let entry = getResponseBody response
  today <- toGregorian . utctDay <$> getCurrentTime
  argv  <- getArgs
  let output = ical today entry
  if any isLeft output
    then error $ "Invalid input" <> show entry
    else do
      let fileName = case argv of
            ("-" : _) -> "/dev/stdout"
            (x   : _) -> x
            _         -> "/dev/stdout"
      withFile fileName WriteMode $ \h -> do
        (T.hPutStr h . T.intercalate "\r\n" . rights) output
 where
  parse :: T.Text -> Either String DeliveryDay
  parse = P.parseOnly parseDeliveryDay
  getDays :: [T.Text] -> [Either String DeliveryDay]
  getDays = fmap parse
  ical :: (Integer, Int, Int) -> Entry -> [Either String T.Text]
  ical today (Entry days _) =
    (Right <$> preamble)
      <> (getDays days >>= event today)
      <> (Right <$> ["END:VCALENDAR", ""])
  event
    :: (Integer, Int, Int)
    -> Either String DeliveryDay
    -> [Either String T.Text]
  event (thisYear, thisMonth, thisDay) (Right dd@(DeliveryDay dayName d m)) =
    let year =
            if thisMonth == 12 && m /= December then thisYear + 1 else thisYear
    in
      Right
        <$> [ "BEGIN:VEVENT"
            , T.pack $ printf "UID:%s" (show dd :: T.Text)
            , T.pack
              $ printf "SUMMARY:Posten kommer %s" (show dayName :: T.Text)
            , T.pack $ printf "DTSTART:%d%02d%02d" year (fromEnum m + 1) d
            , "DURATION:P1D"
            , T.pack
              $ printf "DTSTAMP:%d%02d%02dT000000" thisYear thisMonth thisDay
            , "END:VEVENT"
            ]
  event _ (Left x) = [Left x]
  preamble :: [T.Text]
  preamble =
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , "PRODID:-//Aasan//Aasan Postgang 1.0//EN"
    , "CALSCALE:GREGORIAN"
    , "METHOD:PUBLISH"
    ]
