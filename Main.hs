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
import Data.Attoparsec.Combinator
  ( (<?>)
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
  ( DayOfWeek (..)
  , toGregorian
  )
import Data.Time.Clock
  ( UTCTime (utctDay)
  , getCurrentTime
  )
import Data.Time.Format
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

data DeliveryDay =
  DeliveryDay
    { day :: DayOfWeek
    , dayNum :: Int
    , month :: Month
    }
  deriving (Show)

-- Generic show
show :: (Show a, IsString b) => a -> b
show = fromString . Show.show

parseDeliveryDay :: P.Parser DeliveryDay
parseDeliveryDay =
  DeliveryDay
    <$>- parseWeekDay
    <*>- P.decimal
    <*>- parseMonth
    <?>  "parseDeliveryDay"

(<$>-) :: (a -> b) -> P.Parser a -> P.Parser b
f <$>- p = f <$> skipUntil p

(<*>-) :: P.Parser (a -> b) -> P.Parser a -> P.Parser b
f <*>- p = f <*> skipUntil p

skipUntil :: P.Parser a -> P.Parser a
skipUntil p = P.try p <|> (P.anyChar >> skipUntil p)

parseWeekDay :: P.Parser DayOfWeek
parseWeekDay = do
  (Monday ¤ "mandag")
    <|> (Tuesday ¤ "tirsdag")
    <|> (Wednesday ¤ "onsdag")
    <|> (Thursday ¤ "torsdag")
    <|> (Friday ¤ "fredag")
    <?> "parseWeekDay"

parseMonth :: P.Parser Month
parseMonth =
  (January ¤ "januar")
    <|> (February ¤ "februar")
    <|> (March ¤ "mars")
    <|> (April ¤ "april")
    <|> (May ¤ "mai")
    <|> (June ¤ "juni")
    <|> (July ¤ "juli")
    <|> (August ¤ "august")
    <|> (September ¤ "september")
    <|> (October ¤ "oktober")
    <|> (November ¤ "november")
    <|> (December ¤ "desember")

(¤) :: a -> T.Text -> P.Parser a
c ¤ s = c <$ P.string s

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
  now  <- getCurrentTime

  argv <- getArgs
  let output = ical now entry
  if any isLeft output
    then error $ "Invalid input" <> show entry <> "\n" <> show output
    else do
      let fileName = case argv of
            ("-" : _) -> "/dev/stdout"
            (x   : _) -> x
            _         -> "/dev/stdout"
      withFile fileName WriteMode $ \h -> do
        (T.hPutStr h . T.intercalate "\r\n" . rights) output
 where
  ical :: UTCTime -> Entry -> [Either String T.Text]
  ical now (Entry days _) =
    (Right <$> preamble)
      <> (days >>= event now . P.parseOnly parseDeliveryDay)
      <> (Right <$> ["END:VCALENDAR", ""])
  event :: UTCTime -> Either String DeliveryDay -> [Either String T.Text]
  event now (Right dd@(DeliveryDay dayName d m)) =
    let
      year = thisYear + if thisMonth == 12 && m /= December then 1 else 0
      (thisYear, thisMonth, _) = (toGregorian . utctDay) now
    in
      Right
        <$> [ "BEGIN:VEVENT"
            , "UID:" <> show dd
            , "SUMMARY:Posten kommer " <> show dayName
            , T.pack $ printf "DTSTART:%d%02d%02d" year (fromEnum m + 1) d
            , "DURATION:P1D"
            , T.pack
            $  "DTSTAMP:"
            <> formatTime defaultTimeLocale "%C%y%m%dT%H%M%S" now
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
