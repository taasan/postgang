{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Prelude hiding
  ( show
  )

import Control.Applicative
  ( Alternative (..)
  , liftA2
  )
import Control.Monad
  ( replicateM
  )

import Data.Char
  ( chr
  , isDigit
  , isHexDigit
  )
import Data.List
  ( intercalate
  )
import Data.Maybe
  ( catMaybes
  , isNothing
  , mapMaybe
  )
import Data.String
  ( IsString (..)
  )
import Data.Time
  ( UTCTime (utctDay)
  , defaultTimeLocale
  , formatTime
  , getCurrentTime
  , toGregorian
  )

import Numeric
  ( readHex
  )

import System.Environment
  ( getArgs
  )
import System.IO
  ( IOMode (WriteMode)
  , hPutStr
  , withFile
  )
import System.Process
  ( proc
  , readCreateProcess
  )

import qualified Text.ParserCombinators.ReadP as P
import Text.Printf
  ( printf
  )
import Text.Read
  ( readMaybe
  )
import qualified Text.Show as Show

newtype Entry =
  Entry
    { nextDeliveryDays :: [String]
    -- , isStreetAddressReq :: Bool
    }
  deriving (Show)

data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  deriving (Show, Eq, Enum)

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

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)


parseEntry :: JsonValue -> Maybe Entry
parseEntry (JsonObject v) = do
  days <- lookup "nextDeliveryDays" v
  pure $ Entry (extractJsonStrings days)

parseEntry _ = Nothing

-- Generic show
show :: (Show a, IsString b) => a -> b
show = fromString . Show.show

type Parser = P.ReadP

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (P.char '[' *> ws *> elements <* ws <* P.char ']')
 where
  elements :: Parser [JsonValue]
  elements = P.sepBy jsonValue (ws *> P.char ',' <* ws)

jsonValue :: Parser JsonValue
jsonValue =
  jsonObject
    <|> jsonArray
    <|> jsonString
    <|> jsonBool
    <|> jsonNull
    <|> jsonNumber

jsonBool :: Parser JsonValue
jsonBool = JsonBool <$> (jsonTrue <|> jsonFalse)
 where
  jsonTrue  = True ¤ "true"
  jsonFalse = False ¤ "false"

jsonNull :: Parser JsonValue
jsonNull = JsonNull ¤ "null"

-- | Parser for json number values
jsonNumber :: Parser JsonValue
jsonNumber = do
  JsonNumber <$> (P.munch1 isNumberChar >>= f readMaybe)
 where
  isNumberChar c =
    isDigit c || c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E'
  f :: Alternative f => (t -> Maybe a) -> t -> f a
  f g x = case g x of
    Just res -> pure res
    _        -> empty

ws :: P.ReadP ()
ws = P.skipSpaces

jsonObject :: Parser JsonValue
jsonObject =
  JsonObject
    <$> (  P.char '{'
        *> ws
        *> P.sepBy pair (ws *> P.char ',' <* ws)
        <* ws
        <* P.char '}'
        )
  where pair = liftA2 (,) (stringLiteral <* ws <* P.char ':' <* ws) jsonValue

-- | Parser for characters as unicode in input
escapeUnicode :: Parser Char
escapeUnicode =
  chr . fst . head . readHex <$> replicateM 4 (P.satisfy isHexDigit)

-- | Parser for characters that are scaped in the input
escapeChar :: Parser Char
escapeChar =
  ('"' ¤ "\\\"")
    <|> ('\\' ¤ "\\\\")
    <|> ('/' ¤ "\\/")
    <|> ('\b' ¤ "\\b")
    <|> ('\f' ¤ "\\f")
    <|> ('\n' ¤ "\\n")
    <|> ('\r' ¤ "\\r")
    <|> ('\t' ¤ "\\t")
    <|> (P.string "\\u" *> escapeUnicode)

-- | Parser of a character that is not " or \\
normalChar :: Parser Char
normalChar = P.satisfy ((&&) <$> (/= '"') <*> (/= '\\'))

-- | Parser of a string that is between double quotes (not considering any double quots that are scaped)
stringLiteral :: Parser String
stringLiteral = P.char '"' *> P.many (normalChar <|> escapeChar) <* P.char '"'

-- | Parser of literal json string values
jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral


parseDeliveryDay :: Parser DeliveryDay
parseDeliveryDay =
  DeliveryDay <$>- parseWeekDay <*>- (read <$> digits) <*>- parseMonth

digits :: P.ReadP String
digits = P.munch1 isDigit

(<$>-) :: (a -> b) -> Parser a -> Parser b
f <$>- p = f <$> skipUntil p

(<*>-) :: Parser (a -> b) -> Parser a -> Parser b
f <*>- p = f <*> skipUntil p

skipUntil :: Parser a -> Parser a
skipUntil p = p <|> (P.get *> skipUntil p)

parseWeekDay :: Parser DayOfWeek
parseWeekDay = do
  (Monday ¤ "mandag")
    <|> (Tuesday ¤ "tirsdag")
    <|> (Wednesday ¤ "onsdag")
    <|> (Thursday ¤ "torsdag")
    <|> (Friday ¤ "fredag")

parseMonth :: Parser Month
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

(¤) :: a -> String -> Parser a
c ¤ s = c <$ P.string s

parse :: P.ReadP a -> String -> Maybe a
parse parser str = case P.readP_to_S parser str of
  []             -> Nothing
  ((res, _) : _) -> Just res

(<$?>) :: (a -> Maybe b) -> [a] -> [b]
(<$?>) = mapMaybe

infixl 4 <$?>

extractJsonStrings :: JsonValue -> [String]
extractJsonStrings (JsonArray xs) = getJsonString <$?> xs
extractJsonStrings _              = []

getJsonString :: Alternative f => JsonValue -> f String
getJsonString (JsonString x) = pure x
getJsonString _              = empty

main :: IO ()
main = do
  now   <- getCurrentTime
  str   <- fetchData
  entry <- case parse (jsonValue <* P.eof) str >>= parseEntry of
    Just value -> pure value
    _          -> error $ "Invalid data: " <> str
  argv <- getArgs
  let output = ical now entry
  if any isNothing output
    then error $ "Invalid input" <> show entry <> "\n" <> show output
    else do
      let fileName = case argv of
            ("-" : _) -> "/dev/stdout"
            (x   : _) -> x
            _         -> "/dev/stdout"
      withFile fileName WriteMode $ \h -> do
        (hPutStr h . intercalate "\r\n") $ catMaybes output
 where
  ical :: UTCTime -> Entry -> [Maybe String]
  ical now (Entry days) =
    (Just <$> preamble)
      <> (days >>= event now . parse parseDeliveryDay)
      <> (Just <$> ["END:VCALENDAR", ""])
  event :: UTCTime -> Maybe DeliveryDay -> [Maybe String]
  event now (Just dd@(DeliveryDay dayName d m)) =
    let
      year = thisYear + if thisMonth == 12 && m /= December then 1 else 0
      (thisYear, thisMonth, _) = (toGregorian . utctDay) now
    in
      Just
        <$> [ "BEGIN:VEVENT"
            , "UID:" <> show dd
            , "SUMMARY:Posten kommer " <> show dayName
            , printf "DTSTART:%d%02d%02d" year (fromEnum m + 1) d
            , "DURATION:P1D"
            , "DTSTAMP:" <> formatTime defaultTimeLocale "%C%y%m%dT%H%M%S" now
            , "END:VEVENT"
            ]
  event _ _ = []
  preamble :: [String]
  preamble =
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , "PRODID:-//Aasan//Aasan Postgang 1.0//EN"
    , "CALSCALE:GREGORIAN"
    , "METHOD:PUBLISH"
    ]

  fetchData :: IO String
  fetchData = readCreateProcess
    (proc
      "curl"
      [ "-sSL"
      , "https://www.posten.no/levering-av-post-2020/_/component/main/1/leftRegion/1?postCode=7530"
      , "-H"
      , "x-requested-with: XMLHttpRequest"
      ]
    )
    ""
