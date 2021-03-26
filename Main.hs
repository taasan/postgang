{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  )
where

import Prelude

import Control.Applicative
  ( Alternative (..)
  , liftA2
  , optional
  )
import Control.Monad
  ( ap
  , join
  , replicateM
  , when
  )

import Data.Char
  ( chr
  , isDigit
  , isHexDigit
  , ord
  )
import Data.List
  ( intercalate
  )
import Data.Maybe
  ( catMaybes
  , fromJust
  , isNothing
  )
import Data.Time
  ( UTCTime (utctDay)
  , defaultTimeLocale
  , formatTime
  , getCurrentTime
  , toGregorian
  )
import Data.Version
  ( showVersion
  )


import Numeric
  ( readHex
  )

import GitHash

import System.Console.GetOpt
  ( ArgDescr (..)
  , ArgOrder (..)
  , OptDescr (..)
  , getOpt
  , usageInfo
  )
import System.Environment
  ( getArgs
  , getProgName
  )
import System.IO
  ( hPutStrLn
  , stderr
  )
import System.Process
  ( readProcessWithExitCode
  )

import Data.Bits
  ( shiftL
  )
import Data.Foldable
  ( traverse_
  )
import Data.Function
  ( (&)
  )
import Data.Functor
  ( void
  )
import Data.Ix
  ( inRange
  )
import Paths_postgang
import System.Exit
  ( ExitCode (..)
  , exitFailure
  , exitSuccess
  , exitWith
  )
import TH
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , choice
  , get
  , munch1
  , readP_to_S
  , satisfy
  , sepBy
  , skipSpaces
  , string
  )
import Text.Printf
  ( PrintfType
  , hPrintf
  , printf
  )

type StringT = String

newtype Entry =
  Entry [StringT]
  deriving (Show)

data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  deriving (Eq, Enum)

instance Show DayOfWeek where
  show Monday    = "mandag"
  show Tuesday   = "tirsdag"
  show Wednesday = "onsdag"
  show Thursday  = "torsdag"
  show Friday    = "fredag"

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

type Parser = ReadP

(¤) :: a -> StringT -> Parser a
c ¤ s = c <$ string s

(<$>-) :: (a -> b) -> Parser a -> Parser b
f <$>- p = f <$> skipUntil p

(<*>-) :: Parser (a -> b) -> Parser a -> Parser b
f <*>- p = f <*> skipUntil p

skipUntil :: Parser a -> Parser a
skipUntil p = p <|> (get *> skipUntil p)

entryP :: Parser Entry
entryP = do
  (void . skipUntil)
    $  string "\"nextDeliveryDays\""
    <* skipSpaces
    <* char ':'
    <* skipSpaces
  Entry <$> (char '[' *> skipSpaces *> elements <* skipSpaces <* char ']')
  where elements = sepBy jsonStringP (skipSpaces *> char ',' <* skipSpaces)

jsonStringP :: Parser StringT
jsonStringP = char '"' *> jString
 where
  jString = do
    optional jsonChar >>= \case
      Nothing                              -> "" <$ char '"'
      Just first | not (isSurrogate first) -> (first :) <$> jString
      Just first                           -> do
        second <- jsonChar
        if isHighSurrogate first && isLowSurrogate second
          then (combineSurrogates first second :) <$> jString
          else empty

  isHighSurrogate = inRange (0xD800, 0xDBFF) . ord
  isLowSurrogate  = inRange (0xDC00, 0xDFFF) . ord
  isSurrogate     = liftA2 (||) isHighSurrogate isLowSurrogate

  combineSurrogates a b =
    chr $ ((ord a - 0xD800) `shiftL` 10) + (ord b - 0xDC00) + 0x10000

  jsonChar =
    ('"' ¤ "\\\"")
      <|> ('\\' ¤ "\\\\")
      <|> ('/' ¤ "\\/")
      <|> ('\b' ¤ "\\b")
      <|> ('\f' ¤ "\\f")
      <|> ('\n' ¤ "\\n")
      <|> ('\r' ¤ "\\r")
      <|> ('\t' ¤ "\\t")
      <|> (string "\\u" *> escapeUnicode)
      <|> satisfy (\c -> not (c == '\"' || c == '\\' || isControl c))
   where
    isControl c = c < '\x20'
    escapeUnicode =
      chr . fst . head . readHex <$> replicateM 4 (satisfy isHexDigit)

deliveryDayP :: Parser DeliveryDay
deliveryDayP = DeliveryDay <$>- weekDayP <*>- (read <$> digitsP) <*>- monthP

digitsP :: Parser StringT
digitsP = munch1 isDigit

weekDayP :: ReadP DayOfWeek
weekDayP = choice $ ap (¤) show <$> [Monday .. Friday]

monthP :: Parser Month
monthP =
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

runParser :: Parser a -> StringT -> Maybe a
runParser parser str = case readP_to_S parser str of
  []             -> Nothing
  ((res, _) : _) -> Just res

f :: Alternative f => (t -> Maybe a) -> t -> f a
f g x = case g x of
  Just res -> pure res
  _        -> empty

-- Options
data Options = Options
    { optOutput :: Maybe FilePath
    , optReadStdin :: Bool
    , optShowHelp :: Bool
    , optShowVersion :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions = Options { optOutput      = Nothing
                         , optReadStdin   = False
                         , optShowHelp    = False
                         , optShowVersion = False
                         }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['o']
           ["output"]
           (ReqArg (\val opts -> opts { optOutput = Just val }) "FILE")
           "Path of output file"
  , Option ['s']
           ["stdin"]
           (NoArg (\opts -> opts { optReadStdin = True }))
           "Read input from stdin instead of fetching from posten.no"
  , Option ['h']
           ["help"]
           (NoArg (\opts -> opts { optShowHelp = True }))
           "Show this help message"
  , Option ['V']
           ["version"]
           (NoArg (\opts -> opts { optShowVersion = True }))
           "Show version information"
  ]

main :: IO ()
main = do
  prog <- getProgName
  (Options outFile readFromStdin showHelp showVersionO, rest) <- opts
  when
    (rest /= [])
    do
      hPrintf stderr "Superfluous arguments: %s" (show rest :: StringT)
      hPutStrLn stderr ""
      hPutStrLn stderr $ showUsage prog
      exitFailure

  when
    showHelp
    do
      showUsage prog
      exitSuccess

  when
    showVersionO
    do
      printf' "Build date" $$tNow
      putStrLn ""
      traverse_ putStrLn gitVersionInfo
      exitSuccess

  (exitCode, response, errResponse) <- fetchData readFromStdin
  case exitCode of
    e@(ExitFailure _) -> do
      hPutStrLn stderr $ printf' "ERROR" "Failed to fetch data"
      traverse_ (hPutStrLn stderr) gitVersionInfo
      hPutStrLn stderr errResponse
      exitWith e
    _ -> pure ()

  outputLines <- liftA2 ical getCurrentTime (f (runParser entryP) response)

  when (any isNothing outputLines)
    $  error
    $  "Invalid input"
    <> "\n"
    <> show outputLines

  let icalOutput = intercalate "\r\n" $ catMaybes outputLines

  if isNothing outFile || outFile == Just "-"
    then putStr icalOutput
    else writeFile (fromJust outFile) icalOutput
 where
  showUsage :: PrintfType r => StringT -> r
  showUsage = printf "Usage: %s" . flip usageInfo options

  ical :: UTCTime -> Entry -> [Maybe StringT]
  ical now (Entry days) =
    (Just <$> preamble)
      <> (days >>= event now . runParser deliveryDayP)
      <> (Just <$> ["END:VCALENDAR", ""])

  event :: UTCTime -> Maybe DeliveryDay -> [Maybe StringT]
  event now (Just dd@(DeliveryDay dayName d m)) =
    let year = thisYear + if thisMonth == 12 && m /= December then 1 else 0
        (thisYear, thisMonth, _) = (toGregorian . utctDay) now
    in  Just
          <$> [ "BEGIN:VEVENT"
              , "UID:" <> show dd
              , printf "SUMMARY:Posten kommer %s %d." (show dayName) d
              , printf "DTSTART:%d%02d%02d" year (fromEnum m + 1) d
              , "DURATION:P1D"
              , "DTSTAMP:" <> formatTime defaultTimeLocale "%C%y%m%dT%H%M%S" now
              , "END:VEVENT"
              ]
  event _ _ = []

  preamble :: [StringT]
  preamble =
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , printf "PRODID:-//Aasan//Aasan Postgang %s//EN" $ giTag gi
    , "CALSCALE:GREGORIAN"
    , "METHOD:PUBLISH"
    ]

  fetchData :: Bool -> IO (ExitCode, StringT, StringT)
  fetchData True = getContents >>= \c -> pure (ExitSuccess, c, "")
  fetchData _    = readProcessWithExitCode
    "curl"
    [ "-sSL"
    , "--retry"
    , "5"
    , "https://www.posten.no/levering-av-post-2020/_/component/main/1/leftRegion/1?postCode=7530"
    , "-H"
    , "x-requested-with: XMLHttpRequest"
    ]
    ""

  opts = do
    argv <- getArgs
    prog <- getProgName
    let header = showUsage prog
    case getOpt RequireOrder options argv of
      (o, n, []) -> return (foldl (&) defaultOptions o, n)
      (_, _, errs) ->
        ioError (userError (join errs <> usageInfo header options))

  gitVersionInfo
    | giDirty gi
    = [ printf' "Branch"
          $  giBranch gi
          <> "@"
          <> giHash gi
          <> " (uncommitted files present)"
      ]
    | otherwise
    = [ printf' "Version" $ showVersion version
      , ""
      , printf' "Tag" $ giTag gi
      , printf' "Branch" $ giBranch gi
      , printf' "Date" $ giCommitDate gi
      , printf' "Message" $ giCommitMessage gi
      ]

  printf' :: PrintfType r => StringT -> r
  printf' = printf "%-12s: %s"

  commit :: StringT
  commit = printf "%s@%s" (giBranch gi) (giHash gi)

  dirty | giDirty gi = " (uncommitted files present)"
        | otherwise  = ""

  gi = $$tGitInfoCwd
