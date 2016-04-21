{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data.WeatherBalloon.Types where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Bifunctor
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as C
import           Data.Char
import           Data.Csv
import           Data.Map.Strict       (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Ratio
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.Vector           as V
import           System.Locale
import           Test.QuickCheck

data DistanceUnit = Kilometres | Miles | Metres
    deriving (Eq, Show, Read)

data TemperatureUnit = Celsius | Fahrenheit | Kelvin
    deriving (Eq, Show, Read)

newtype Location = Location { unLocation :: (Double, Double) }
    deriving (Eq, Ord, Show)

newtype Observatory = Observatory { unObservatory :: Text }
    deriving (Eq, Ord, Show)

fromDoubles :: Double -> Double -> Location
fromDoubles x y = Location (x,y)

newtype Temperature = Temperature { unTemperature :: Double }
    deriving (Eq, Ord)

instance Show Temperature where
    show (Temperature t) = show t

-- | One line in the balloon's log. We use doubles here because that's
--   we'll use for the processing phase, but they should be truncated to
--   nats before they're output.
data LogEntry = LogEntry {
    _entryTimestamp   :: !UTCTime
  , _entryLocation    :: !Location
  , _entryTemperature :: !Temperature
  , _entryObservatory :: !Observatory
}

makeLenses ''LogEntry

-- | Keeps state required to generate a report on the processed data.
data ReportState = ReportState {
    _reportCount         :: !Integer -- ^ Keep the record count from the first pass so we can calculate the mean.
  , _reportMean          :: !(Maybe Temperature)  -- ^ Running total of mean temperature.
  , _reportMin           :: !(Maybe Temperature)  -- ^ Running total of minimum temperature.
  , _reportMax           :: !(Maybe Temperature)  -- ^ Running total of maximum temperature.
  , _reportDistance      :: !Double  -- ^ Running total of (Euclidian) distance travelled.
  , _reportLastLocation  :: !(Maybe Location) -- ^ Location of the latest observation.
  , _reportObservatories :: !(Map Observatory Integer)
} deriving (Show)

makeLenses ''ReportState

--
-- CSV-parsing instances.
--

instance FromRecord LogEntry where
    parseRecord v
        | V.length v /= 4 = fail "Wrong number of fields."
        | otherwise       = LogEntry <$> (v .! 0)
                                     <*> (v .! 1)
                                     <*> (v .! 2)
                                     <*> (v .! 3)

instance FromField Observatory where
    parseField inp = do
        val <- parseField inp
        if T.null val
            then fail "Observatory field empty."
            else pure $ Observatory val

instance FromField UTCTime where
    parseField inp = case parseTime defaultTimeLocale timestampFormat (C.unpack inp) of
        Just t -> pure t
        Nothing -> fail "Invalid timestamp."

instance FromField Temperature where
    parseField inp = Temperature <$> parseField inp

instance FromField Location where
    parseField inp = let parts = S.split comma inp in
        case length parts of
            2 -> fromDoubles <$> parseField (parts !! 0) <*> parseField (parts !! 1)
            x -> fail "Invalid location."
      where
        comma = fromIntegral $ ord ','

--
-- CSV-writing instances. We round the Doubles in the LogEntry to nats
-- for output here.
--

instance ToRecord LogEntry where
    toRecord e = record [
        toField (e ^. entryTimestamp)
      , toField (bimap toInt toInt (unLocation (e ^. entryLocation)))
      , toField (toInt (unTemperature (e ^. entryTemperature)))
      , toField (e ^. entryObservatory)
     ]
       where
         toInt :: Double -> Int
         toInt = round

instance ToField Observatory where
    toField (Observatory o) = toField o

instance ToField Temperature where
    toField (Temperature t) = toField t

instance (ToField a, ToField b) => ToField (a, b) where
    toField (x, y) = toField x <> "," <> toField y

instance ToField UTCTime where
    toField t = C.pack $ formatTime defaultTimeLocale timestampFormat t

timestampFormat :: String
timestampFormat = "%Y-%m-%dT%H:%M"

--
-- Arbitrary instances.
--

instance Arbitrary LogEntry where
    arbitrary = liftM4 LogEntry arbitrary
                                arbitrary
                                arbitrary
                                arbitrary

-- Generate a random date within around ten years of 2015-04-25 in
-- either direction.
instance Arbitrary UTCTime where
    arbitrary = liftM (fromJust . parseTime defaultTimeLocale "%s" . show)
                      (choose (1114425009 :: Integer, 1745145013 :: Integer))

-- This will sometimes generate empty strings (feature, not bug).
instance Arbitrary Observatory where
    arbitrary = liftM (Observatory . T.pack . filterInvalid) arbitrary
      where
        filterInvalid :: String -> String
        filterInvalid = filter (\c -> c <= 'Z' && c >= 'A')

instance Arbitrary Temperature where
    arbitrary = liftM Temperature arbitrary

-- Locations are specified as nats, so use a custom generator to reflect this.
instance Arbitrary Location where
    arbitrary = liftM2 fromDoubles genPositiveReal genPositiveReal

genPositiveReal :: Gen Double
genPositiveReal = do
    a <- choose (1, precision)
    b <- choose (a, precision)
    return (fromRational (b % a))
  where
    precision = 9999999999

-- | CLI options for weatherballoon-report.
data WeatherOpts = WeatherOpts
    { optReportMin            :: Bool
    , optReportMax            :: Bool
    , optReportMean           :: Bool
    , optReportDistance       :: Bool
    , optReportObservatories  :: Bool
    , optNormalizeTemperature :: TemperatureUnit
    , optNormalizeDistance    :: DistanceUnit
    , optOutputPath           :: FilePath
    } deriving Show
