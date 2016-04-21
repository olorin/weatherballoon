{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Data.WeatherBalloon.Util where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Data.Bifunctor
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as S
import           Data.Char
import           Data.Csv
import           Data.Csv.Parser
import           Data.List
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Word
import           Pipes
import qualified Pipes.ByteString          as PB
import qualified Pipes.Csv                 as PC
import qualified Pipes.Prelude             as P
import           System.Directory
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Process

import           Data.WeatherBalloon.Types

-- | Given an observatory, returns the distance unit that observatory uses.
--
--   >>> distanceUnit (Observatory (T.pack "FR"))
--   Metres
--
--   >>> distanceUnit (Observatory (T.pack "foo"))
--   Kilometres
distanceUnit :: Observatory -> DistanceUnit
distanceUnit (Observatory "AU") = Kilometres
distanceUnit (Observatory "US") = Miles
distanceUnit (Observatory "FR") = Metres
distanceUnit _                  = Kilometres

-- | Given an observatory, returns the temperature unit that observatory uses.
--
--   >>> temperatureUnit (Observatory (T.pack "US"))
--   Fahrenheit
--
--   >>> temperatureUnit (Observatory (T.pack "foo"))
--   Kelvin
temperatureUnit :: Observatory -> TemperatureUnit
temperatureUnit (Observatory "AU") = Celsius
temperatureUnit (Observatory "US") = Fahrenheit
temperatureUnit (Observatory "FR") = Kelvin
temperatureUnit _                  = Kelvin

-- | Takes a temperature unit and value and returns the value in degrees
--   Kelvin.
--
--   >>> normalizeTemperature Fahrenheit (Temperature 100)
--   310.9277777777778
normalizeTemperature :: TemperatureUnit -> Temperature -> Temperature
normalizeTemperature Celsius (Temperature !t)    = Temperature $ 273.15 + t
normalizeTemperature Fahrenheit (Temperature !t) = Temperature $ (t + 459.67) * (5.0/9.0)
normalizeTemperature Kelvin     !t               = t

-- | Takes a temperature unit and normalized value (in degrees Kelvin)
--   and returns the value converted to the provided unit.
--
--   >>> deNormalizeTemperature Fahrenheit (Temperature 310.9277777777778)
--   100.00000000000006
--
--   >>> deNormalizeTemperature Fahrenheit (Temperature 256)
--   1.1299999999999955
deNormalizeTemperature :: TemperatureUnit -> Temperature -> Temperature
deNormalizeTemperature Celsius    (Temperature !t) = Temperature $ t - 273.15
deNormalizeTemperature Fahrenheit (Temperature !t) = Temperature $ (t * (9.0/5.0)) - 459.67
deNormalizeTemperature Kelvin     !t               = t

-- | Takes a distance unit and value and returns the value in metres.
--
--   >>> normalizeLocation Miles (Location (10.0, 20.0))
--   Location {unLocation = (16093.44,32186.88)}
normalizeLocation :: DistanceUnit -> Location -> Location
normalizeLocation Kilometres (Location !l) = Location $ monomap (1000.0 *) l
normalizeLocation Miles      (Location !l) = Location $ monomap (1609.344 *) l
normalizeLocation Metres     !l            = l

-- | Takes a distance unit and normalized location (in metres) and returns
--   the value converted to the provided unit.
--
--   >>> deNormalizeLocation Miles (Location (16093.44,32186.88))
--   Location {unLocation = (10.0,20.0)}
deNormalizeLocation :: DistanceUnit -> Location -> Location
deNormalizeLocation u (Location (!x,!y)) = Location $ monomap (deNormalizeDistance u) (x,y)

-- | Takes a distance unit and normalized distance (in metres) and returns
--   the value converted to the provided unit.
--
--   >>> deNormalizeDistance Kilometres 300
--   0.3
deNormalizeDistance :: DistanceUnit -> Double -> Double
deNormalizeDistance Kilometres !x = x / 1000.0
deNormalizeDistance Miles !x = x / 1609.344
deNormalizeDistance Metres !x = x

-- | Map a single function over both parts of a bifunctor.
monomap :: Bifunctor p => (a -> b) -> p a a -> p b b
monomap f = bimap f f

-- | Convert all the distance and temperature values in a LogEntry to
--   metres/Kelvin.
normalizeEntry :: LogEntry -> LogEntry
normalizeEntry e =
    let distUnit = distanceUnit $ e ^. entryObservatory
        tempUnit = temperatureUnit $ e ^. entryObservatory in
    entryLocation %~ normalizeLocation distUnit $ (entryTemperature %~ normalizeTemperature tempUnit) e

-- | Given units for temperature and distance, convert all values in the
--   provided (normalized) LogEntry to those units.
deNormalizeEntry :: TemperatureUnit -> DistanceUnit -> LogEntry -> LogEntry
deNormalizeEntry t d e =
    entryLocation %~ deNormalizeLocation d $! (entryTemperature %~ deNormalizeTemperature t) e

weatherDelimiter :: Word8
weatherDelimiter = fromIntegral (ord '|')

weatherDecodeOpts :: DecodeOptions
weatherDecodeOpts =
    defaultDecodeOptions {
        decDelimiter = weatherDelimiter
    }

weatherEncodeOpts :: EncodeOptions
weatherEncodeOpts =
    defaultEncodeOptions {
        encDelimiter = weatherDelimiter
    }

-- | Pipe to encode a LogEntry to a '|'-delimited record. The stripping
--   step at the end is horrible, but necessary while cassava forces
--   escaping all commas - if I get time this weekend I'll implement the
--   changes suggested here[0].
--
--   [0]: https://github.com/tibbe/cassava/issues/80
encodeLogEntries :: Monad m => Pipe LogEntry ByteString m r
encodeLogEntries = PC.encodeWith weatherEncodeOpts >-> P.map stripQuotes
  where
    stripQuotes = S.filter (fromIntegral (ord '"') /=)

-- | Decode LogEntries from a ByteString producer.
decodeLogEntries :: Monad m => Producer ByteString m () -> Producer (Either String LogEntry) m ()
decodeLogEntries = PC.decodeWith weatherDecodeOpts NoHeader

-- | Given a FilePath with LogEntries, writes sorted output to a
--   temporary file and returns its path. Ideally I'd do this in Haskell
--   for robustness and portability reasons, but writing a decently fast
--   constant-memory sort is probably not worth the additional complexity in
--   this case.
--
--   Default options are fine for GNU sort as the timestamp is the first
--   field and the dates are most-significant-component-first.
sortEntries :: FilePath -> IO FilePath
sortEntries f = do
    (fp, fh) <- openTempFile "." ".tmp_sorted.txt"
    ph <- runProcess "/usr/bin/sort" [f] Nothing Nothing Nothing (Just fh) Nothing
    exit <- waitForProcess ph
    hClose fh
    case exit of
        ExitSuccess -> return fp
        x -> fail "Bailing out: sort returned nonzero exit code."

-- | Empty object for the report state accumulator.
initialReportState :: ReportState
initialReportState =
    ReportState 0       -- report count
                Nothing -- mean temperature
                Nothing -- min temperature
                Nothing -- max temperature
                0.0     -- total distance
                Nothing -- Last observed location.
                M.empty -- observatory sightings

-- | Update the running total with the distance the balloon has travelled between two
--   observations. The distance is assumed to be Euclidian (i.e.,
--   a single straight-line path), and so will underestimate distance
--   travelled if the balloon takes a more convoluted path between two
--   observations.
updateDistance :: Maybe Location -> Location -> Double -> Double
updateDistance !last !current !acc = case last of
    Nothing -> 0.0 -- If this is the first report, we can't meaningfully report a positive distance travelled.
    Just last' -> acc + observationDistance last' current
  where
    observationDistance (Location (x,y)) (Location (w,z)) = sqrt $! (w-x)^2 + (z-y)^2

-- | Given the previous minimum temperature and the current observed
--   temperature, return the new minimum temperature.
updateMin :: Temperature -> Maybe Temperature -> Maybe Temperature
updateMin (Temperature !current) !acc = case acc of
    Nothing -> Just $ Temperature  current
    Just (Temperature acc') -> Just . Temperature $! if current < acc' then current else acc'

-- | Given the previous maximum temperature and the current observed
--   temperature, return the new maximum temperature.
updateMax :: Temperature -> Maybe Temperature -> Maybe Temperature
updateMax (Temperature !current) !acc = case acc of
    Nothing -> Just $! Temperature current
    Just (Temperature acc') -> Just . Temperature $! if current > acc' then current else acc'

-- | Update the mean temperature given the total number of observations,
--   the current temperature observation and the accumulator. Sum of
--   quotients rather than quotient of sum to avoid excessive floating-point
--   error.
updateMean :: Integer -> Temperature -> Maybe Temperature -> Maybe Temperature
updateMean !count (Temperature !current) !acc = let delta = current / (fromRational . toRational) count in
    case acc of
        Nothing -> Just $ Temperature delta
        Just (Temperature acc') -> Just . Temperature $! acc' + delta

-- | Update the number of times we've seen a given observatory.
updateObservatories :: Observatory -> Map Observatory Integer -> Map Observatory Integer
updateObservatories = M.alter insertOrIncrement
  where
    insertOrIncrement Nothing  = Just 1
    insertOrIncrement (Just x) = Just $ x + 1

-- | Apply a log entry to the report accumulator. Assumes the timestamp
--   of the provided LogEntry is later than any we've accumulated
--   previously, that the value in _reportCount has been computed,
--   and that the units have been normalized to metres/Kelvin.
updateReportState :: ReportState -> LogEntry -> ReportState
updateReportState !s !e =
    (reportDistance %~ updateDistance (s ^. reportLastLocation) (e ^. entryLocation )) .
    (reportLastLocation .~ Just (e ^. entryLocation)) .
    (reportMin %~ updateMin (e ^. entryTemperature)) .
    (reportMax %~ updateMax (e ^. entryTemperature)) .
    (reportMean %~ updateMean (s ^. reportCount) (e ^. entryTemperature)) .
    (reportObservatories %~ updateObservatories (e ^. entryObservatory)) $! s

-- | Convert all temperatures and distances in the report to the provided
--   ones.
deNormalizeReport :: ReportState -> TemperatureUnit -> DistanceUnit -> ReportState
deNormalizeReport !r !t !d =
    (reportDistance %~ deNormalizeDistance d) .
    (reportMin      %~ maybeDeNormalizeTemperature t) .
    (reportMax      %~ maybeDeNormalizeTemperature t) .
    (reportMean     %~ maybeDeNormalizeTemperature t) $! r
  where
    maybeDeNormalizeTemperature _ Nothing = Nothing
    maybeDeNormalizeTemperature !u (Just !t) = Just $ deNormalizeTemperature u t

-- | Filters out errors from the decoded values; if a value is Left, an
--   error is output to stderr and the value is not yielded.
filterErrors :: MonadIO m => Pipe (Either String LogEntry) LogEntry m r
filterErrors = forever $ do
    val <- await
    case val of
        (Left e)      -> liftIO $ hPutStrLn stderr $ "Error decoding log entry: " <> e
        (Right entry) -> yield entry

-- | Increment the count by one for every entry seen.
updateCount :: LogEntry -> StateT ReportState IO LogEntry
updateCount e = do
    st <- get
    put $! (reportCount %~ (+ 1) $ st)
    return e

-- | Update the current report state with data from the provided
--   LogEntry.
accumulateState :: LogEntry -> StateT ReportState IO LogEntry
accumulateState !e = do
    st <- get
    put $! updateReportState st e
    return e

displayReport :: ReportState -> WeatherOpts -> IO ()
displayReport report WeatherOpts{..} = do
    let report' = deNormalizeReport report optNormalizeTemperature optNormalizeDistance
    maybePrint optReportMin ("Minimum temperature observed: " <>
        showMaybe (report' ^. reportMin) optNormalizeTemperature)
    maybePrint optReportMax ("Maximum temperature observed: " <>
        showMaybe (report' ^. reportMax) optNormalizeTemperature)
    maybePrint optReportMean ("Mean temperature observed: " <>
        showMaybe (report' ^. reportMean) optNormalizeTemperature)
    maybePrint optReportDistance ("Total distance travelled: " <>
        show (report' ^. reportDistance) <> " " <> show optNormalizeDistance <> ".")
    maybePrint optReportObservatories ("Records from each observatory: " <>
        showObservatories (report' ^. reportObservatories))
  where
    maybePrint False _ = return ()
    maybePrint True s = putStrLn s

    showMaybe Nothing _ = "insufficient data."
    showMaybe (Just v) unit = show v <> " " <> show unit <> "."

    showObservatories m = intercalate "\n" $ map showObservatory $ M.assocs m

    showObservatory (Observatory o,n) = "Reports from " <> T.unpack o <> ": " <> show n

-- | Take a ByteString Producer; decode entries and filter out the
--   invalid ones, count the valid ones and sort in a tempfile.
sortAndCount :: Producer ByteString (StateT ReportState IO) () -> StateT ReportState IO FilePath
sortAndCount inp = do
    (unsortedfn, unsortedfh) <- liftIO $ openTempFile "." ".tmp_unsorted.txt"
    runEffect $ decodeLogEntries inp >->
                filterErrors >->
                P.mapM updateCount >->
                P.map normalizeEntry >->
                encodeLogEntries >->
                PB.toHandle unsortedfh
    liftIO $ hClose unsortedfh
    sortedfn <- liftIO $ sortEntries unsortedfn
    liftIO $ removeFile unsortedfn
    return sortedfn

-- | Given a FilePath containing sorted valid entries and provided report
--   options, write normalized records to the output file while
--   accumulating data needed for the report.
accumAndOutput :: FilePath -> WeatherOpts -> StateT ReportState IO ()
accumAndOutput fn WeatherOpts{..} = do
    fh <- liftIO $ openFile fn ReadMode
    outputfh <- liftIO $ openFile optOutputPath WriteMode
    runEffect $ decodeLogEntries (PB.fromHandle fh) >->
                filterErrors >->
                P.mapM accumulateState >->
                P.map (deNormalizeEntry optNormalizeTemperature optNormalizeDistance) >->
                encodeLogEntries >->
                PB.toHandle outputfh
    liftIO $ hClose fh >> hClose outputfh >> removeFile fn
