{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Data.List
import qualified Data.Map.Strict                  as M
import qualified Data.Text                        as T
import           Options.Applicative
import           Pipes
import qualified Pipes.ByteString                 as PB
import qualified Pipes.Prelude                    as P
import           System.Directory
import           System.IO
import           System.IO.Temp

import           Data.WeatherBalloon
import           Data.WeatherBalloon.Types

parseOpts = info (helper <*> opts)
             (   fullDesc
              <> progDesc "Writes a normalized and sorted version of the data to OUTPUT-FILE and optionally reports selected statistics to stdout."
              <> header   "weatherballoon-report - generate reports from weather balloon data"
             )
  where
    opts = WeatherOpts
           <$> flag False True (long "report-min"
                                <> help "Report minimum temperature observed."
                               )
           <*> flag False True (long "report-max"
                                <> help "Report maximum temperature observed."
                               )
           <*> flag False True (long "report-mean"
                                <> help "Report mean temperature observed."
                               )
           <*> flag False True (long "report-distance"
                                <> help "Report total distance travelled by the balloon."
                               )
           <*> flag False True (long "report-observatories"
                                <> help "Report number of observatories from which reports have been received."
                               )
           <*> option auto     (long "temperature-unit"
                                <> short 't'
                                <> value Kelvin
                                <> help "Unit to which to normalize temperatures (Kelvin, Celsius or Fahrenheit)."
                                <> metavar "TEMPERATURE-UNIT"
                               )
           <*> option auto     (long "distance-unit"
                                <> short 'd'
                                <> value Metres
                                <> help "Unit to which to normalize distances (Metres, Kilometres or Miles)."
                                <> metavar "DISTANCE-UNIT"
                               )
           <*> option auto     (long "output-file"
                                <> short 'o'
                                <> value "weather-out.txt"
                                <> help "Filepath to which to write normalized measurements."
                                <> metavar "OUTPUT-FILE"
                               )

-- | We read the input and stream it to a tempfile, normalizing it and
--   stripping out bad data along the way. Then sort the tempfile on
--   disk and stream in the sorted version, while accumulating the
--   information needed for the report. Finally, normalize the output to
--   the requested units, write it to the output file, and display the
--   report.
main :: IO ()
main = do
    o@WeatherOpts{..} <- execParser parseOpts
    (sortedfn, reportState) <- runStateT (sortAndCount PB.stdin) initialReportState
    report <- execStateT (accumAndOutput sortedfn o) reportState
    displayReport report o
