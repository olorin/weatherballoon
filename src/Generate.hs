module Main where

import qualified Data.ByteString.Char8     as C
import           Options.Applicative
import           Pipes
import qualified Pipes.ByteString          as PB
import           Pipes.Csv
import qualified Pipes.Prelude             as P
import           System.IO
import           Test.QuickCheck

import           Data.WeatherBalloon
import           Data.WeatherBalloon.Types

args = info (helper <*> argument auto (metavar "COUNT"))
            (   fullDesc
             <> progDesc "Write COUNT phony records to stdout. The reports are not ordered, and some may be invalid."
             <> header   "weatherballoon-generate - generate random weather-balloon reports"
            )

main :: IO ()
main = do
    count <- execParser args
    runEffect $ produceEntries count >->
                encodeLogEntries >->
                PB.toHandle stdout
  where
    produceEntries n = P.replicateM n (generate arbitrary)
