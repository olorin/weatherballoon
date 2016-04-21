module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "lib/Data/WeatherBalloon/Types.hs", "lib/Data/WeatherBalloon/Util.hs" ]
