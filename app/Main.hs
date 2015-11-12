module Main where

import Lib
import Game
import Foundation

main :: IO ()
main = do
    let game = initGame "hello" "world"
    let pg = (\g -> printGame g)
    game >>= pg
    let game' = game >>= (\g -> setReady g "hello") >>= (\g -> setReady g "world")
    game' >>= pg
    let game'' = game' >>= tick >>= tick >>= tick
    game'' >>= pg
    someFunc
