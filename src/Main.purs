-- src/Main.purs
-- AndrewJ 2019-03-31

module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Jaipur (initGame)
import Model (showState)

main :: Effect Unit
main = do
  s0 <- initGame
  log $ showState s0