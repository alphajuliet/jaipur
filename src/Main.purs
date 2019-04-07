-- src/Main.purs
-- AndrewJ 2019-03-31

module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Jaipur (initialState)

main :: Effect Unit
main = do
  log $ show initialState
