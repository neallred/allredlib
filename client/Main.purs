module Main where

import Prelude
import Effect (Effect)
import List as List
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Effect.Console (log)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI List.component unit body
