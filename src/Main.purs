module Main where

import Preload
import Concur.Dom (runWidgetInDom)
import Test.Counter as Test

main :: Effect Unit
main = runWidgetInDom "root" <| Test.main
