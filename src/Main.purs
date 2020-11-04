module Main where

import Preload
import Concur.Dom (runWidgetInDom)
import Test.Tree as Test

main :: Effect Unit
main = runWidgetInDom "root" <| Test.main
