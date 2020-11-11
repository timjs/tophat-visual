module Main where

import Preload
import Concur (dynamic)
import Concur.Dom (runWidgetInDom)
import Task.Script.Example as Example
import Test.Flow as Flow

main :: Effect Unit
main = runWidgetInDom "root" <| dynamic <| Flow.task Example.enter_passenger
