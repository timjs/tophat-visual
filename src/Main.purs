module Main where

import Preload
import Concur (dynamic)
import Concur.Dom (runWidgetInDom)
import Task.Script.Example.Subsidy as Example
import Task.Script.Renderer as Renderer
import Test.Selection as Test

main :: Effect Unit
-- main = runWidgetInDom "root" <| dynamic <| Renderer.task Example.request_subsidy
main = runWidgetInDom "root" <| Test.main

-- main = runWidgetInDom "root" <| Renderer.main Example.request_subsidy
