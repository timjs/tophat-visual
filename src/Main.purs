module Main where

import Preload
import Concur (dynamic)
import Concur.Dom (runWidgetInDom)
import Task.Script.Example.Subsidy as Example
import Task.Script.Renderer as Renderer
-- import Test.Selection as Test
import Test.Tree as Test

main :: Effect Unit
main = runWidgetInDom "root" <| Test.main
-- main = runWidgetInDom "root" <| Renderer.main Example.context Example.types Example.request_subsidy
