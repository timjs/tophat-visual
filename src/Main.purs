module Main where

import Preload
import Concur (dynamic)
import Concur.Dom (runWidgetInDom)
import Task.Script.Example.Subsidy as Example
import Task.Script.Renderer as Renderer

main :: Effect Unit
main = runWidgetInDom "root" <| dynamic <| Renderer.task Example.request_subsidy
