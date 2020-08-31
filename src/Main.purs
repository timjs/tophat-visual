module Main where

import Preload
import Concur (Widget)
import Concur.Dom (Dom, runWidgetInDom)
import Concur.Forms as Forms
import Test.Counter as Test

hello :: forall a. Widget Dom a
hello = do
  void <| Forms.button "Say Hello"
  Forms.text "Hello Sailor!"

main :: Effect Unit
main = runWidgetInDom "root" Test.main'
