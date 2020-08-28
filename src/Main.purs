module Main where

import Preload
import Concur (Widget)
import Concur.Forms (Html)
import Concur.Forms as Forms

hello :: forall a. Widget Html a
hello = do
  void <| Forms.button "Say Hello"
  Forms.text "Hello Sailor!"

main :: Effect Unit
main = Forms.runWidgetInDom "root" hello
