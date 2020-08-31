module Main where

import Preload
import Concur (Widget)
import Concur.Dom (Dom, runWidgetInDom)
import Concur.Forms as Forms

hello :: forall a. Widget Dom a
hello = do
  void <| Forms.button "Say Hello"
  Forms.text "Hello Sailor!"

buts :: Widget Dom Unit
buts = Forms.button "blub" <|> Forms.button "blab"

main :: Effect Unit
main = runWidgetInDom "root" buts
