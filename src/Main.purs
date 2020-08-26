module Main where

import Preload
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as Html
import Concur.React.Props as Props
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)

hello :: forall a. Widget HTML a
hello = do
  void <| Html.button [ Props.onClick ] [ Html.text "Say Hello" ]
  Html.text "Hello Sailor!"

main :: Effect Unit
main = runWidgetInDom "root" hello
