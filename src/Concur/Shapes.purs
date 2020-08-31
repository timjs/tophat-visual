module Concur.Shapes
  -- # Shapes
  ( box
  -- # Rendering
  , render
  ) where

import Preload
import Concur (Widget)
import Concur.Dom (Dom)
import Concur.Dom.Tags as Tag
import Concur.Dom.Attributes as Attr

---- Dimensions ----------------------------------------------------------------
measure :: Int
measure = 40

---- Shapes --------------------------------------------------------------------
box :: forall a. String -> Widget Dom a
box label = Tag.rect [ Attr.fill "lightgray", Attr.width (show w), Attr.height (show h) ] [ Tag.text [ Attr.fill "red" ] [ Tag.text_ label ] ]
  where
  w = max (4 * measure) ( {-width label + -}h)

  h = measure

---- Render --------------------------------------------------------------------
render :: forall a. Widget Dom a -> Widget Dom a
--FIXME: remove hardcoded values
render w = Tag.svg [ Attr.width "1280", Attr.height "800" ] [ w ]
