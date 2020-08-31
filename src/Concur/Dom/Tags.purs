module Concur.Dom.Tags
  ( module Html
  , module Svg
  , text_
  ) where

import Concur.Core.LiftWidget (class LiftWidget)
import Concur.Dom (Dom)
import Concur.React.DOM (text) as Internal
import Concur.React.DOM hiding (text) as Html
import Concur.React.SVG as Svg

text_ :: forall m a. LiftWidget Dom m => String -> m a
text_ = Internal.text
