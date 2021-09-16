module Task.Script.Expander
  ( class Expand
  , expand
  ) where

import Preload

import Data.HashMap as HashMap
import Task.Script.Error (Error(..))
import Task.Script.Syntax (BasicType(..), Type_(..), ofBasic)
import Task.Script.Context (Typtext)

---- Alias expansion -----------------------------------------------------------

class Expand t where
  expand :: Typtext -> t -> Error + t

instance Expand Type_ where
  expand s = case _ of
    TFunction t1 t2 -> TFunction <|| expand s t1 -|| expand s t2
    TName n -> HashMap.lookup n s |> note (UnknownTypeName n) ||> ofBasic ||= expand s
    TList t -> TList <|| expand s t
    TRecord ts -> TRecord <|| traverse (expand s) ts
    TVariant ts -> TVariant <|| traverse (expand s) ts
    TReference b -> TReference <|| expand s b
    TTask ts -> TTask <|| traverse (expand s) ts
    TPrimitive p -> done <| TPrimitive p

instance Expand BasicType where
  expand s = case _ of
    BName n -> HashMap.lookup n s |> note (UnknownTypeName n) ||= expand s
    BList b -> BList <|| expand s b
    BRecord bs -> BRecord <|| traverse (expand s) bs
    BVariant bs -> BVariant <|| traverse (expand s) bs
    BPrimitive p -> done <| BPrimitive p