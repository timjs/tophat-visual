module Task.Script.Example.Booking where

import Preload
import Data.HashMap as HashMap
import Task.Script.Annotation (Checked, unchecked)
import Task.Script.Syntax (Constant(..), Expression(..), Match(..), PrimType(..), Task(..), Type_(..))

---- Types ---------------------------------------------------------------------
t_nationality :: Type_
t_nationality =
  TVariant
    <| from
      [ "Dutch" ~ TRecord HashMap.empty
      , "British" ~ TRecord HashMap.empty
      , "German" ~ TRecord HashMap.empty
      ]

t_passenger :: Type_
t_passenger =
  TRecord
    <| from
      [ "first_name" ~ TPrimitive TString
      , "last_name" ~ TPrimitive TString
      , "nationality" ~ t_nationality
      , "age" ~ TPrimitive TInt
      ]

t_flight :: Type_
t_flight =
  TVariant
    <| from
      [ "ToAmsterdam" ~ TRecord HashMap.empty
      , "ToLondon" ~ TRecord HashMap.empty
      , "ToBerlin" ~ TRecord HashMap.empty
      ]

t_row :: Type_
t_row = TPrimitive TInt

t_chair :: Type_
t_chair = TPrimitive TString

t_seat :: Type_
t_seat =
  TRecord
    <| from
      [ "row" ~ t_row
      , "chair" ~ t_chair
      ]

t_booking :: Type_
t_booking =
  TRecord
    <| from
      [ "passengers" ~ TList t_passenger
      , "flight" ~ t_flight
      , "seats" ~ TList t_seat
      ]

---- Tasks ---------------------------------------------------------------------
enter_passenger :: Checked Task
enter_passenger =
  unchecked
    <| Step (MRecord <| from [ "value" ~ MBind "passengers" ]) (unchecked <| Enter "String")
    <| unchecked
    <| Select
      [ "Continue"
          ~ Constant (B true)
          ~ (unchecked <| Lift (Record <| from [ "passengers" ~ Variable "passengers" ]))
      ]

-- choose_seats :: Expression -> Expression -> Checked Task
-- choose_seats store record =
--   unchecked
--     <| Step (MRecord <| from ["values" ~ MBind "seats"]) (unchecked <| Change "Pick some seats" store)
--     <| unchecked
--     <| Select
--       ["Continue"
--         ~ Apply (Variable "==") (Apply (Variable "len") (Variable "seats")) (Variable "amount")
--         ~ (unchecked
--           <| Step (MBind ""))
--       ]
