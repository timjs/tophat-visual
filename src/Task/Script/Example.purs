module Task.Script.Example where

import Preload
import Task.Script.Syntax (BasicType(..), Constant(..), Expression(..), Match(..), PrimType(..), Task(..), Type(..))
import Task.Script.Error (Unchecked(..))

---- Types ---------------------------------------------------------------------
t_nationality :: Type
t_nationality =
  TVariant
    <| from
        [ "Dutch" ** TRecord neutral
        , "British" ** TRecord neutral
        , "German" ** TRecord neutral
        ]

t_passenger :: Type
t_passenger =
  TRecord
    <| from
        [ "first_name" ** TPrimitive TString
        , "last_name" ** TPrimitive TString
        , "nationality" ** t_nationality
        , "age" ** TPrimitive TInt
        ]

t_flight :: Type
t_flight =
  TVariant
    <| from
        [ "ToAmsterdam" ** TRecord neutral
        , "ToLondon" ** TRecord neutral
        , "ToBerlin" ** TRecord neutral
        ]

t_row :: Type
t_row = TPrimitive TInt

t_chair :: Type
t_chair = TPrimitive TString

t_seat :: Type
t_seat =
  TRecord
    <| from
        [ "row" ** t_row
        , "chair" ** t_chair
        ]

t_booking :: Type
t_booking =
  TRecord
    <| from
        [ "passengers" ** TList t_passenger
        , "flight" ** t_flight
        , "seats" ** TList t_seat
        ]

---- Tasks ---------------------------------------------------------------------
enter_passenger :: Unchecked Task
enter_passenger =
  Unchecked
    <| Step (MRecord <| from [ "value" ** MBind "passengers" ]) (Unchecked <| Enter (BPrimitive TString) "Passenger details")
    <| Unchecked
    <| Select
    <| from
        [ "Continue"
            ** Constant (B true)
            ** (Unchecked <| Lift (Record <| from [ "passengers" ** Variable "passengers" ]))
        ]

simple :: Unchecked Task
simple =
  Unchecked
    <| Step (MRecord <| from [ "value" ** MBind "passengers" ]) (Unchecked <| Enter (BPrimitive TString) "Passenger details")
    <| Unchecked
    <| Update "Update passenger details" (Variable "passengers")
