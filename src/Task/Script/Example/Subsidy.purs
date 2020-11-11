module Task.Script.Example.Subsidy where

import Preload
import Task.Script.Error (Unchecked(..))
import Task.Script.Syntax (BasicType(..), Argument(..), Constant(..), Expression(..), Match(..), PrimType(..), Task(..), Type(..))

---- Types ---------------------------------------------------------------------
t_citizen :: BasicType
t_citizen =
  BRecord
    <| from
        [ "ssn" ** BPrimitive TInt
        , "name" ** BPrimitive TString
        , "address" ** t_address
        ]

t_company :: BasicType
t_company =
  BRecord
    <| from
        [ "coc" ** BPrimitive TInt
        , "name" ** BPrimitive TString
        , "address" ** t_address
        ]

t_address :: BasicType
t_address =
  BRecord
    <| from
        [ "stree" ** BPrimitive TString
        , "house_number" ** BPrimitive TInt
        , "postal_code" ** BPrimitive TInt
        , "city" ** BPrimitive TString
        ]

t_documents :: BasicType
t_documents =
  BRecord
    <| from
        [ "invoice_amount" ** BPrimitive TInt
        , "invoice_date" ** BPrimitive TInt
        , "roof_photos" ** BList (BPrimitive TString)
        ]

t_declaration :: BasicType
t_declaration =
  BRecord
    <| from
        [ "roof_photos" ** BList (BPrimitive TString)
        , "date" ** BPrimitive TString
        ]

t_dossier :: BasicType
t_dossier =
  BRecord
    <| from
        [ "documents" ** t_documents
        , "declaration" ** t_declaration
        ]

---- Tasks ---------------------------------------------------------------------
request_subsidy :: Unchecked Task
request_subsidy =
  Unchecked
    <| Step (MRecord <| from [ "value" ** MBind "details" ]) (Unchecked <| Enter t_citizen "Passenger details")
    <| Unchecked
    <| Step (MRecord <| from [ "approved" ** MBind "approved" ]) (Unchecked <| Execute "check_conditions" (ARecord <| from [ "details" ** Variable "details" ]))
    <| Unchecked
    <| Branch
        [ Apply (Variable "not") (Variable "approved")
            ** ( Unchecked
                  <| View "Cannot apply for this subsidy" (Record neutral)
              )
        , Variable "approved"
            ** ( Unchecked
                  <| Step (MRecord <| from [ "documents" ** MBind "documents", "declaration" ** MBind "declaration" ])
                      ( Unchecked
                          <| Pair
                              [ Unchecked
                                  <| Execute "provide_documents" (ARecord <| from [ "details" ** Variable "details" ])
                              , Unchecked
                                  <| Step (MRecord <| from [ "contractor" ** MBind "contractor" ]) (Unchecked <| Execute "select_contractor" (ARecord neutral))
                                  <| Unchecked
                                  <| Step (MRecord <| from [ "declaration" ** MBind "declaration" ]) (Unchecked <| Execute "provide_declaration" (ARecord <| from [ "contractor" ** Variable "contractor", "details" ** Variable "details" ]))
                                  <| Unchecked
                                  <| Lift (Record <| from [ "contractor" ** Variable "contractor", "declaration" ** Variable "declaration" ])
                              ]
                      )
                  <| Unchecked
                  <| Execute "submit_request" (ARecord <| from [ "dossier" ** Record (from [ "declaration" ** Variable "declaration", "documents" ** Variable "documents" ]) ])
              )
        ]
