module Task.Script.Example.Subsidy where

import Preload
import Concur.Core.DevTools (subscribe)
import Task.Script.Error (Unchecked(..), Context)
import Task.Script.Syntax (Argument(..), BasicType(..), Constant(..), Expression(..), Match(..), PrimType(..), Row, Task(..), Type(..), ofBasic)

---- Types ---------------------------------------------------------------------
t_bool :: BasicType
t_bool = BPrimitive TBool

t_int :: BasicType
t_int = BPrimitive TInt

t_string :: BasicType
t_string = BPrimitive TString

t_list :: BasicType -> BasicType
t_list t = BList t

t_task :: Array (String ** BasicType) -> Type
t_task ts = TTask (map ofBasic (from ts))

t_citizen :: BasicType
t_citizen =
  BRecord
    <| from
        [ "ssn" ** t_int
        , "name" ** t_string
        , "address" ** t_address
        ]

t_company :: BasicType
t_company =
  BRecord
    <| from
        [ "coc" ** t_int
        , "name" ** t_string
        , "address" ** t_address
        ]

t_address :: BasicType
t_address =
  BRecord
    <| from
        [ "stree" ** t_string
        , "house_number" ** t_int
        , "postal_code" ** t_int
        , "city" ** t_string
        ]

t_documents :: BasicType
t_documents =
  BRecord
    <| from
        [ "invoice_amount" ** t_int
        , "invoice_date" ** t_int
        , "roof_photos" ** t_list t_string
        ]

t_declaration :: BasicType
t_declaration =
  BRecord
    <| from
        [ "roof_photos" ** t_list t_string
        , "date" ** t_string
        ]

t_dossier :: BasicType
t_dossier =
  BRecord
    <| from
        [ "documents" ** t_documents
        , "declaration" ** t_declaration
        ]

---- Tasks ---------------------------------------------------------------------
{-
  {value = details} <- enter {name : String, ssn : Int, address : {postal_code : Int, house_number : Int, city : String, stree : String}} "Passenger details"
  {approved} <- check_conditions {details}
  branch [
    not approved |->
    view "Cannot apply for this subsidy" {}
    approved |->
      {documents, declaration} <- pair [
        provide_documents {details}
        {contractor} <- select_contractor {}
        {declaration} <- provide_declaration {details, contractor}
        done {contractor, declaration}
    ]
    submit_request {dossier = {documents, declaration}}
  ]
-}
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

---- Context -------------------------------------------------------------------
subsidy_context :: Context
subsidy_context =
  from
    [ "check_conditions"
        ** TFunction
            (ofBasic <| BRecord <| from [ "details" ** t_citizen ])
            (t_task [ "approved" ** t_bool ])
    , "provide_documents"
        ** TFunction
            (ofBasic <| BRecord <| from [ "details" ** t_citizen ])
            (t_task [ "documents" ** t_documents ])
    , "select_contractor"
        ** TFunction
            (ofBasic <| BRecord neutral)
            (t_task [ "contractor" ** t_company ])
    , "provide_declaration"
        ** TFunction
            (ofBasic <| BRecord <| from [ "contractor" ** t_company, "details" ** t_citizen ])
            (t_task [ "declaration" ** t_declaration ])
    , "submit_request"
        ** TFunction
            (ofBasic <| BRecord <| from [ "dossier" ** t_dossier ])
            (t_task [])
    ]

toplevel :: Context
toplevel =
  from
    [ "not" ** TFunction (TPrimitive TBool) (TPrimitive TBool) ]
