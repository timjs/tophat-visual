module Task.Script.Example.Subsidy where

import Preload
import Task.Script.Context (Context, (:->), b_bool, b_int, b_string, b_list, t_record, t_task)
import Task.Script.Error (Unchecked(..))
import Task.Script.Syntax (Argument(..), BasicType(..), Expression(..), Match(..), Task(..))

---- Types ---------------------------------------------------------------------
b_citizen :: BasicType
b_citizen =
  BRecord
    <| from
        [ "ssn" ** b_int
        , "name" ** b_string
        , "address" ** b_address
        ]

b_company :: BasicType
b_company =
  BRecord
    <| from
        [ "coc" ** b_int
        , "name" ** b_string
        , "address" ** b_address
        ]

b_address :: BasicType
b_address =
  BRecord
    <| from
        [ "stree" ** b_string
        , "house_number" ** b_int
        , "postal_code" ** b_int
        , "city" ** b_string
        ]

b_documents :: BasicType
b_documents =
  BRecord
    <| from
        [ "invoice_amount" ** b_int
        , "invoice_date" ** b_int
        , "roof_photos" ** b_list b_string
        ]

b_declaration :: BasicType
b_declaration =
  BRecord
    <| from
        [ "roof_photos" ** b_list b_string
        , "date" ** b_string
        ]

b_dossier :: BasicType
b_dossier =
  BRecord
    <| from
        [ "documents" ** b_documents
        , "declaration" ** b_declaration
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
    <| Step (MRecord <| from [ "value" ** MBind "details" ]) (Unchecked <| Enter b_citizen "Passenger details")
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
        ** t_record [ "details" ** b_citizen ]
        :-> t_task [ "approved" ** b_bool ]
    , "provide_documents"
        ** t_record [ "details" ** b_citizen ]
        :-> t_task [ "documents" ** b_documents ]
    , "select_contractor"
        ** t_record []
        :-> t_task [ "contractor" ** b_company ]
    , "provide_declaration"
        ** t_record [ "contractor" ** b_company, "details" ** b_citizen ]
        :-> t_task [ "declaration" ** b_declaration ]
    , "submit_request"
        ** t_record [ "dossier" ** b_dossier ]
        :-> t_task []
    ]
