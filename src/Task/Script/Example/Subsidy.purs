module Task.Script.Example.Subsidy where

import Preload
import Task.Script.Context (Context, Typtext, listOf, recordOf, taskOf, (:->))
import Task.Script.Error (Unchecked(..))
import Task.Script.Syntax (Argument(..), BasicType(..), Expression(..), Match(..), Task(..))

---- Types ---------------------------------------------------------------------
t_citizen :: BasicType
t_citizen =
  BRecord
    <| from
        [ "ssn" ** BName "Nat"
        , "name" ** BName "String"
        , "address" ** BName "Address"
        ]

t_company :: BasicType
t_company =
  BRecord
    <| from
        [ "coc" ** BName "Nat"
        , "name" ** BName "String"
        , "address" ** BName "Address"
        ]

t_address :: BasicType
t_address =
  BRecord
    <| from
        [ "street" ** BName "String"
        , "house_number" ** BName "Nat"
        , "postal_code" ** BName "Nat"
        , "city" ** BName "String"
        ]

t_documents :: BasicType
t_documents =
  BRecord
    <| from
        [ "invoice_amount" ** BName "Nat"
        , "invoice_date" ** BName "Date"
        , "roof_photos" ** BList (BName "String")
        ]

t_declaration :: BasicType
t_declaration =
  BRecord
    <| from
        [ "roof_photos" ** BList (BName "String")
        , "date" ** BName "Date"
        ]

t_dossier :: BasicType
t_dossier =
  BRecord
    <| from
        [ "documents" ** BName "Documents"
        , "declaration" ** BName "Declaration"
        ]

---- Tasks ---------------------------------------------------------------------
{-
  {value = details} <- enter Citizen "Passenger details"
  {approved} <- check_conditions {details}
  branch
    not approved |->
      view "Cannot apply for this subsidy" {}
    approved |->
      {documents, declaration} <- pair
        provide_documents {details}
        {contractor} <- select_contractor {}
        {declaration} <- provide_declaration {details, contractor}
        done {contractor, declaration}
    submit_request {dossier = {documents, declaration}}
-}
request_subsidy :: Unchecked Task
request_subsidy =
  Unchecked
    <| Step (MRecord <| from [ "value" ** MBind "details" ]) (Unchecked <| Enter "Citizen" "Citizen details")
    <| Unchecked
    <| Step (MRecord <| from [ "approved" ** MBind "approved" ]) (Unchecked <| Execute "check_conditions" (ARecord <| from [ "details" ** Variable "details" ]))
    <| Unchecked
    <| Branch
        [ Apply (Variable "not") (Variable "approved")
            ** ( Unchecked
                  <| View "Cannot approve" (Record neutral)
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
context :: Context
context =
  from
    [ "check_conditions"
        ** recordOf [ "details" ** BName "Citizen" ]
        :-> taskOf [ "approved" ** BName "Bool" ]
    , "provide_documents"
        ** recordOf [ "details" ** BName "Citizen" ]
        :-> taskOf [ "documents" ** BName "Documents" ]
    , "select_contractor"
        ** recordOf []
        :-> taskOf [ "contractor" ** BName "Company" ]
    , "provide_declaration"
        ** recordOf [ "contractor" ** BName "Company", "details" ** BName "Citizen" ]
        :-> taskOf [ "declaration" ** BName "Declaration" ]
    , "submit_request"
        ** recordOf [ "dossier" ** BName "Dossier" ]
        :-> taskOf []
    ]

typtext :: Typtext
typtext =
  from
    [ "Citizen" ** t_citizen
    , "Company" ** t_company
    , "Address" ** t_address
    , "Documents" ** t_documents
    , "Declaration" ** t_declaration
    , "Dossier" ** t_dossier
    ]
