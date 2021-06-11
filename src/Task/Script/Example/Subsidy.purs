module Task.Script.Example.Subsidy where

import Preload
import Data.HashMap as HashMap
import Task.Script.Context (Context, Typtext, recordOf', recordOf, taskOf, (:->))
import Task.Script.Knot (Unchecked(..))
import Task.Script.Syntax (Arguments(..), BasicType(..), Expression(..), Match(..), Task(..))

---- Context -------------------------------------------------------------------
types :: Typtext
types =
  from
    [ "Citizen"
        ~> recordOf'
            [ "ssn" ~> BName "Nat"
            , "name" ~> BName "String"
            , "address" ~> BName "Address"
            ]
    , "Company"
        ~> recordOf'
            [ "coc" ~> BName "Nat"
            , "name" ~> BName "String"
            , "address" ~> BName "Address"
            ]
    , "Address"
        ~> recordOf'
            [ "street" ~> BName "String"
            , "house_number" ~> BName "Nat"
            , "postal_code" ~> BName "Nat"
            , "city" ~> BName "String"
            ]
    , "Documents"
        ~> recordOf'
            [ "invoice_amount" ~> BName "Nat"
            , "invoice_date" ~> BName "Date"
            , "roof_photos" ~> BList (BName "String")
            ]
    , "Declaration"
        ~> recordOf'
            [ "roof_photos" ~> BList (BName "String")
            , "date" ~> BName "Date"
            ]
    , "Dossier"
        ~> recordOf'
            [ "documents" ~> BName "Documents"
            , "declaration" ~> BName "Declaration"
            ]
    ]

context :: Context
context =
  from
    [ "check_conditions"
        ~> recordOf [ "details" ~> BName "Citizen" ]
        :-> taskOf [ "approved" ~> BName "Bool" ]
    , "provide_documents"
        ~> recordOf [ "details" ~> BName "Citizen" ]
        :-> taskOf [ "documents" ~> BName "Documents" ]
    , "select_contractor"
        ~> recordOf []
        :-> taskOf [ "contractor" ~> BName "Company" ]
    , "provide_declaration"
        ~> recordOf [ "contractor" ~> BName "Company", "details" ~> BName "Citizen" ]
        :-> taskOf [ "declaration" ~> BName "Declaration" ]
    , "submit_request"
        ~> recordOf [ "dossier" ~> BName "Dossier" ]
        :-> taskOf []
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
    <| Step (MRecord <| from [ "value" ~> MBind "details" ]) (Unchecked <| Enter "Citizen" "Citizen details")
    <| Unchecked
    <| Step (MRecord <| from [ "approved" ~> MBind "approved" ]) (Unchecked <| Execute "check_conditions" (ARecord <| from [ "details" ~> Variable "details" ]))
    <| Unchecked
    <| Branch
        [ Apply (Variable "not") (Variable "approved")
            ~> ( Unchecked
                  <| View "Cannot approve" (Record HashMap.empty)
              )
        , Variable "approved"
            ~> ( Unchecked
                  <| Step (MRecord <| from [ "documents" ~> MBind "documents", "declaration" ~> MBind "declaration" ])
                      ( Unchecked
                          <| Pair
                              [ Unchecked
                                  <| Execute "provide_documents" (ARecord <| from [ "details" ~> Variable "details" ])
                              , Unchecked
                                  <| Step (MRecord <| from [ "contractor" ~> MBind "contractor" ]) (Unchecked <| Execute "select_contractor" (ARecord HashMap.empty))
                                  <| Unchecked
                                  <| Step (MRecord <| from [ "declaration" ~> MBind "declaration" ]) (Unchecked <| Execute "provide_declaration" (ARecord <| from [ "contractor" ~> Variable "contractor", "details" ~> Variable "details" ]))
                                  <| Unchecked
                                  <| Lift (Record <| from [ "contractor" ~> Variable "contractor", "declaration" ~> Variable "declaration" ])
                              ]
                      )
                  <| Unchecked
                  <| Execute "submit_request" (ARecord <| from [ "dossier" ~> Record (from [ "declaration" ~> Variable "declaration", "documents" ~> Variable "documents" ]) ])
              )
        ]
