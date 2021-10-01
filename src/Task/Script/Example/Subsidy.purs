module Task.Script.Example.Subsidy where

import Preload hiding (pair)

import Data.HashMap as HashMap

import Task.Script.Annotation (Checked, unchecked)
import Task.Script.Context (Context, Typtext, recordOf', recordOf, taskOf, (:->))
import Task.Script.Syntax (Arguments(..), BasicType(..), Constant(..), Expression(..), Match(..), PrimType(..), Task(..))
import Task.Script.Builder (branch, execute, lift, pair, step, view)

---- Context -------------------------------------------------------------------

types :: Typtext
types =
  from
    [ "Citizen"
        ~ BPrimitive TString
    -- ~ recordOf'
    --     [ "ssn" ~ BName "Nat"
    --     , "name" ~ BName "String"
    --     , "address" ~ BName "Address"
    --     ]
    , "Company"
        ~ BPrimitive TString
    -- ~ recordOf'
    --     [ "coc" ~ BName "Nat"
    --     , "name" ~ BName "String"
    --     , "address" ~ BName "Address"
    --     ]
    , "Address"
        ~ BPrimitive TString
    -- ~ recordOf'
    --     [ "street" ~ BName "String"
    --     , "house_number" ~ BName "Nat"
    --     , "postal_code" ~ BName "Nat"
    --     , "city" ~ BName "String"
    --     ]
    , "Documents"
        ~ BPrimitive TInt
    -- ~ recordOf'
    --     [ "invoice_amount" ~ BName "Nat"
    --     , "invoice_date" ~ BName "Date"
    --     , "roof_photos" ~ BList (BName "String")
    --     ]
    , "Declaration"
        ~ BPrimitive TInt
    -- ~ recordOf'
    --     [ "roof_photos" ~ BList (BName "String")
    --     , "date" ~ BName "Date"
    --     ]
    , "Dossier"
        ~ recordOf'
            [ "documents" ~ BName "Documents"
            , "declaration" ~ BName "Declaration"
            ]
    ]

context :: Context
context =
  from
    [ "check_conditions"
        ~ recordOf [ "details" ~ BName "Citizen" ]
        :-> taskOf [ "approved" ~ BName "Bool" ]
    , "provide_documents"
        ~ recordOf [ "details" ~ BName "Citizen" ]
        :-> taskOf [ "documents" ~ BName "Documents" ]
    , "select_contractor"
        ~ recordOf []
        :-> taskOf [ "contractor" ~ BName "Company" ]
    , "provide_declaration"
        ~ recordOf [ "contractor" ~ BName "Company", "details" ~ BName "Citizen" ]
        :-> taskOf [ "declaration" ~ BName "Declaration" ]
    , "submit_request"
        -- ~ recordOf [ "dossier" ~ BName "Dossier" ]
        ~ recordOf
            [ "documents" ~ BName "Documents"
            , "declaration" ~ BName "Declaration"
            ]
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
request_subsidy :: Checked Task
request_subsidy =
  step (MRecord <| from [ "value" ~ MBind "details" ]) (unchecked <| Enter "Citizen")
    <| branch (MRecord <| from [ "approved" ~ MBind "approved" ]) (unchecked <| Execute "check_conditions" (ARecord <| from [ "details" ~ Variable "details" ]))
      [ Apply (Variable "not") (Variable "approved")
          ~
            ( view (Constant (S "Cannot approve"))
            )
      , Variable "approved"
          ~
            ( step (MRecord <| from [ "documents" ~ MBind "documents", "declaration" ~ MBind "declaration" ])
                ( pair
                    [ step (MRecord <| from [ "documents" ~ MBind "documents" ]) (unchecked <| Execute "provide_documents" (ARecord <| from [ "details" ~ Variable "details" ]))
                        <| lift (Record <| from [ "documents" ~ Variable "documents" ])
                    , step (MRecord <| from [ "contractor" ~ MBind "contractor" ]) (unchecked <| Execute "select_contractor" (ARecord HashMap.empty))
                        <| step (MRecord <| from [ "declaration" ~ MBind "declaration" ]) (unchecked <| Execute "provide_declaration" (ARecord <| from [ "contractor" ~ Variable "contractor", "details" ~ Variable "details" ]))
                        <| lift (Record <| from [ "contractor" ~ Variable "contractor", "declaration" ~ Variable "declaration" ])
                    ]
                )
                -- <| execute "submit_request" (ARecord <| from [ "dossier" ~ Record (from [ "declaration" ~ Variable "declaration", "documents" ~ Variable "documents" ]) ])
                <| execute "submit_request" (ARecord <| from [ "declaration" ~ Variable "declaration", "documents" ~ Variable "documents" ])
            )
      ]
