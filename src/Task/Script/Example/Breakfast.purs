module Task.Script.Example.Breakfast where

import Preload hiding (pair)

import Task.Script.Annotation (Checked)
import Task.Script.Builder (execute, lift, step, pair)
import Task.Script.Syntax (Arguments(..), Expression(..), Match(..), Task)
import Task.Script.World (World)

arguments :: Array String -> Arguments
arguments = map (\x -> (x ~ Variable x)) >> from >> ARecord

values :: Array String -> Expression
values = map (\x -> (x ~ Variable x)) >> from >> Record

matches :: Array String -> Match
matches = map (\x -> (x ~ MBind x)) >> from >> MRecord

make_breakfast :: Checked Task
make_breakfast =
  pair
    [ step
        (matches [ "thee" ])
        (execute "zet thee" <| arguments [ "water", "zakje", "sterkte" ])
        <| lift (values [ "thee" ])
    , step
        (matches [ "gekookt ei" ])
        (execute "kook ei" <| arguments [ "rauw ei", "hardheid" ])
        <| lift (values [ "gekookt ei" ])
    ]

world :: World
world = { types: from [], context: from [], tasks: from [ "maak ontbijt" ~ (from [] ~ make_breakfast) ] }
