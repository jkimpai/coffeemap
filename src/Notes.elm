port module Notes exposing (..)

import SharedModels exposing (GMPos, CoffeeShop, Entry)
import Data exposing (shops, getNum, getStr, getCS)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import String
import Task

newEntry : String -> Int -> Entry
newEntry desc id =
    { description = desc
    , editing = False
    , id = id
    }

addEntry : CoffeeShop -> CoffeeShop 
addEntry shop =
    let newNotes = shop.notes ++ [ newEntry shop.field shop.eid ]
    in
        { shop | field = "", eid = shop.eid + 1, notes = newNotes }

updateField : CoffeeShop -> String -> CoffeeShop 
updateField shop str  = 
    { shop | field = str }

editingEntry : CoffeeShop -> Int -> Bool -> CoffeeShop
editingEntry shop id isEditing =
    let
        updateEntry t =
            if t.id == id then
                { t | editing = isEditing }
            else
                t
    in
        { shop | notes = List.map updateEntry shop.notes }

updateEntry : CoffeeShop -> Int -> String -> CoffeeShop 
updateEntry shop id str =
    let
        updateEntry t =
            if t.id == id then
                { t | description = str }
            else
                t
    in
        { shop | notes = List.map updateEntry shop.notes }

delete : CoffeeShop -> Int -> CoffeeShop
delete shop id = 
    { shop | notes = List.filter (\t -> t.id /= id) shop.notes }