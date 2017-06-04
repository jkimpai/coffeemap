port module GMaps exposing (..)

import SharedModels exposing (GMPos, CoffeeShop)

-- PORTS

port moveMap : GMPos -> Cmd msg

port reset : GMPos -> Cmd msg
port hardReset : GMPos -> Cmd msg

port getShop : Int -> Cmd msg
port gotShop : (String -> msg) -> Sub msg

port star : Int -> Cmd msg

port mapMoved : (GMPos -> msg) -> Sub msg

port saveShop : List String -> Cmd msg
port requestShop : String -> Cmd msg 
port receiveShop : ((List String) -> msg) -> Sub msg

