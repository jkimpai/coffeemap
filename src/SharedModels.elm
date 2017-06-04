module SharedModels exposing (..)

type alias GMPos =
    { lat : Float
    , lng : Float
    }

type alias Entry =
    { description : String
    , editing : Bool
    , id : Int
    }

type alias CoffeeShop = 
    { num : Int             --0
    , id : String           --1
    , name : String         --2
    , pos : GMPos           --3,4
    , loc : String          --5
    , field : String        --6
    , rating : Int          --7
    , notes : List Entry    --8
    , eid : Int             --9
    , favorite: Bool        --10
    }


