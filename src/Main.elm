module Main exposing (..)

import Html exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Html.Keyed as Keyed

import SharedModels exposing (GMPos, CoffeeShop, Entry)
import GMaps exposing (moveMap, mapMoved, getShop
    , reset, star, requestShop, receiveShop
    , saveShop, hardReset, gotShop)
import Array exposing (..)
import Data exposing (shops, getNum, getStr, getCS
    , setShops, toStringList, fromStringList)
import Notes exposing (newEntry, addEntry
    , updateField, editingEntry, updateEntry, delete)

-- MAIN
-- https://github.com/farmio/elm-demo-google-maps
main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL
type alias Model =
    { pos : GMPos 
    , current : CoffeeShop
    , shops : Array CoffeeShop
    , tempNotes : String 
    , anySelected : Bool
    }

-- UPDATE
type Msg
    = Find String
    | Move Direction
    | Reset
    | MapMoved GMPos
    | GotShop String
    | SetRating Int
    | ShowStar
    | Add
    | UpdateEntry Int String
    | EditingEntry Int Bool
    | Delete Int
    | LoadShop (List String)
    | UpdateField String 
    | ClearSaved

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Find id ->
            let shop = getCS id model.shops
            in 
                ({ model | pos = shop.pos
                    , current = shop
                    , anySelected = True }
                , getShop shop.num)
        
        Move direction ->
            let newPos = movePos model.pos direction
            in 
                ({ model | pos = newPos }, moveMap newPos)
        
        Reset -> 
            init
        
        MapMoved newPos ->
            ({ model | pos = newPos }, Cmd.none)
        
        GotShop id -> 
            let shop = getCS id model.shops
            in 
                ({ model | pos = shop.pos
                    , current = shop
                    , anySelected = True }
                , requestShop shop.id)
        
        SetRating r ->
            let n = model.current.num 
            in let shop = Array.get n model.shops 
            in 
                case shop of 
                    Just s -> 
                        let newShop = {s | rating = r}
                        in let newShops = set n newShop model.shops 
                        in 
                            ({ model | current = newShop, shops = newShops }
                            , saveShop (toStringList newShop))
                    Nothing ->
                        (model, Cmd.none)
        
        ShowStar ->
            let num = model.current.num in
            (model, star num)
        
        Add -> 
            let newShop = addEntry model.current 
            in let newShops = set newShop.num newShop model.shops
            in 
                ({ model | current = newShop
                    , shops = newShops }
                , saveShop (toStringList newShop))
        
        UpdateEntry id str -> 
            let newShop = updateEntry model.current id str
            in let newShops = set newShop.num newShop model.shops
            in 
                ({ model | current = newShop
                    , shops = newShops }
                , saveShop (toStringList newShop))
        
        EditingEntry id isEditing ->
            let newShop = editingEntry model.current id isEditing
            in let newShops = set newShop.num newShop model.shops
            in 
                ({ model | current = newShop
                    , shops = newShops }
                , Cmd.none)
        
        Delete id ->
            let newShop = delete model.current id
            in let newShops = set newShop.num newShop model.shops
            in 
                ({ model | current = newShop
                    , shops = newShops }
                , saveShop (toStringList newShop)) 
        
        UpdateField str -> 
            let newShop = updateField model.current str
            in let newShops = set newShop.num newShop model.shops
            in 
                ({ model | current = newShop
                    , shops = newShops }
                , Cmd.none) 

        LoadShop shopList ->
            if (shopList == [])
            then (model, Cmd.none)
            else 
                let shop = fromStringList shopList 
                in let newShops = set shop.num shop model.shops 
                in
                    ({ model | current = shop, shops = newShops }, Cmd.none)

        ClearSaved ->
            let uchicago = (GMPos 41.792 -87.5987) 
            in
                ( Model uchicago (getCS "none" Data.shops) Data.shops "" False, hardReset uchicago)

type Direction
    = North
    | South
    | West
    | East

movePos : GMPos -> Direction -> GMPos
movePos pos direction =
    case direction of
        North ->
            { pos | lat = pos.lat + 0.005 }
        South ->
            { pos | lat = pos.lat - 0.005 }
        West ->
            { pos | lng = pos.lng - 0.005 }
        East ->
            { pos | lng = pos.lng + 0.005 }

-- VIEW
drawRating : Int -> List (Html Msg)
drawRating num = 
    let fLoop n msg = 
        case n of 
          0 -> msg
          i -> fLoop (i-1) ([Html.img [Html.Attributes.src "icons/fillcup.svg", 
            Html.Attributes.style [("width", "40px"),("height", "60px")]] []] ++ msg)
    in let uLoop n msg = 
        case n of 
          0 -> msg
          i -> uLoop (i-1) ([Html.img [Html.Attributes.src "icons/emptycup.svg", 
            Html.Attributes.style [("width", "40px"),("height", "60px")]] []] ++ msg)
    in 
        (fLoop num []) ++ (uLoop (5 - num) [])

view : Model -> Html Msg
view model =
    let myStyle = Html.Attributes.style [("padding","5px")] in
    let showCoffee = 
        if (model.anySelected) 
        then 
            div [ Html.Attributes.style [("padding-top", "15px")] ] 
                [ h2 [] [ text model.current.name ]
                , h3 [] [ text model.current.loc ] 
                , div [ Html.Attributes.style [ ("text-align", "center") ] ] 
                    (drawRating model.current.rating)
                , div [ myStyle ] 
                    [ text "Rating: "
                    , button [ onClick (SetRating 1) ] [ text " 1 " ]
                    , button [ onClick (SetRating 2) ] [ text " 2 " ]
                    , button [ onClick (SetRating 3) ] [ text " 3 " ]
                    , button [ onClick (SetRating 4) ] [ text " 4 " ]
                    , button [ onClick (SetRating 5) ] [ text " 5 " ]
                    , button [ onClick ShowStar ] [ text "Favorite" ] 
                    ]
                , div [ class "notes-wrapper", Html.Attributes.style [ ( "visibility", "hidden" ) ] ]
                    [ section
                        [ class "notes" ]
                        [ lazy viewInput model.current.field
                        , lazy viewEntries model.current.notes
                        ]
                    , infoFooter
                    ]
                ]
        else div [Html.Attributes.style [("display","inline-text"), ("text-align", "center")] ]
            [ text "Select a coffee shop from above or on the map!" ]
    in 
    div [ Html.Attributes.style [("margin", "15px")] ]
        [ div [] 
            [ text "Navigate: "
            , button [ class "nav", onClick (Reset) ] [text "Re-Center"]
            , button [ class "nav", onClick (Move North) ] [ text "North" ]
            , button [ class "nav", onClick (Move South) ] [ text "South" ]
            , button [ class "nav", onClick (Move West) ] [ text "West" ]
            , button [ class "nav", onClick (Move East) ] [ text "East" ] 
            ]
        , div [] 
            [ button [ class "shps", onClick (Find "cobb") ] [ text "1. Cobb Cafe" ]
            , button [ class "shps", onClick (Find "exlib") ] [ text "2. Ex Libris" ]
            , button [ class "shps", onClick (Find "hallowed") ] [ text "3. Hallowed Grounds" ]
            , button [ class "shps", onClick (Find "cshop") ] [ text "4. C-Shop" ]
            , button [ class "shps", onClick (Find "miriams") ] [ text "5. Miriam's Cafe" ]
            , button [ class "shps", onClick (Find "harper") ] [ text "6. Harper Cafe" ]
            , button [ class "shps", onClick (Find "plein") ] [ text "7. Plein Air" ]
            , button [ class "shps", onClick (Find "div") ] [ text "8. Grounds of Being" ]
            , button [ class "shps", onClick (Find "dollop") ] [ text "9. Dollop Coffee Co" ] 
            ]
        , div [ myStyle ]
            [ button [class "reset", onClick (ClearSaved) ] [ text "Reset All" ] ]
        , showCoffee
        ]


viewInput : String -> Html Msg
viewInput task =
    header
        [ class "header" ]
        [ h1 [] [ text "notes" ]
        , input
            [ class "new-note"
            , placeholder "add notes"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)

-- VIEW ALL ENTRIES
viewEntries : List Entry -> Html Msg
viewEntries entries =
    let
        cssVisibility =
            if List.isEmpty entries then
                "hidden"
            else
                "visible"
    in
        section
            [ class "main"
            , style [ ( "visibility", cssVisibility ) ]
            ]
            [ Keyed.ul [ class "notes-list" ] <|
                List.map viewKeyedEntry entries
            ]

-- VIEW INDIVIDUAL ENTRIES
viewKeyedEntry : Entry -> ( String, Html Msg )
viewKeyedEntry todo =
    ( toString todo.id, lazy viewEntry todo )

viewEntry : Entry -> Html Msg
viewEntry notes =
    li
        [ classList [ ( "editing", notes.editing ) ] ]
        [ div
            [ class "view" ]
            [ label
                [ onDoubleClick (EditingEntry notes.id True) ]
                [ text notes.description ]
            , button
                [ class "destroy"
                , onClick (Delete notes.id)
                ]
                []
            ]
        , Html.input
            [ class "edit"
            , value notes.description
            , name "title"
            , id ("note-" ++ toString notes.id)
            , onInput (UpdateEntry notes.id)
            , onBlur (EditingEntry notes.id False)
            , onEnter (EditingEntry notes.id False)
            ]
            []
        ]

-- INFO FOOTER
infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "double-click to edit a note" ]]


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
        [ mapMoved MapMoved
        , receiveShop LoadShop
        , gotShop GotShop
        ]
    
-- INIT
init : ( Model, Cmd Msg )
init =
    let uchicago = (GMPos 41.792 -87.5987) 
    in
        ( Model uchicago (getCS "none" Data.shops) Data.shops "" False, reset uchicago)
        

