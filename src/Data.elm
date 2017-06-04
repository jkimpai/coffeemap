module Data exposing (..)

import Array exposing (..)
import SharedModels exposing (CoffeeShop, GMPos, Entry)

shopsArr : Array (List String)
shopsArr = Array.fromList
    [ ["num", "id"      , "name"                       , "lat"      , "lng"       , "location"                  , "field"   , "rating"  , "" , "eid", "favorite"]
    , ["1"  , "cobb"    , "Cobb Coffee Shop"           , "41.788993", "-87.600852", "Cobb Hall"                 , ""        , "0"       , "" , "0"  , "0"]
    , ["2"  , "exlib"   , "Ex Libris Cafe"             , "41.792214", "-87.599934", "Regenstein Library"        , ""        , "0"       , "" , "0"  , "0"]
    , ["3"  , "hallowed", "Hallowed Grounds"           , "41.791235", "-87.598319", "Reynold's Club 2nd Floor"  , ""        , "0"       , "" , "0"  , "0"]
    , ["4"  , "cshop"   , "Einstein Bros (C-Shop)"     , "41.791238", "-87.598280", "Reynold's Club 1st Floor"  , ""        , "0"       , "" , "0"  , "0"]
    , ["5"  , "miriams" , "Miriam's Cafe at the Smart" , "41.793751", "-87.600099", "Smart Museum of Art"       , ""        , "0"       , "" , "0"  , "0"]
    , ["6"  , "harper"  , "Harper Cafe"                , "41.787987", "-87.599560", "Harper Memorial Library"   , ""        , "0"       , "" , "0"  , "0"]
    , ["7"  , "plein"   , "Plein Air Cafe"             , "41.790026", "-87.596066", "Seminary Co-op Bookstore"  , ""        , "0"       , "" , "0"  , "0"]
    , ["8"  , "div"     , "Grounds of Being"           , "41.788982", "-87.599999", "Divinity School"           , ""        , "0"       , "" , "0"  , "0"]
    , ["9"  , "dollop"  , "Dollop Coffee Co"           , "41.794797", "-87.598314", "Campus North Commons"      , ""        , "0"       , "" , "0"  , "0"]     
    ]

noShop : CoffeeShop
noShop = (CoffeeShop 0 "" "" (GMPos 0 0) "" "" 0 [] 0 False)

getStr : Int -> Array String -> String
getStr n arr =
    case Array.get n arr of 
        Just a  -> a
        Nothing -> ""

getFloat : Int -> Array String -> Float 
getFloat n arr =
    case Array.get n arr of 
        Just x  -> 
            case (String.toFloat x) of 
                Ok y -> y
                _ -> 0
        Nothing -> 0

getInt : Int -> Array String -> Int 
getInt n arr =
    case Array.get n arr of 
        Just x  -> 
            case (String.toInt x) of 
                Ok y -> y
                _ -> 0
        Nothing -> 0

getFav : Int -> Array String -> Bool
getFav n arr =
    case getInt n arr of 
        0 -> False
        _ -> True

setShops : Array (List String) -> Array CoffeeShop
setShops arr =
    let len = (Array.length arr) in  
    let foo i out = 
        if (i == len) 
        then out
        else 
            let shopInfo = Array.fromList (Maybe.withDefault [] (get i arr))
            in let notes = stringToNotes (getStr 8 shopInfo)
            in foo (i + 1) ( push (CoffeeShop i (getStr 1 shopInfo) (getStr 2 shopInfo) 
                (GMPos (getFloat 3 shopInfo) (getFloat 4 shopInfo)) 
                (getStr 5 shopInfo) (getStr 6 shopInfo) (getInt 7 shopInfo)
                notes (getInt 9 shopInfo) (getFav 10 shopInfo)) out)
    in 
        foo 1 (Array.fromList [noShop])

newEntry : String -> Int -> Entry
newEntry desc id =
    { description = desc
    , editing = False
    , id = id
    }

stringToNotes : String -> List Entry
stringToNotes str =
    let strList = String.split "," str 
    in let len = List.length strList 
    in let foo i strs = 
        case strs of 
            [] -> []
            [""] -> []
            s::rest ->
                let newEntr = newEntry s i 
                in [newEntr] ++ (foo (i+1) rest)
    in 
        foo 0 strList

toStringList : CoffeeShop -> List String
toStringList shp =
    [(toString shp.num)] ++
    [shp.id] ++
    [shp.name] ++
    [(toString shp.pos.lat)] ++
    [(toString shp.pos.lng)] ++
    [shp.loc] ++ 
    [shp.field] ++ 
    [toString shp.rating] ++ 
    [notesToString shp.notes] ++ 
    [toString shp.eid]

notesToString : List Entry -> String
notesToString notes =
    let foo ns acc =
        case ns of 
            [] -> acc
            n::rest -> foo rest (acc ++ n.description ++ ",")
    in 
        foo notes ""

fromStringList : List String -> CoffeeShop
fromStringList shp =
    let shopInfo = Array.fromList shp 
    in let notes = stringToNotes (getStr 8 shopInfo)
    in
        CoffeeShop (getInt 0 shopInfo) (getStr 1 shopInfo) (getStr 2 shopInfo) 
            (GMPos (getFloat 3 shopInfo) (getFloat 4 shopInfo)) 
            (getStr 5 shopInfo) (getStr 6 shopInfo) (getInt 7 shopInfo)
            notes (getInt 9 shopInfo) (getFav 10 shopInfo)

shops : Array CoffeeShop
shops = setShops shopsArr 

getNum : String -> Int
getNum id = 
    case id of 
        "cobb" -> 1
        "exlib" -> 2
        "hallowed" -> 3
        "cshop" -> 4
        "miriams" -> 5
        "harper" -> 6
        "plein" -> 7
        "div" -> 8
        "dollop" -> 9
        _ -> 0

getCS : String -> Array CoffeeShop -> CoffeeShop
getCS id shops = 
    case (Array.get (getNum id) shops) of 
        Just a -> a
        Nothing -> noShop
