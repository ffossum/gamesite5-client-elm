module Data.Validation exposing (Valid, Validation(..), ap, apply, combineErrors, ensure, firstError, fromResult, fromValid, map, toResult, validate)


type Validation e a
    = Bad (List e)
    | Good a


type Valid a
    = Valid a


validate : Validation e a -> Result (List e) (Valid a)
validate va =
    case va of
        Bad es ->
            Err es

        Good a ->
            Ok (Valid a)


fromValid : Valid a -> a
fromValid (Valid a) =
    a


map : (a -> b) -> Validation e a -> Validation e b
map f v =
    case v of
        Bad e ->
            Bad e

        Good a ->
            Good (f a)


map2 : (a -> b -> c) -> Validation e a -> Validation e b -> Validation e c
map2 f va vb =
    map f va
        |> apply vb


apply : Validation e a -> Validation e (a -> b) -> Validation e b
apply va vf =
    ap vf va


ap : Validation e (a -> b) -> Validation e a -> Validation e b
ap vf va =
    case ( vf, va ) of
        ( Good f, Good a ) ->
            Good (f a)

        ( Bad es1, Bad es2 ) ->
            Bad (es1 ++ es2)

        ( Bad es, Good _ ) ->
            Bad es

        ( Good _, Bad es ) ->
            Bad es


toResult : Validation e a -> Result (List e) a
toResult v =
    case v of
        Bad e ->
            Err e

        Good a ->
            Ok a


fromResult : Result e a -> Validation e a
fromResult r =
    case r of
        Err e ->
            Bad [ e ]

        Ok a ->
            Good a


ensure : (a -> Bool) -> (a -> e) -> a -> Validation e a
ensure pred err a =
    if pred a then
        Good a

    else
        Bad [ err a ]


combineErrors : Validation e a -> Validation e b -> Validation e b
combineErrors va vb =
    map (\a b -> b) va |> apply vb


firstError : Validation e a -> Validation e b -> Validation e b
firstError va vb =
    case ( va, vb ) of
        ( Bad e, _ ) ->
            Bad e

        _ ->
            vb
