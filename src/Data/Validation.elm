module Data.Validation exposing (Valid, Validation(..), apply, chainAll, chainFirst, ensure, fromResult, fromValid, map, successAs, toResult, validate)


type Validation e a
    = Failure (List e)
    | Success a


type Valid a
    = Valid a


validate : Validation e a -> Result (List e) (Valid a)
validate va =
    case va of
        Failure es ->
            Err es

        Success a ->
            Ok (Valid a)


fromValid : Valid a -> a
fromValid (Valid a) =
    a


map : (a -> b) -> Validation e a -> Validation e b
map f v =
    case v of
        Failure e ->
            Failure e

        Success a ->
            Success (f a)


map2 : (a -> b -> c) -> Validation e a -> Validation e b -> Validation e c
map2 f va vb =
    map f va
        |> apply vb


apply : Validation e a -> Validation e (a -> b) -> Validation e b
apply va vf =
    case ( va, vf ) of
        ( Success a, Success f ) ->
            Success (f a)

        ( Failure e2, Failure e1 ) ->
            Failure (e1 ++ e2)

        ( Failure es, Success _ ) ->
            Failure es

        ( Success _, Failure es ) ->
            Failure es


toResult : Validation e a -> Result (List e) a
toResult v =
    case v of
        Failure e ->
            Err e

        Success a ->
            Ok a


fromResult : Result e a -> Validation e a
fromResult r =
    case r of
        Err e ->
            Failure [ e ]

        Ok a ->
            Success a


ensure : Bool -> e -> a -> Validation e a
ensure p e a =
    if p then
        Success a

    else
        Failure [ e ]


chainAll : Validation e a -> Validation e b -> Validation e a
chainAll target source =
    case ( target, source ) of
        ( Failure targetErrors, Failure sourceErrors ) ->
            Failure (sourceErrors ++ targetErrors)

        ( Success _, Failure e ) ->
            Failure e

        _ ->
            target


chainFirst : Validation e a -> Validation e b -> Validation e a
chainFirst target source =
    case ( target, source ) of
        ( _, Failure e ) ->
            Failure e

        _ ->
            target


successAs : a -> Validation e b -> Validation e a
successAs a =
    map (\_ -> a)


void : Validation e a -> Validation e ()
void =
    map (\_ -> ())
