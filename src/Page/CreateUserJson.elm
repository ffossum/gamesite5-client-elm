module Page.CreateUserJson exposing (CreateUser, encodeCreateUser)

import Json.Encode exposing (..)


type alias CreateUser =
    { name : String
    , email : String
    , password : String
    }


encodeCreateUser : CreateUser -> Value
encodeCreateUser u =
    object
        [ ( "name", string u.name )
        , ( "email", string u.email )
        , ( "password", string u.password )
        ]
