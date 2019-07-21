module Global exposing (Global, Session(..), SessionUser, sessionUserDecoder)

import Browser.Navigation as Nav
import Json.Decode as D


type alias Global =
    { navKey : Nav.Key
    , session : Session
    }


type Session
    = LoggedIn SessionUser
    | NotLoggedIn


type alias SessionUser =
    { id : Int
    , name : String
    , email : String
    }


sessionUserDecoder : D.Decoder SessionUser
sessionUserDecoder =
    D.map3 SessionUser
        (D.at [ "id" ] D.int)
        (D.at [ "name" ] D.string)
        (D.at [ "email" ] D.string)
