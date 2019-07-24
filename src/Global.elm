module Global exposing (Global, Session(..), SessionUser, sessionUserDecoder)

import Browser.Navigation as Nav
import Json.Decode as D


type alias Global =
    { navKey : Nav.Key
    , session : Session
    }


type Session
    = UnknownLoggedIn
    | LoggedIn SessionUser
    | NotLoggedIn


type alias SessionUser =
    { id : Int
    , name : String
    , email : String
    }


sessionUserDecoder : D.Decoder SessionUser
sessionUserDecoder =
    D.map3 SessionUser
        (D.field "id" D.int)
        (D.field "name" D.string)
        (D.field "email" D.string)
