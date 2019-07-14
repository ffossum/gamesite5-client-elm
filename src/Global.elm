module Global exposing (Global, Session(..), SessionUser)

import Browser.Navigation as Nav


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
