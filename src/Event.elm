module Event exposing (Event(..), eventDecoder)

import Global exposing (SessionUser, sessionUserDecoder)
import Json.Decode as D


type Event
    = LoginEvent (Maybe SessionUser)


eventDecoder : D.Decoder Event
eventDecoder =
    D.index 0 D.string
        |> D.andThen
            (\eventName ->
                case eventName of
                    "login" ->
                        D.map LoginEvent (D.index 1 (D.nullable sessionUserDecoder))

                    _ ->
                        D.fail "unknown event name"
            )
