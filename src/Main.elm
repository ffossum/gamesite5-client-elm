port module Main exposing (Model, Msg(..), init, main, receiveMessage, sendMessage, subscriptions, update, view, viewLink)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import Page.Register as Register
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , receivedMessages : List String
    , registerModel : Register.Model
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        registerModel =
            Register.init
    in
    ( { key = key
      , url = url
      , receivedMessages = []
      , registerModel = registerModel
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | MessageReceived String
    | SendMessageClicked
    | RegisterMsg Register.Msg


port sendMessage : E.Value -> Cmd msg


port receiveMessage : (D.Value -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RegisterMsg registerMsg ->
            let
                ( updatedModel, registerCmd ) =
                    Register.update registerMsg model.registerModel
            in
            ( { model
                | registerModel =
                    updatedModel
              }
            , Cmd.map RegisterMsg registerCmd
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        SendMessageClicked ->
            ( model, sendMessage (E.string "Hello") )

        MessageReceived m ->
            ( { model | receivedMessages = m :: model.receivedMessages }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveMessage
        (\v ->
            case D.decodeValue D.string v of
                Ok str ->
                    MessageReceived str

                _ ->
                    MessageReceived "Error parsing message"
        )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        registerPage =
            Register.view model.registerModel
    in
    { title = registerPage.title
    , body =
        [ text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , ul []
            [ viewLink "/home"
            , viewLink "/profile"
            , viewLink "/reviews/the-century-of-the-self"
            , viewLink "/reviews/public-opinion"
            , viewLink "/reviews/shah-of-shahs"
            ]
        , button [ onClick SendMessageClicked ] [ text "Send message!" ]
        , ul [] (List.map (\msg -> li [] [ text msg ]) model.receivedMessages)
        , Html.map RegisterMsg registerPage.content
        ]
    }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
