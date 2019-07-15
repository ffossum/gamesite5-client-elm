port module Main exposing (Model, init, main, receiveMessage, sendMessage, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (href, target)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import Page.Register as Register
import Route as Route exposing (Route)
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


type Model
    = Home Global
    | Register Register.Model
    | NotFound Global


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        global =
            { navKey = key, session = NotLoggedIn }

        initModel =
            Home global
    in
    changePage url initModel



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | MessageReceived String
    | SendMessageClicked
    | RegisterMsg Register.Msg


port sendMessage : E.Value -> Cmd msg


port receiveMessage : (D.Value -> msg) -> Sub msg


getGlobal : Model -> Global
getGlobal model =
    case model of
        Home global ->
            global

        Register registerModel ->
            registerModel.global

        NotFound global ->
            global


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( RegisterMsg registerMsg, Register registerModel ) ->
            let
                ( updatedModel, registerCmd ) =
                    Register.update registerMsg registerModel
            in
            ( Register updatedModel
            , Cmd.map RegisterMsg registerCmd
            )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (getGlobal model).navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changePage url model

        ( SendMessageClicked, _ ) ->
            ( model, sendMessage (E.string "Hello") )

        ( MessageReceived m, _ ) ->
            ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


changePage : Url.Url -> Model -> ( Model, Cmd Msg )
changePage url model =
    let
        maybeRoute =
            Route.fromUrl url

        global =
            getGlobal model
    in
    case maybeRoute of
        Nothing ->
            Debug.todo "not found page"

        Just Route.Home ->
            ( Home global, Cmd.none )

        Just Route.Register ->
            ( Register (Register.init global), Cmd.none )



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
    case model of
        Register registerModel ->
            let
                registerPage =
                    Register.view registerModel
            in
            { title = registerPage.title
            , body =
                [ viewLinks
                , Html.map RegisterMsg registerPage.content
                ]
            }

        Home global ->
            { title = "Home"
            , body = [ viewLinks ]
            }

        NotFound global ->
            { title = "Not found"
            , body = [ text "Not found" ]
            }


viewLinks : Html msg
viewLinks =
    ul []
        [ li [] [ a [ href "/" ] [ text "Home" ] ]
        , li [] [ a [ href "/register" ] [ text "Register" ] ]
        ]
