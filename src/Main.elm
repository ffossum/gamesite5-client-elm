port module Main exposing (Model, init, main, receiveMessage, sendMessage, subscriptions, update, view, viewLink)

import Browser
import Browser.Navigation as Nav
import Global exposing (..)
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


type Model
    = Home Global
    | Register Register.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        global =
            { navKey = key, session = NotLoggedIn }

        registerModel =
            Register.init global
    in
    ( Register registerModel, Cmd.none )



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
            ( model
            , Cmd.none
            )

        ( SendMessageClicked, _ ) ->
            ( model, sendMessage (E.string "Hello") )

        ( MessageReceived m, _ ) ->
            ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )



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
                [ Html.map RegisterMsg registerPage.content
                ]
            }

        Home global ->
            { title = "Home"
            , body = [ div [] [ text "Home" ] ]
            }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
