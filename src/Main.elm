port module Main exposing (Model, init, main, receiveMessage, sendMessage, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (href, target)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode as D
import Json.Encode as E
import Page.Login as Login
import Page.Register as Register
import Route as Route exposing (Route)
import Url exposing (Url)



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
    | Login Login.Model
    | Register Register.Model
    | NotFound Global


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
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
    | UrlChanged Url
    | MessageReceived String
    | SendMessageClicked
    | RegisterMsg Register.Msg
    | LoginMsg Login.Msg
    | LogoutClicked
    | LogoutComplete (Result Http.Error ())


port sendMessage : E.Value -> Cmd msg


port receiveMessage : (D.Value -> msg) -> Sub msg


getGlobal : Model -> Global
getGlobal model =
    case model of
        Home global ->
            global

        Login loginModel ->
            loginModel.global

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

        ( LoginMsg loginMsg, Login loginModel ) ->
            let
                ( updatedModel, loginCmd ) =
                    Login.update loginMsg loginModel
            in
            ( Login updatedModel
            , Cmd.map LoginMsg loginCmd
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

        ( LogoutClicked, _ ) ->
            ( model
            , Http.get
                { url = "/api/logout"
                , expect = Http.expectWhatever LogoutComplete
                }
            )

        ( LogoutComplete _, _ ) ->
            ( model, Nav.reload )

        ( _, _ ) ->
            ( model, Cmd.none )


changePage : Url -> Model -> ( Model, Cmd Msg )
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

        Just Route.Login ->
            ( Login (Login.init global), Cmd.none )

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

        Login loginModel ->
            let
                loginPage =
                    Login.view loginModel
            in
            { title = loginPage.title
            , body =
                [ viewLinks
                , Html.map LoginMsg loginPage.content
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


viewLinks : Html Msg
viewLinks =
    ul []
        [ li [] [ a [ href "/" ] [ text "Home" ] ]
        , li [] [ a [ href "/login" ] [ text "Login" ] ]
        , li [] [ a [ href "/register" ] [ text "Register" ] ]
        , li [] [ button [ onClick LogoutClicked ] [ text "Log out" ] ]
        ]
