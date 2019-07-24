port module Main exposing (Model, init, main, receiveMessage, sendMessage, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Event exposing (Event(..), eventDecoder)
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
            { navKey = key, session = UnknownLoggedIn }

        initModel =
            Home global
    in
    ( changePage url initModel, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | RegisterMsg Register.Msg
    | LoginMsg Login.Msg
    | LogoutClicked
    | LogoutComplete (Result Http.Error ())
    | ReceivedLogin (Maybe SessionUser)


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


modifyGlobal : (Global -> Global) -> Model -> Model
modifyGlobal f model =
    case model of
        Home global ->
            Home (f global)

        Login loginModel ->
            Login (modifyRecordGlobal f loginModel)

        Register registerModel ->
            Register (modifyRecordGlobal f registerModel)

        NotFound global ->
            NotFound (f global)


modifyRecordGlobal : (Global -> Global) -> { a | global : Global } -> { a | global : Global }
modifyRecordGlobal f model =
    { model | global = f model.global }


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
            ( changePage url model, Cmd.none )

        ( LogoutClicked, _ ) ->
            ( model
            , Http.get
                { url = "/api/logout"
                , expect = Http.expectWhatever LogoutComplete
                }
            )

        ( LogoutComplete _, _ ) ->
            ( model, Nav.reload )

        ( ReceivedLogin (Just user), _ ) ->
            ( modifyGlobal (\global -> { global | session = LoggedIn user }) model, Cmd.none )

        ( ReceivedLogin Nothing, _ ) ->
            ( modifyGlobal (\global -> { global | session = NotLoggedIn }) model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


changePage : Url -> Model -> Model
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
            Home global

        Just Route.Login ->
            Login (Login.init global)

        Just Route.Register ->
            Register (Register.init global)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveMessage
        (\v ->
            case D.decodeValue eventDecoder v of
                Ok (LoginEvent maybeUser) ->
                    ReceivedLogin maybeUser

                Err err ->
                    Debug.log
                        (D.errorToString err)
                        (Debug.todo "handle unrecognized event")
        )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        global =
            getGlobal model
    in
    case model of
        Register registerModel ->
            let
                registerPage =
                    Register.view registerModel
            in
            { title = registerPage.title
            , body =
                [ viewHeader global.session
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
                [ viewHeader global.session
                , Html.map LoginMsg loginPage.content
                ]
            }

        Home _ ->
            { title = "Home"
            , body = [ viewHeader global.session ]
            }

        NotFound _ ->
            { title = "Not found"
            , body =
                [ viewHeader global.session
                , p [] [ text "Page not found" ]
                ]
            }


viewHeader : Session -> Html Msg
viewHeader session =
    let
        navItems =
            [ li [] [ a [ href "/" ] [ text "Home" ] ] ]
                ++ (case session of
                        UnknownLoggedIn ->
                            []

                        LoggedIn _ ->
                            [ li [] [ button [ onClick LogoutClicked ] [ text "Log out" ] ] ]

                        NotLoggedIn ->
                            [ li [] [ a [ href "/login" ] [ text "Login" ] ]
                            , li [] [ a [ href "/register" ] [ text "Register" ] ]
                            ]
                   )
    in
    Html.header []
        [ nav []
            [ ul [] navItems ]
        ]
