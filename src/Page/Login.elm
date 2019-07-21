module Page.Login exposing (Form, Model, Msg, init, update, view)

import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D
import Json.Encode as E


type alias Model =
    { global : Global
    , form : Form
    }


type alias Form =
    { email : String
    , password : String
    }


type Msg
    = EnteredEmail String
    | EnteredPassword String
    | SubmittedForm
    | CompletedLogin (Result Http.Error SessionUser)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        SubmittedForm ->
            ( model
            , Http.post
                { url = "//localhost:8080/api/login"
                , body =
                    Http.jsonBody
                        (E.object
                            [ ( "email", E.string model.form.email )
                            , ( "password", E.string model.form.password )
                            ]
                        )
                , expect =
                    Http.expectJson CompletedLogin sessionUserDecoder
                }
            )

        CompletedLogin (Err _) ->
            ( model, Cmd.none )

        CompletedLogin (Ok user) ->
            let
                global =
                    model.global

                updatedGlobal =
                    { global | session = LoggedIn user }
            in
            ( { model | global = updatedGlobal }, Cmd.none )


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )


init : Global -> Model
init global =
    { global = global
    , form = { email = "", password = "" }
    }


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content =
        case model.global.session of
            UnknownLoggedIn ->
                text ""

            LoggedIn user ->
                text "You are logged in."

            NotLoggedIn ->
                viewForm model.form
    }


viewForm : Form -> Html Msg
viewForm form =
    Html.form [ onSubmit SubmittedForm ]
        [ p []
            [ label
                [ for "login_email" ]
                [ text "Email" ]
            , input
                [ id "login_email"
                , placeholder "Email"
                , onInput EnteredEmail
                , value form.email
                ]
                []
            ]
        , p []
            [ label
                [ for "login_password" ]
                [ text "Password" ]
            , input
                [ id "login_password"
                , placeholder "Password"
                , onInput EnteredPassword
                , value form.password
                ]
                []
            ]
        , button [] [ text "Log in" ]
        ]
