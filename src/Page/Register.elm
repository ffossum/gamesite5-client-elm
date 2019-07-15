module Page.Register exposing (Model, Msg(..), init, update, view)

import Global exposing (Global, Session(..), SessionUser)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E



-- MODEL


type alias Model =
    { global : Global
    , form : Form
    }


type alias Form =
    { email : String
    , username : String
    , password : String
    , repeatPassword : String
    }


init : Global -> Model
init global =
    { global = global
    , form =
        { email = ""
        , username = ""
        , password = ""
        , repeatPassword = ""
        }
    }



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Register"
    , content =
        case model.global.session of
            NotLoggedIn ->
                viewForm model.form

            LoggedIn user ->
                text "You are logged in"
    }


viewForm : Form -> Html Msg
viewForm form =
    Html.form [ onSubmit SubmittedForm ]
        [ p []
            [ label
                [ for "register_username" ]
                [ text "Username" ]
            , input
                [ id "register_username"
                , placeholder "Username"
                , onInput EnteredUsername
                , value form.username
                ]
                []
            ]
        , p []
            [ label [ for "register_email" ] [ text "Email" ]
            , input
                [ id "register_email"
                , placeholder "Email"
                , onInput EnteredEmail
                , value form.email
                ]
                []
            ]
        , p []
            [ label [ for "register_password" ] [ text "Password" ]
            , input
                [ type_ "password"
                , id "register_password"
                , placeholder "Password"
                , onInput EnteredPassword
                , value form.password
                ]
                []
            , label [ for "register_repeat_password" ] [ text "Repeat password" ]
            , input
                [ type_ "password"
                , id "register_repeat_password"
                , placeholder "RepeatPassword"
                , onInput EnteredRepeatPassword
                , value form.repeatPassword
                ]
                []
            ]
        , button [] [ text "Sign up" ]
        ]



-- UPDATE


type Msg
    = EnteredEmail String
    | EnteredUsername String
    | EnteredPassword String
    | EnteredRepeatPassword String
    | SubmittedForm
    | CompletedRegister (Result Http.Error SessionUser)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredUsername username ->
            updateForm (\form -> { form | username = username }) model

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        EnteredRepeatPassword repeatPassword ->
            updateForm (\form -> { form | repeatPassword = repeatPassword }) model

        SubmittedForm ->
            ( model
            , Http.post
                { url = "//localhost:8080/users"
                , body =
                    Http.jsonBody
                        (encodeCreateUser
                            { name = model.form.username
                            , email = model.form.email
                            , password = model.form.password
                            }
                        )
                , expect = Http.expectJson CompletedRegister sessionUserDecoder
                }
            )

        CompletedRegister (Ok user) ->
            ( updateGlobal (\global -> { global | session = LoggedIn user }) model, Cmd.none )

        CompletedRegister (Err _) ->
            ( model, Cmd.none )


updateGlobal : (Global -> Global) -> { a | global : Global } -> { a | global : Global }
updateGlobal f model =
    { model | global = f model.global }


type alias CreateUser =
    { name : String
    , email : String
    , password : String
    }


encodeCreateUser : CreateUser -> E.Value
encodeCreateUser u =
    E.object
        [ ( "name", E.string u.name )
        , ( "email", E.string u.email )
        , ( "password", E.string u.password )
        ]


sessionUserDecoder : D.Decoder SessionUser
sessionUserDecoder =
    D.map3 SessionUser
        (D.at [ "id" ] D.int)
        (D.at [ "name" ] D.string)
        (D.at [ "email" ] D.string)


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- FORM


{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm
    = Trimmed Form


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { username = String.trim form.username
        , email = String.trim form.email
        , password = String.trim form.password
        , repeatPassword = String.trim form.repeatPassword
        }
