module Page.Register exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MODEL


type alias Model =
    { form : Form
    }


type alias Form =
    { email : String
    , username : String
    , password : String
    , repeatPassword : String
    }


init : Model
init =
    { form =
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
    , content = viewForm model.form
    }


viewForm : Form -> Html Msg
viewForm form =
    Html.form []
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
        , button [ class "btn btn-lg btn-primary pull-xs-right" ]
            [ text "Sign up" ]
        ]



-- UPDATE


type Msg
    = EnteredEmail String
    | EnteredUsername String
    | EnteredPassword String
    | EnteredRepeatPassword String


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
