module Page.Login exposing (Form, Model, Msg, init, update, view)

import Data.Validation exposing (..)
import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D
import Json.Encode as E


type alias Problem =
    String


type alias Model =
    { global : Global
    , form : Form
    , problems : List Problem
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
            let
                trimmed =
                    trimFields model.form

                validateResult =
                    validateForm trimmed

                (Trimmed updatedForm) =
                    trimmed
            in
            case validateResult of
                Err problems ->
                    ( { model | form = updatedForm, problems = problems }, Cmd.none )

                Ok validForm ->
                    ( { model | form = updatedForm, problems = [] }, postValidForm validForm )

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


postValidForm : Valid TrimmedForm -> Cmd Msg
postValidForm validForm =
    let
        (Trimmed form) =
            fromValid validForm
    in
    Http.post
        { url = "//localhost:8080/api/login"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "email", E.string form.email )
                    , ( "password", E.string form.password )
                    ]
                )
        , expect =
            Http.expectJson CompletedLogin sessionUserDecoder
        }


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
    , problems = []
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
                viewForm model.form model.problems
    }


viewForm : Form -> List Problem -> Html Msg
viewForm form problems =
    Html.form [ onSubmit SubmittedForm ]
        [ ul [] (List.map (\p -> li [] [ text p ]) problems)
        , p []
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


type TrimmedForm
    = Trimmed Form


trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { email = String.trim form.email
        , password = String.trim form.password
        }


isBlank : String -> Bool
isBlank =
    String.trim >> String.isEmpty


validateForm : TrimmedForm -> Result (List Problem) (Valid TrimmedForm)
validateForm (Trimmed form) =
    validate
        (validateEmail form.email
            |> chainAll (validatePassword form.password)
            |> successAs (Trimmed form)
        )


validateEmail : String -> Validation Problem String
validateEmail email =
    ensure (not (isBlank email)) "Email cannot be blank." email


validatePassword : String -> Validation Problem String
validatePassword email =
    ensure (not (isBlank email)) "Password cannot be blank." email
