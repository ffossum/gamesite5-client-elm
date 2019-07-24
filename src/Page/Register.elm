module Page.Register exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Data.Validation as V exposing (Valid, Validation(..))
import Global exposing (Global, Session(..), SessionUser, sessionUserDecoder)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E



-- MODEL


type alias Problem =
    String


type alias Model =
    { global : Global
    , form : Form
    , problems : List Problem
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
    , problems = []
    }



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Register"
    , content =
        case model.global.session of
            UnknownLoggedIn ->
                text ""

            NotLoggedIn ->
                viewForm model.form model.problems

            LoggedIn user ->
                text "You are logged in."
    }


viewForm : Form -> List String -> Html Msg
viewForm form problems =
    Html.form [ onSubmit SubmittedForm ]
        [ ul [] (List.map (\p -> li [] [ text p ]) problems)
        , p []
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
            let
                trimmedForm =
                    trimFields model.form

                validateResult =
                    validateForm trimmedForm
            in
            case validateResult of
                Ok validatedForm ->
                    let
                        (Trimmed form) =
                            V.fromValid validatedForm
                    in
                    ( { model | form = form, problems = [] }
                    , postValidForm validatedForm
                    )

                Err problems ->
                    let
                        (Trimmed form) =
                            trimmedForm
                    in
                    ( { model | form = form, problems = problems }, Cmd.none )

        CompletedRegister (Ok user) ->
            ( updateGlobal (\global -> { global | session = LoggedIn user }) model, Nav.reload )

        CompletedRegister (Err _) ->
            ( model, Cmd.none )


postValidForm : Valid TrimmedForm -> Cmd Msg
postValidForm validForm =
    let
        (Trimmed form) =
            V.fromValid validForm
    in
    Http.post
        { url = "//localhost:8080/api/register"
        , body =
            Http.jsonBody
                (encodeCreateUser
                    { name = form.username
                    , email = form.email
                    , password = form.password
                    }
                )
        , expect = Http.expectJson CompletedRegister sessionUserDecoder
        }


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


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- FORM


type TrimmedForm
    = Trimmed Form


trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { username = String.trim form.username
        , email = String.trim form.email
        , password = String.trim form.password
        , repeatPassword = String.trim form.repeatPassword
        }


validateForm : TrimmedForm -> Result (List Problem) (Valid TrimmedForm)
validateForm (Trimmed form) =
    V.validate
        (validateUsername form.username
            |> V.chainAll (validateEmail form.email)
            |> V.chainAll (validatePassword form.password form.repeatPassword)
            |> V.successAs (Trimmed form)
        )


isBlank : String -> Bool
isBlank =
    String.trim >> String.isEmpty


validateUsername : String -> Validation Problem String
validateUsername name =
    V.ensure (not (isBlank name)) "Username cannot be blank." name


validateEmail : String -> Validation Problem String
validateEmail email =
    V.ensure (not (isBlank email)) "Email cannot be blank." email


validatePassword : String -> String -> Validation Problem String
validatePassword password repeatPassword =
    let
        minLength =
            8

        lengthFailureDesc =
            "Password must be at least " ++ String.fromInt minLength ++ " characters long."
    in
    V.ensure (String.length password >= minLength) lengthFailureDesc password
        |> V.chainFirst (V.ensure (password == repeatPassword) "Passwords do not match." repeatPassword)
