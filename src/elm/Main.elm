port module Main exposing (Model, Msg(..), initialModel, main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList, src)
import Http
import Theme exposing (dark, light)
import WOM.API exposing (gains)
import WOM.Period exposing (formatPeriod)
import WOM.Types exposing (Boss(..), DeltaObj(..), Period(..), TopItems)
import WOM.Utils exposing (getImg)



-- CONSTANTS
-- MESSAGES


type Msg
    = ChangeTheme String
    | GotGains (Result Http.Error TopItems)



-- MODEL


type alias Model =
    { theme : String
    , period : Period
    , topItems : Maybe TopItems
    }



-- INIT


initialModel : ( Model, Cmd Msg )
initialModel =
    let
        period =
            Day
    in
    ( { theme = "dark"
      , period = period
      , topItems = Nothing
      }
    , gains "Voroth" period GotGains
    )



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW


view : Model -> Html Msg
view model =
    let
        colors =
            classList
                [ ( dark.bg, model.theme == "dark" )
                , ( dark.text, model.theme == "dark" )
                , ( light.bg, model.theme == "light" )
                , ( light.text, model.theme == "light" )
                ]

        statsDisplay =
            case model.topItems of
                Nothing ->
                    div [] []

                Just items ->
                    stats model.period items
    in
    main_
        [ class "h-full p-5", colors ]
        [ h1 [ class "text-5xl" ] [ text "Voroth" ]
        , statsDisplay
        ]


stats : Period -> TopItems -> Html msg
stats period topItems =
    let
        topActivity =
            topItems.topActivity

        topSkill =
            topItems.topSkill

        topBoss =
            topItems.topBoss
    in
    div
        [ class "text-lg font-medium leading-6 text-gray-700"
        ]
        [ text (formatPeriod period)
        , dl
            [ class "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-2"
            ]
            [ card "Gained experience" topItems.gainedExperience
            , card "Top boss" topBoss
            , card "Top skill" topSkill
            , card "Top activity" topActivity
            ]
        ]


card : String -> Maybe DeltaObj -> Html msg
card title item =
    let
        bg =
            "bg-gray-800"

        stat_text =
            "text-gray-400"

        card_styles =
            classList [ ( bg, True ), ( stat_text, True ), ( "capitalize", True ) ]

        name =
            case item of
                Just (SkillDelta a) ->
                    case a.name of
                        "overall" ->
                            String.fromInt a.experience.gained

                        n ->
                            n

                Just (ActivityDelta a) ->
                    a.name

                Just (BossDelta a) ->
                    a.name

                Nothing ->
                    "N/A"

        imageUrl =
            case String.toInt name of
                Nothing ->
                    getImg name

                Just _ ->
                    getImg "overall"

        hasImage =
            case imageUrl of
                Just _ ->
                    True

                Nothing ->
                    False

        gainedDiff =
            case item of
                Just (SkillDelta s) ->
                    " +" ++ String.fromInt s.experience.gained ++ " exp"

                Just (BossDelta s) ->
                    " +" ++ String.fromInt s.kills.gained ++ " kills"

                Just (ActivityDelta s) ->
                    " +" ++ String.fromInt s.score.gained

                Nothing ->
                    ""

        formatItem =
            String.split "_" >> String.join " "
    in
    div
        [ class "px-4 py-5 overflow-hidden rounded-lg shadow sm:p-6"
        , card_styles
        ]
        [ dt
            [ class "truncate text-sm font-medium text-gray-500"
            ]
            [ text title, text gainedDiff ]
        , dd
            [ class "flex items-center mt-1 text-3xl font-semibold tracking-tight"
            ]
            [ img
                [ src (imageUrl |> Maybe.withDefault "")
                , class "w-5 h-5 mr-2"
                , classList [ ( "hidden", not hasImage ) ]
                ]
                []
            , text (formatItem name)
            ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTheme theme ->
            ( model, changeTheme theme )

        GotGains res ->
            case res of
                Ok data ->
                    ( { model | topItems = Just data }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- COMMANDS
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- PORTS


port changeTheme : String -> Cmd msg
