port module Main exposing (Model, Msg(..), initialModel, main)

import Browser
import Chart exposing (addValueToLabel, colours, hBar, pie, title, toHtml, updateStyles)
import Dict exposing (Dict)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, selected, src, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode
import Theme exposing (dark, light)
import Types exposing (AccountInfo, SkillInfo, Tab(..))
import WOM.API exposing (accountInfo, ehpRates, gains, getTopItems)
import WOM.Data exposing (maxExp)
import WOM.Period exposing (formatPeriod, fromValue, toValue)
import WOM.Types exposing (Boss(..), DeltaObj(..), EHPRates, GainedData, Period(..), Skill(..), TopItems)
import WOM.Utils exposing (getImg, getProgressPercent, ttm)



-- CONSTANTS
-- MESSAGES


type Msg
    = ChangeTheme String
    | DurationPicked String
    | GotGains (Result Http.Error GainedData)
    | GotEHPRates (Result Http.Error EHPRates)
    | GotAccountInfo (Result Http.Error AccountInfo)
    | SwitchTab Tab



-- MODEL


type alias Model =
    { theme : String
    , username : String
    , period : Period
    , gainedData : Maybe GainedData
    , topItems : Maybe TopItems
    , ehpRates : Maybe EHPRates
    , accountInfo : Maybe AccountInfo
    , tab : Tab
    }


locale =
    { usLocale | decimals = FormatNumber.Locales.Exact 2 }


decimalLocale decimals =
    { usLocale | decimals = FormatNumber.Locales.Exact decimals }



-- INIT


initialModel : ( Model, Cmd Msg )
initialModel =
    let
        period =
            Day

        username =
            "Voroth"
    in
    ( { theme = "dark"
      , period = period
      , gainedData = Nothing
      , topItems = Nothing
      , ehpRates = Nothing
      , accountInfo = Nothing
      , tab = Gains
      , username = username
      }
    , gains username period GotGains
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

        currentView =
            case model.tab of
                Gains ->
                    gainsView model

                MaxProgress ->
                    maxProgressView model
    in
    main_
        [ class "h-full p-5", colors ]
        [ menu model, currentView ]


maxProgressStats : Model -> Html msg
maxProgressStats model =
    case ( model.accountInfo, model.ehpRates ) of
        ( Nothing, _ ) ->
            section [] [ h1 [] [ text "Something went wrong" ] ]

        ( _, Nothing ) ->
            section [] [ h1 [] [ text "Something went wrong" ] ]

        ( Just account, Just ehpRates ) ->
            let
                remainingSkills : Dict String SkillInfo
                remainingSkills =
                    Dict.filter (\_ v -> v.level < 99) account.skills

                maxedSkills : Int
                maxedSkills =
                    Dict.filter (\_ v -> v.level == 99) account.skills |> Dict.size

                totalSkills : Int
                totalSkills =
                    Dict.size remainingSkills + maxedSkills

                remainingExp : Int
                remainingExp =
                    let
                        expNeededForMax =
                            Dict.size remainingSkills * maxExp

                        sumExperience : String -> SkillInfo -> Int -> Int
                        sumExperience _ skill acc =
                            acc + skill.experience

                        expInNonMaxedSkills =
                            Dict.foldl sumExperience 0 remainingSkills
                    in
                    expNeededForMax - expInNonMaxedSkills

                percentDone : Float
                percentDone =
                    let
                        expNeededForMax =
                            totalSkills * maxExp
                    in
                    (toFloat (maxedSkills * maxExp) / toFloat expNeededForMax) * 100
            in
            div
                [ class "bg-gray-900"
                ]
                [ div
                    [ class "mx-auto max-w-7xl"
                    ]
                    [ div
                        [ class "grid grid-cols-1 gap-px bg-white/5 sm:grid-cols-2 lg:grid-cols-4"
                        ]
                        [ div
                            [ class "bg-gray-900 px-4 py-6 sm:px-6 lg:px-8"
                            ]
                            [ p
                                [ class "text-sm font-medium leading-6 text-gray-400"
                                ]
                                [ text "Skills left to max" ]
                            , p
                                [ class "mt-2 flex items-baseline gap-x-2"
                                ]
                                [ span
                                    [ class "text-4xl font-semibold tracking-tight text-white"
                                    ]
                                    [ text <| String.fromInt <| Dict.size remainingSkills ]
                                ]
                            ]
                        , div
                            [ class "bg-gray-900 px-4 py-6 sm:px-6 lg:px-8"
                            ]
                            [ p
                                [ class "text-sm font-medium leading-6 text-gray-400"
                                ]
                                [ text "Remaining exp" ]
                            , p
                                [ class "mt-2 flex items-baseline gap-x-2"
                                ]
                                [ span
                                    [ class "text-4xl font-semibold tracking-tight text-white"
                                    ]
                                    [ text <| format (decimalLocale 0) (toFloat remainingExp) ]
                                ]
                            ]
                        , div
                            [ class "bg-gray-900 px-4 py-6 sm:px-6 lg:px-8"
                            ]
                            [ p
                                [ class "text-sm font-medium leading-6 text-gray-400"
                                ]
                                [ text "Time to max" ]
                            , p
                                [ class "mt-2 flex items-baseline gap-x-2"
                                ]
                                [ span
                                    [ class "text-4xl font-semibold tracking-tight text-white"
                                    ]
                                    [ text <| format (decimalLocale 1) account.ttm ]
                                , span
                                    [ class "text-sm text-gray-400"
                                    ]
                                    [ text "hours" ]
                                ]
                            ]
                        , div
                            [ class "bg-gray-900 px-4 py-6 sm:px-6 lg:px-8"
                            ]
                            [ p
                                [ class "text-sm font-medium leading-6 text-gray-400"
                                ]
                                [ text "Percent done" ]
                            , p
                                [ class "mt-2 flex items-baseline gap-x-2"
                                ]
                                [ span
                                    [ class "text-4xl font-semibold tracking-tight text-white"
                                    ]
                                    [ text <| format locale percentDone ++ "%" ]
                                ]
                            ]
                        ]
                    ]
                ]


maxProgressView : Model -> Html Msg
maxProgressView model =
    case ( model.accountInfo, model.ehpRates ) of
        ( Nothing, _ ) ->
            section [] [ h1 [] [ text "Something went wrong" ] ]

        ( _, Nothing ) ->
            section [] [ h1 [] [ text "Something went wrong" ] ]

        ( Just account, Just ehpRates ) ->
            let
                nonMaxSkills =
                    Dict.filter (\_ v -> v.level < 99) account.skills

                ttms =
                    Dict.map (\k v -> ( ttm ehpRates v.name v.experience, k )) nonMaxSkills
                        |> Dict.toList
                        |> List.map Tuple.second
                        |> List.sortBy Tuple.first
                        |> List.map (Tuple.mapFirst (format locale >> String.toFloat >> Maybe.withDefault 0))
                        |> List.reverse

                skillProgress =
                    Dict.map (\k v -> ( getProgressPercent v.experience, k )) nonMaxSkills
                        |> Dict.toList
                        |> List.map Tuple.second
                        |> List.sortBy Tuple.first
                        |> List.map (Tuple.mapFirst (format locale >> String.toFloat >> Maybe.withDefault 0))
                        |> List.reverse

                colors =
                    Dict.fromList
                        [ ( "attack", "rgb(155, 32, 7)" )
                        , ( "defence", "rgb(98, 119, 190)" )
                        , ( "strength", "rgb(4, 149, 90)" )
                        , ( "hitpoints", "rgb(131, 126, 126)" )
                        , ( "ranged", "rgb(109, 144, 23)" )
                        , ( "prayer", "rgb(159, 147, 35)" )
                        , ( "magic", "rgb(50, 80, 193)" )
                        , ( "cooking", "rgb(112, 35, 134)" )
                        , ( "woodcutting", "rgb(52, 140, 37)" )
                        , ( "fletching", "rgb(3, 141, 125)" )
                        , ( "fishing", "rgb(106, 132, 164)" )
                        , ( "firemaking", "rgb(189, 120, 25)" )
                        , ( "crafting", "rgb(151, 110, 77)" )
                        , ( "smithing", "rgb(108, 107, 82)" )
                        , ( "mining", "rgb(93, 143, 167)" )
                        , ( "herblore", "rgb(7, 133, 9)" )
                        , ( "agility", "rgb(58, 60, 137)" )
                        , ( "thieving", "rgb(108, 52, 87)" )
                        , ( "slayer", "rgb(100, 100, 100)" )
                        , ( "farming", "rgb(101, 152, 63)" )
                        , ( "runecraft", "rgb(170, 141, 26)" )
                        , ( "hunter", "rgb(92, 89, 65)" )
                        , ( "construction", "rgb(130, 116, 95)" )
                        ]
            in
            section []
                [ maxProgressStats model
                , pie ttms
                    |> title "Time to max by skill"
                    |> colours (List.map (\t -> Dict.get (Tuple.second t) colors |> Maybe.withDefault "#000") ttms)
                    |> addValueToLabel
                    |> updateStyles "chart-container" [ ( "background-color", dark.bg ) ]
                    |> updateStyles "container" [ ( "background-color", dark.bg ) ]
                    |> toHtml
                , hBar skillProgress
                    |> title "Percentage done by skill"
                    |> colours (List.map (\t -> Dict.get (Tuple.second t) colors |> Maybe.withDefault "#000") ttms)
                    |> updateStyles "chart-container" [ ( "background-color", dark.bg ) ]
                    |> updateStyles "container" [ ( "background-color", dark.bg ) ]
                    |> toHtml
                ]


gainsView : Model -> Html Msg
gainsView model =
    let
        statsDisplay =
            case model.topItems of
                Nothing ->
                    div [] []

                Just items ->
                    stats model.period items
    in
    section []
        [ h1 [ class "text-5xl" ] [ text model.username ]
        , statsDisplay
        ]


menu : Model -> Html Msg
menu m =
    let
        currentStyles =
            "bg-gray-900 text-white"

        baseStyles =
            "rounded-md px-3 py-2 text-sm font-medium"

        defaultStyles =
            "text-gray-300 hover:bg-gray-700 hover:text-white"

        getStyles t =
            if m.tab == t then
                baseStyles ++ currentStyles

            else
                baseStyles ++ defaultStyles
    in
    div
        [ class "flex items-center"
        ]
        [ div
            [ class "hidden sm:ml-6 sm:block"
            ]
            [ div
                [ class "flex space-x-4"
                ]
                [ a
                    [ href "#"
                    , class (getStyles Gains)
                    , onClick <| SwitchTab Gains
                    ]
                    [ text "Gains" ]
                , a
                    [ href "#"
                    , class (getStyles MaxProgress)
                    , onClick <| SwitchTab MaxProgress
                    ]
                    [ text "Max progress" ]
                ]
            ]
        ]


stats : Period -> TopItems -> Html Msg
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
        [ class "text-lg font-medium text-gray-700 leading-6"
        ]
        [ select [ onInput DurationPicked ]
            [ option [ value (toValue Five_min), selected <| period == Five_min ] [ text (formatPeriod Five_min) ]
            , option [ value (toValue Day), selected <| period == Day ] [ text (formatPeriod Day) ]
            , option [ value (toValue Week), selected <| period == Week ] [ text (formatPeriod Week) ]
            , option [ value (toValue Month), selected <| period == Month ] [ text (formatPeriod Month) ]
            , option [ value (toValue Year), selected <| period == Year ] [ text (formatPeriod Year) ]
            ]
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
            [ class "text-sm font-medium text-gray-500 truncate"
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
        DurationPicked s ->
            let
                newPeriod =
                    fromValue s
            in
            ( { model | period = newPeriod }, gains model.username newPeriod GotGains )

        ChangeTheme theme ->
            ( model
            , infoForJS
                { tag = "changeTheme"
                , data =
                    Json.Encode.string theme
                }
            )

        GotGains res ->
            case res of
                Ok data ->
                    let
                        top =
                            getTopItems data
                    in
                    ( { model | topItems = Just top, gainedData = Just data }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotEHPRates res ->
            case res of
                Ok data ->
                    ( { model | ehpRates = Just data }, Cmd.none )

                Err e ->
                    Debug.log "ehpRates Err" e |> (\_ -> ( model, Cmd.none ))

        GotAccountInfo res ->
            case res of
                Ok data ->
                    ( { model | accountInfo = Just data }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SwitchTab tab ->
            ( { model | tab = tab }, fetchDataForTab tab model )


fetchDataForTab : Tab -> Model -> Cmd Msg
fetchDataForTab tab model =
    case tab of
        Gains ->
            gains model.username model.period GotGains

        MaxProgress ->
            Cmd.batch [ accountInfo model.username GotAccountInfo, ehpRates GotEHPRates ]



-- COMMANDS
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- PORTS


type alias GenericJSData =
    { tag : String, data : Json.Encode.Value }


port infoForJS : GenericJSData -> Cmd msg
