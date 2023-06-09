port module Main exposing (Model, initialModel, main)

import Accessibility.Key exposing (enter, onKeyDown)
import BarChart
import Browser
import Color
import Dict exposing (Dict)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, id, name, placeholder, selected, type_, value)
import Html.Events exposing (onInput)
import Json.Decode
import Json.Encode
import PieChart
import Process
import Svg as Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import Task
import Theme exposing (dark, light)
import Types exposing (AccountInfo, Msg(..), SkillInfo, Skills)
import Url
import WOM.API exposing (accountInfo, ehpRates, gains, getTopItems)
import WOM.Data exposing (colors, colours, maxExp)
import WOM.Period exposing (formatPeriod, fromValue, toValue)
import WOM.Types exposing (Boss(..), DeltaObj(..), EHPRates, GainedData, Period(..), Skill(..), TopItems)
import WOM.Utils exposing (getProgressPercent, percentTowardsMax, remainingExp, ttm)


locale : FormatNumber.Locales.Locale
locale =
    { usLocale | decimals = FormatNumber.Locales.Exact 2 }


decimalLocale : Int -> FormatNumber.Locales.Locale
decimalLocale decimals =
    { usLocale | decimals = FormatNumber.Locales.Exact decimals }



-- CONSTANTS
-- MESSAGES


type alias Model =
    { theme : String
    , username : Maybe String
    , period : Period
    , gainedData : Maybe GainedData
    , topItems : Maybe TopItems
    , accountInfo : Maybe AccountInfo
    , ehpRates : Maybe EHPRates
    }



-- INIT


type alias UrlParams =
    { username : Maybe String }


initialModel : UrlParams -> ( Model, Cmd Msg )
initialModel params =
    let
        period =
            WOM.Types.Month

        username =
            params.username

        initModel : Model
        initModel =
            { theme = "dark"
            , period = period
            , gainedData = Nothing
            , topItems = Nothing
            , username = username
            , accountInfo = Nothing
            , ehpRates = Nothing
            }

        t : Task.Task x Model
        t =
            Process.sleep 500 |> Task.andThen (always <| Task.succeed <| initModel)
    in
    ( initModel, Task.perform (always SubmitUsername) t )


searchIfUsername : Model -> Cmd Msg
searchIfUsername m =
    case m.username of
        Just name ->
            Cmd.batch
                [ accountInfo (Debug.log "name" name) GotAccountInfo
                , ehpRates GotEHPRates
                , gains name m.period GotGains
                ]

        Nothing ->
            Cmd.none



-- MAIN


type alias Flags =
    { currentUrl : String
    }


defaultFlags : Flags
defaultFlags =
    { currentUrl = "/" }


parseUrl : String -> UrlParams
parseUrl urlString =
    let
        url : Maybe Url.Url
        url =
            Url.fromString urlString

        parseParams : Url.Url -> Maybe UrlParams
        parseParams inUrl =
            inUrl.query |> Maybe.map queryToParamsDict |> Maybe.andThen (\d -> Just { username = Dict.get "username" d })
    in
    Maybe.andThen parseParams url |> Maybe.withDefault { username = Nothing }


queryToParamsDict : String -> Dict.Dict String String
queryToParamsDict query =
    query
        |> String.split "&"
        |> List.filterMap
            (\part ->
                case String.split "=" part of
                    [ key, value ] ->
                        Just ( key, Maybe.withDefault value (Url.percentDecode (String.replace "+" " " value)) )

                    _ ->
                        Nothing
            )
        |> Dict.fromList


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flagsValue =
    let
        decodeFlags : Json.Decode.Decoder Flags
        decodeFlags =
            Json.Decode.map Flags <|
                Json.Decode.field "currentUrl" Json.Decode.string

        flags : Flags
        flags =
            Json.Decode.decodeValue decodeFlags flagsValue
                |> Result.withDefault defaultFlags

        params =
            parseUrl flags.currentUrl
    in
    initialModel params


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW


maxProgressView : Model -> Html msg
maxProgressView model =
    case ( model.accountInfo, model.ehpRates ) of
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

                ttmChartData =
                    List.map (\( val, name ) -> { value = val, label = name, color = Maybe.withDefault Color.white <| Dict.get name colours }) ttms

                percentDoneChartData =
                    List.map (\( val, name ) -> { value = val, label = name, color = Maybe.withDefault Color.white <| Dict.get name colours }) skillProgress
            in
            section []
                [ maxProgressStats model
                , section [ class "grid grid-cols-2 mx-auto max-w-7xl" ]
                    [ PieChart.chart ttmChartData
                    , BarChart.chart percentDoneChartData
                    ]
                ]

        ( _, _ ) ->
            section [] []


maxProgressStats : Model -> Html msg
maxProgressStats model =
    case model.accountInfo of
        Nothing ->
            section [] [ h1 [] [] ]

        Just account ->
            let
                remainingExp =
                    WOM.Utils.remainingExp account.skills

                remainingSkills : Dict String SkillInfo
                remainingSkills =
                    Dict.filter (\_ v -> v.level < 99) account.skills

                percentDone : Float
                percentDone =
                    percentTowardsMax account.skills

                getSkillExp : DeltaObj -> Maybe Int
                getSkillExp do =
                    case do of
                        SkillDelta sd ->
                            Just sd.experience.gained

                        _ ->
                            Nothing

                gained : Maybe GainedData
                gained =
                    model.gainedData

                thenSkills : Maybe Skills
                thenSkills =
                    Maybe.map .skill gained
                        |> Maybe.map
                            (Dict.map
                                (\k val ->
                                    { experience = val.experience.start
                                    , level = val.level.start
                                    , name = k
                                    }
                                )
                            )

                getRemainingLevels : Dict String { o | level : Int } -> Int
                getRemainingLevels skills =
                    let
                        totalLevel =
                            Dict.filter (\k v -> k == "overall") skills
                                |> Dict.map (\_ v -> v.level)
                                |> Dict.foldl (\_ v acc -> acc + v) 0
                    in
                    2277 - totalLevel

                remainingLevels : Int
                remainingLevels =
                    getRemainingLevels account.skills

                gainedLevels : Int
                gainedLevels =
                    Maybe.map getRemainingLevels thenSkills |> Maybe.map (\n -> remainingLevels - n) |> Maybe.map abs |> Maybe.withDefault 0

                gainedMaxedSkills : Int
                gainedMaxedSkills =
                    let
                        skillsThen =
                            Maybe.map (Dict.filter (\_ v -> v.level < 99) >> Dict.size) thenSkills |> Maybe.withDefault 0
                    in
                    skillsThen - Dict.size remainingSkills

                gainedPercent : Float
                gainedPercent =
                    let
                        percentThen =
                            Maybe.map percentTowardsMax thenSkills |> Maybe.withDefault 0

                        _ =
                            Debug.log "thenSkills" thenSkills |> (\_ -> Debug.log "nowSkills" account.skills)
                    in
                    percentDone - percentThen

                gainedExperience2 : Int
                gainedExperience2 =
                    Maybe.map WOM.Utils.remainingExp thenSkills
                        |> Maybe.map (\n -> n - remainingExp)
                        |> Maybe.withDefault 0

                gainedExperience : Maybe Int
                gainedExperience =
                    Maybe.map getTopItems gained
                        |> Maybe.andThen .gainedExperience
                        |> Maybe.andThen getSkillExp

                gainedEHP : Float
                gainedEHP =
                    Maybe.map .computed gained |> Maybe.andThen (Dict.get "ehp") |> Maybe.map .gained |> Maybe.withDefault 0
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
                            [ class "flex flex-wrap items-baseline justify-between gap-x-4 bg-gray-900 gap-y-2 px-4 py-10 sm:px-6 xl:px-8"
                            ]
                            [ dt
                                [ class "text-sm font-medium leading-6 text-gray-400"
                                ]
                                [ text "Remaining levels" ]
                            , dd
                                [ class "text-xs font-medium text-green-700"
                                ]
                                [ text ("-" ++ String.fromInt gainedLevels) ]
                            , dd
                                [ class "w-full flex-none text-3xl font-medium leading-10 tracking-tight text-white"
                                ]
                                [ text <| String.fromInt <| remainingLevels ]
                            ]
                        , div
                            [ class "flex flex-wrap items-baseline justify-between gap-x-4 bg-gray-900 gap-y-2 px-4 py-10 sm:px-6 xl:px-8"
                            ]
                            [ dt
                                [ class "text-sm font-medium leading-6 text-gray-400"
                                ]
                                [ text "Remaining exp" ]
                            , dd
                                [ class "text-xs font-medium text-green-700"
                                ]
                                [ text ("-" ++ format (decimalLocale 0) (gainedExperience2 |> toFloat)) ]
                            , dd
                                [ class "w-full flex-none text-3xl font-medium leading-10 tracking-tight text-white"
                                ]
                                [ text <| format (decimalLocale 0) (toFloat remainingExp) ]
                            ]
                        , div
                            [ class "flex flex-wrap items-baseline justify-between gap-x-4 bg-gray-900 gap-y-2 px-4 py-10 sm:px-6 xl:px-8"
                            ]
                            [ dt
                                [ class "text-sm font-medium leading-6 text-gray-400"
                                ]
                                [ text "Time to max" ]
                            , dd
                                [ class "text-xs font-medium text-green-700"
                                ]
                                [ text ("-" ++ format locale gainedEHP ++ " hours") ]
                            , dd
                                [ class "w-full flex-none text-3xl font-medium leading-10 tracking-tight text-white"
                                ]
                                [ text <| format (decimalLocale 1) account.ttm
                                , span
                                    [ class "ml-1 text-sm text-gray-400"
                                    ]
                                    [ text "hours" ]
                                ]
                            ]
                        , div
                            [ class "flex flex-wrap items-baseline justify-between gap-x-4 bg-gray-900 gap-y-2 px-4 py-10 sm:px-6 xl:px-8"
                            ]
                            [ dt
                                [ class "text-sm font-medium leading-6 text-gray-400"
                                ]
                                [ text "Percent done" ]
                            , dd
                                [ class "text-xs font-medium text-green-700"
                                ]
                                [ text <| "+" ++ format locale gainedPercent ++ "%" ]
                            , dd
                                [ class "w-full flex-none text-3xl font-medium leading-10 tracking-tight text-white"
                                ]
                                [ text <| format locale percentDone ++ "%" ]
                            ]
                        ]
                    ]
                ]


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
            section [] [ searchView model, maxProgressView model ]
    in
    main_
        [ class "h-full min-h-screen flex flex-col", colors ]
        [ section
            [ class "flex-1 h-full p-5 flex flex-col" ]
            [ currentView ]
        , myFooter
        ]


periodSelector : { a | period : Period } -> Html Msg
periodSelector model =
    select [ onInput DurationPicked, class "border-0", classList [ ( dark.bg, True ) ] ]
        [ option [ value (toValue Day), selected <| model.period == Day ] [ text (formatPeriod Day) ]
        , option [ value (toValue Week), selected <| model.period == Week ] [ text (formatPeriod Week) ]
        , option [ value (toValue Month), selected <| model.period == Month ] [ text (formatPeriod Month) ]
        , option [ value (toValue Year), selected <| model.period == Year ] [ text (formatPeriod Year) ]
        ]


searchView : Model -> Html Msg
searchView model =
    section [ class "flex" ]
        [ div
            [ class "max-w-md mx-auto flex"
            ]
            [ periodSelector model
            , div
                [ class "ml-2 relative flex items-center w-full h-12 rounded-lg focus-within:shadow-lg bg-gray-800 overflow-hidden"
                ]
                [ div
                    [ class "grid place-items-center h-full w-12 text-gray-300"
                    ]
                    [ svg
                        [ SvgAttr.class "h-6 w-6"
                        , SvgAttr.fill "none"
                        , SvgAttr.viewBox "0 0 24 24"
                        , SvgAttr.stroke "currentColor"
                        ]
                        [ path
                            [ SvgAttr.strokeLinecap "round"
                            , SvgAttr.strokeLinejoin "round"
                            , SvgAttr.strokeWidth "2"
                            , SvgAttr.d "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"
                            ]
                            []
                        ]
                    ]
                , input
                    [ class "peer h-full w-full outline-none text-sm text-gray-300 pr-2 bg-gray-800 border-0"
                    , type_ "text"
                    , placeholder "Search for a username.."
                    , onInput (\str -> SearchFieldChanged str)
                    , onKeyDown [ enter SubmitUsername ]
                    , value <| Maybe.withDefault "" model.username
                    ]
                    []
                ]
            ]
        ]


myFooter : Html msg
myFooter =
    footer
        [ class "bg-gray-800 flex-none"
        ]
        [ div
            [ class "mx-auto max-w-7xl px-6 py-12 md:flex md:items-center md:justify-between lg:px-8"
            ]
            [ div
                [ class "flex justify-center space-x-6 md:order-2"
                ]
                [ p
                    [ class "text-center text-xs leading-5 text-gray-500"
                    ]
                    [ text "Uses data from ", a [ class "text-gray-400", href "https://wiseoldman.net/" ] [ text "wiseoldman.org" ] ]
                ]
            , div
                [ class "mt-8 md:order-1 md:mt-0"
                ]
                [ p
                    [ class "text-center text-xs leading-5 text-gray-500"
                    ]
                    [ text "© 2023 Pontus Hjortskog. All rights reserved." ]
                ]
            ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged newUrl ->
            let
                _ =
                    Debug.log "newUrl:" newUrl
            in
            ( model, Cmd.none )

        SubmitUsername ->
            let
                infoDict name =
                    Dict.fromList [ ( "key", "username" ), ( "value", name ) ]
            in
            case model.username of
                Just name ->
                    ( model
                    , Cmd.batch
                        [ infoForJS
                            { tag = "setUrlParam"
                            , data = Json.Encode.dict identity Json.Encode.string (infoDict name)
                            }
                        , searchIfUsername model
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        SearchFieldChanged name ->
            let
                username =
                    if String.length name > 0 then
                        Just name

                    else
                        Nothing
            in
            ( { model | username = username }, Cmd.none )

        DurationPicked s ->
            let
                newPeriod =
                    fromValue s
            in
            ( { model | period = newPeriod }
            , case model.username of
                Just name ->
                    gains name newPeriod GotGains

                Nothing ->
                    Cmd.none
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
                    let
                        newModel =
                            { model | ehpRates = Just data }
                    in
                    ( newModel, Cmd.none )

                Err _ ->
                    ( { model | ehpRates = Nothing }, Cmd.none )

        GotAccountInfo res ->
            case res of
                Ok data ->
                    let
                        newModel =
                            { model | accountInfo = Just data }
                    in
                    ( newModel, Cmd.none )

                Err _ ->
                    ( { model | accountInfo = Nothing }, Cmd.none )



-- COMMANDS
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ urlChanged UrlChanged ]



-- PORTS


type alias GenericJSData =
    { tag : String, data : Json.Encode.Value }


port infoForJS : GenericJSData -> Cmd msg


port urlChanged : (String -> msg) -> Sub msg
