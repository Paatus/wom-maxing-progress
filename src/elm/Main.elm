port module Main exposing (Model, UrlParams, main)

import Accessibility.Key exposing (enter, onKeyDown)
import BarChart
import Browser
import Color
import Dict exposing (Dict)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, selected, type_, value)
import Html.Events exposing (onInput)
import Json.Decode
import Json.Encode
import PieChart
import Process
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import Task
import Types exposing (AccountInfo, Msg(..), Skills)
import Url
import WOM.API exposing (accountInfo, ehpRates, gains)
import WOM.Data exposing (colors)
import WOM.Period exposing (formatPeriod, fromValue, toValue)
import WOM.Types exposing (EHPRates, GainedData, Period(..))
import WOM.Utils exposing (getProgressPercent, hasEhpRate, percentTowardsMax, releasedSkills, ttm)


locale : FormatNumber.Locales.Locale
locale =
    { usLocale | decimals = FormatNumber.Locales.Exact 2 }


decimalLocale : Int -> FormatNumber.Locales.Locale
decimalLocale decimals =
    { usLocale | decimals = FormatNumber.Locales.Exact decimals }



-- CONSTANTS
-- MESSAGES


type alias Model =
    { username : Maybe String
    , period : Period
    , gainedData : Maybe GainedData
    , accountInfo : Maybe AccountInfo
    , ehpRates : Maybe EHPRates
    , currentYear : Int
    }



-- INIT


type alias UrlParams =
    { username : Maybe String }


initialModel : UrlParams -> Int -> ( Model, Cmd Msg )
initialModel params currentYear =
    let
        period : Period
        period =
            WOM.Types.Month

        username : Maybe String
        username =
            params.username

        initModel : Model
        initModel =
            { period = period
            , gainedData = Nothing
            , username = username
            , accountInfo = Nothing
            , ehpRates = Nothing
            , currentYear = currentYear
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
                [ accountInfo name GotAccountInfo
                , ehpRates GotEHPRates
                , gains name m.period GotGains
                ]

        Nothing ->
            Cmd.none



-- MAIN


type alias Flags =
    { currentUrl : String
    , currentYear : Int
    }


defaultFlags : Flags
defaultFlags =
    { currentUrl = "/", currentYear = 0 }


parseUrl : String -> UrlParams
parseUrl urlString =
    let
        url : Maybe Url.Url
        url =
            Url.fromString urlString

        parseParams : Url.Url -> Maybe UrlParams
        parseParams inUrl =
            inUrl.query |> Maybe.map queryToParamsDict |> Maybe.map (\d -> { username = Dict.get "username" d })
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
            Json.Decode.map2 Flags
                (Json.Decode.field "currentUrl" Json.Decode.string)
                (Json.Decode.field "currentYear" Json.Decode.int)

        flags : Flags
        flags =
            Json.Decode.decodeValue decodeFlags flagsValue
                |> Result.withDefault defaultFlags

        params : UrlParams
        params =
            parseUrl flags.currentUrl
    in
    initialModel params flags.currentYear


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW


congratzView : List (Html msg)
congratzView =
    [ div [ class "text-center col-span-2 font-medium text-3xl" ]
        [ text
            "Congratulations, now go do something exciting!"
        ]
    ]


maxProgressView : Model -> Html msg
maxProgressView model =
    case ( model.accountInfo, model.ehpRates ) of
        ( Just account, Just ehpRates ) ->
            let
                nonMaxSkills : Dict String { experience : Int, level : Int, name : String }
                nonMaxSkills =
                    Dict.filter (\_ v -> v.level < 99) account.skills
                        |> Dict.filter (\_ v -> hasEhpRate ehpRates v.name)

                isMaxed : Bool
                isMaxed =
                    Dict.size nonMaxSkills == 0

                contents : List (Html msg)
                contents =
                    if isMaxed then
                        congratzView

                    else
                        let
                            ttms : List ( Float, String )
                            ttms =
                                Dict.map (\k v -> ( ttm ehpRates v.name v.experience, k )) nonMaxSkills
                                    |> Dict.values
                                    |> List.sortBy Tuple.first
                                    |> List.map (Tuple.mapFirst (format locale >> String.toFloat >> Maybe.withDefault 0))
                                    |> List.reverse

                            skillProgress : List ( Float, String )
                            skillProgress =
                                Dict.map (\k v -> ( getProgressPercent v.experience, k )) nonMaxSkills
                                    |> Dict.values
                                    |> List.sortBy Tuple.first
                                    |> List.map (Tuple.mapFirst (format locale >> String.toFloat >> Maybe.withDefault 0))
                                    |> List.reverse

                            ttmChartData : List { value : Float, label : String, color : Color.Color }
                            ttmChartData =
                                List.map (\( val, name ) -> { value = val, label = name, color = Maybe.withDefault Color.white <| Dict.get name colors }) ttms

                            percentDoneChartData : List { value : Float, label : String, color : Color.Color }
                            percentDoneChartData =
                                List.map (\( val, name ) -> { value = val, label = name, color = Maybe.withDefault Color.white <| Dict.get name colors }) skillProgress
                        in
                        [ PieChart.chart ttmChartData
                        , BarChart.chart percentDoneChartData
                        ]
            in
            section []
                [ maxProgressStats model
                , section [ class "grid grid-cols-1 mx-auto max-w-7xl md:grid-cols-2" ]
                    contents
                ]

        _ ->
            section [] []


maxProgressStats : Model -> Html msg
maxProgressStats model =
    case ( model.accountInfo, model.ehpRates ) of
        ( Just account, Just ehpRates ) ->
            let
                allSkills =
                    releasedSkills account.skills

                remainingExp : Int
                remainingExp =
                    WOM.Utils.remainingExp nonMaxSkills

                nonMaxSkills : Dict String { experience : Int, level : Int, name : String }
                nonMaxSkills =
                    Dict.filter (\_ v -> v.level < 99) allSkills

                time_to_max : Float
                time_to_max =
                    Dict.map (\k v -> ( ttm ehpRates v.name v.experience, k )) nonMaxSkills
                        |> Dict.values
                        |> List.foldl (\v acc -> acc + Tuple.first v) 0

                percentDone : Float
                percentDone =
                    percentTowardsMax allSkills

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
                        |> Maybe.map (Dict.filter (\_ v -> v.experience >= 0))

                getRemainingLevels : Dict String { o | level : Int } -> Int
                getRemainingLevels skills =
                    let
                        totalLevel : Int
                        totalLevel =
                            Dict.filter (\k _ -> k == "overall") skills
                                |> Dict.map (\_ v -> v.level)
                                |> Dict.foldl (\_ v acc -> acc + v) 0

                        maxedLevel : Int
                        maxedLevel =
                            Dict.filter (\k _ -> k /= "overall") skills
                                |> Dict.map (\_ _ -> 99)
                                |> Dict.foldl (\_ v acc -> acc + v) 0
                    in
                    maxedLevel - totalLevel

                remainingLevels : Int
                remainingLevels =
                    getRemainingLevels allSkills

                gainedLevels : Int
                gainedLevels =
                    Maybe.map getRemainingLevels thenSkills |> Maybe.map (\n -> remainingLevels - n) |> Maybe.map abs |> Maybe.withDefault 0

                gainedPercent : Float
                gainedPercent =
                    let
                        percentThen : Float
                        percentThen =
                            Maybe.map percentTowardsMax thenSkills |> Maybe.withDefault 0
                    in
                    percentDone - percentThen

                gainedExperience : Int
                gainedExperience =
                    Maybe.map WOM.Utils.remainingExp thenSkills
                        |> Maybe.map (\n -> n - remainingExp)
                        |> Maybe.withDefault 0

                nonMaxedSkillsEHPGained =
                    let
                        skillGainedExp : Dict String Float
                        skillGainedExp =
                            Dict.values nonMaxSkills
                                |> List.map getSkillGainedExp
                                |> Dict.fromList

                        skillEHPRates : Dict String Int
                        skillEHPRates =
                            Dict.values nonMaxSkills
                                |> List.map (\n -> Tuple.pair n.name (WOM.Utils.getEHPRate ehpRates n.name n.experience))
                                |> Dict.fromList

                        mergeExp :
                            Dict String Float
                            -> Dict String Int
                            -> Dict String { name : String, expGained : Float, expRate : Int }
                        mergeExp gainedDict rateDict =
                            Dict.map
                                (\key gainedExp ->
                                    case Dict.get key rateDict of
                                        Just rate ->
                                            { name = key, expGained = gainedExp, expRate = rate }

                                        Nothing ->
                                            -- This shouldn't happen if keys match.
                                            { name = key
                                            , expGained = gainedExp
                                            , expRate = 0
                                            }
                                )
                                gainedDict

                        ehpGained =
                            let
                                merged =
                                    mergeExp skillGainedExp skillEHPRates

                                calcEhp : { name : String, expGained : Float, expRate : Int } -> Float
                                calcEhp rateInfo =
                                    rateInfo.expGained / toFloat rateInfo.expRate
                            in
                            Dict.map (\_ v -> calcEhp v) merged
                                |> Dict.foldl (\_ acc v -> acc + v) 0
                    in
                    ehpGained

                getSkillGainedExp : { experience : Int, level : Int, name : String } -> ( String, Float )
                getSkillGainedExp skill =
                    Maybe.map .skill gained
                        |> Maybe.andThen (Dict.get skill.name)
                        |> Maybe.map .experience
                        |> Maybe.map .gained
                        |> Maybe.withDefault 0
                        |> toFloat
                        |> Tuple.pair skill.name
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
                                [ text ("-" ++ format (decimalLocale 0) (gainedExperience |> toFloat)) ]
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
                                [ text
                                    ("-"
                                        ++ format locale
                                            nonMaxedSkillsEHPGained
                                        ++ " hours"
                                    )
                                ]
                            , dd
                                [ class "w-full flex-none text-3xl font-medium leading-10 tracking-tight text-white"
                                ]
                                [ text <| format (decimalLocale 1) time_to_max
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

        _ ->
            section [] [ h1 [] [] ]


view : Model -> Html Msg
view model =
    main_
        [ class "h-full min-h-screen flex flex-col bg-gray-900 text-gray-300" ]
        [ section
            [ class "flex-1 h-full p-5 flex flex-col" ]
            [ section []
                [ searchView model
                , maxProgressView model
                , newlyReleasedSkillsNoticeView model
                ]
            ]
        , myFooter model
        ]


newlyReleasedSkillsNoticeView : Model -> Html Msg
newlyReleasedSkillsNoticeView model =
    case model.accountInfo of
        Just acc ->
            let
                skillsWithoutExp =
                    acc.skills
                        |> Dict.toList
                        |> List.filter
                            (\n ->
                                Tuple.second n
                                    |> .experience
                                    |> (\a -> a < 0)
                            )

                skillNames =
                    List.map Tuple.first skillsWithoutExp

                hasSkillsWithoutRates =
                    List.length skillsWithoutExp > 0

                noticeMsg : List (Html Msg)
                noticeMsg =
                    List.map (\n -> h1 [] [ text (n ++ " is not yet included, as it is not yet released") ]) skillNames
            in
            if hasSkillsWithoutRates then
                section [ class "w-full items-center text-center pt-12" ] (text "Note:" :: noticeMsg)

            else
                section [] []

        _ ->
            section [] []


periodSelector : { a | period : Period } -> Html Msg
periodSelector model =
    select [ onInput DurationPicked, class "border-0 bg-gray-900 text-gray-300" ]
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


myFooter : Model -> Html msg
myFooter model =
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
                    [ text <| "© " ++ String.fromInt model.currentYear ++ " Pontus Hjortskog. All rights reserved." ]
                ]
            ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitUsername ->
            let
                infoDict : String -> Dict String String
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
                username : Maybe String
                username =
                    if String.length name > 0 then
                        Just name

                    else
                        Nothing
            in
            ( { model | username = username }, Cmd.none )

        DurationPicked s ->
            let
                newPeriod : Period
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
                    ( { model | gainedData = Just data }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotEHPRates res ->
            case res of
                Ok data ->
                    let
                        newModel : Model
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
                        newModel : Model
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
    Sub.none



-- PORTS


type alias GenericJSData =
    { tag : String, data : Json.Encode.Value }


port infoForJS : GenericJSData -> Cmd msg
