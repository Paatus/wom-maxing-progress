module WOM.API exposing
    ( accountInfo
    , ehpRates
    , gains
    )

import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Types exposing (AccountInfo, SkillInfo, Skills)
import WOM.Period exposing (toValue)
import WOM.Types
    exposing
        ( ActivityObj
        , BossObj
        , DiffObj
        , EHPMethod
        , EHPObject
        , EHPRates
        , GainedData
        , Period
        , SkillObj
        )


baseUrl : String
baseUrl =
    "https://api.wiseoldman.net/v2"


skillDecoder : JD.Decoder (Dict String SkillObj)
skillDecoder =
    JD.dict <|
        JD.map4 SkillObj
            (JD.field "metric" JD.string)
            (JD.field "experience" diffDecoder)
            (JD.field "rank" diffDecoder)
            (JD.field "level" diffDecoder)


activityDecoder : JD.Decoder (Dict String ActivityObj)
activityDecoder =
    JD.dict <|
        JD.map3 ActivityObj
            (JD.field "metric" JD.string)
            (JD.field "score" diffDecoder)
            (JD.field "rank" diffDecoder)


bossDecoder : JD.Decoder (Dict String BossObj)
bossDecoder =
    JD.dict <|
        JD.map3 BossObj
            (JD.field "metric" JD.string)
            (JD.field "kills" diffDecoder)
            (JD.field "rank" diffDecoder)


diffDecoder : JD.Decoder DiffObj
diffDecoder =
    JD.map3 DiffObj
        (JD.field "gained" JD.int)
        (JD.field "start" JD.int)
        (JD.field "end" JD.int)


computedDecoder : JD.Decoder (Dict String WOM.Types.ComputedObj)
computedDecoder =
    JD.dict <|
        JD.map3 WOM.Types.ComputedObj
            (JD.at [ "value", "start" ] JD.float)
            (JD.at [ "value", "end" ] JD.float)
            (JD.at [ "value", "gained" ] JD.float)


parseGains : JD.Decoder GainedData
parseGains =
    JD.map4 GainedData
        (JD.at [ "data", "skills" ] skillDecoder)
        (JD.at [ "data", "activities" ] activityDecoder)
        (JD.at [ "data", "bosses" ] bossDecoder)
        (JD.at [ "data", "computed" ] computedDecoder)


gains : String -> Period -> (Result Http.Error GainedData -> msg) -> Cmd msg
gains username period msgType =
    Http.get
        { url = baseUrl ++ "/players/" ++ username ++ "/gained?period=" ++ toValue period
        , expect = Http.expectJson msgType parseGains
        }


parseEHPObject : JD.Decoder EHPRates
parseEHPObject =
    let
        decodeRate : JD.Decoder Int
        decodeRate =
            JD.maybe (JD.field "realRate" JD.int)
                |> JD.andThen
                    (\mi ->
                        case mi of
                            Nothing ->
                                JD.field "rate" JD.int

                            Just a ->
                                JD.succeed a
                    )

        parseMethod : JD.Decoder EHPMethod
        parseMethod =
            JD.map3 EHPMethod
                (JD.field "description" JD.string)
                decodeRate
                (JD.field "startExp" JD.int)

        parseRateObject : JD.Decoder EHPObject
        parseRateObject =
            JD.map2 EHPObject
                (JD.field "skill" JD.string)
                (JD.field "methods" (JD.list parseMethod))
    in
    JD.list
        (JD.map2 Tuple.pair
            (JD.field "skill" JD.string)
            parseRateObject
        )
        |> JD.map Dict.fromList


ehpRates : (Result Http.Error EHPRates -> msg) -> Cmd msg
ehpRates msgType =
    Http.get
        { url = baseUrl ++ "/efficiency/rates/?metric=ehp&type=main"
        , expect = Http.expectJson msgType parseEHPObject
        }


parseAccountInfo : JD.Decoder AccountInfo
parseAccountInfo =
    let
        skillInfoDecoder : JD.Decoder SkillInfo
        skillInfoDecoder =
            JD.map3 SkillInfo
                (JD.field "experience" JD.int)
                (JD.field "level" JD.int)
                (JD.field "metric" JD.string)

        skillsDecoder : JD.Decoder Skills
        skillsDecoder =
            JD.dict skillInfoDecoder
    in
    JD.map4 AccountInfo
        (JD.field "displayName" JD.string)
        (JD.field "ttm" JD.float)
        (JD.field "exp" JD.int)
        (JD.at [ "latestSnapshot", "data", "skills" ] skillsDecoder)


accountInfo : String -> (Result Http.Error AccountInfo -> msg) -> Cmd msg
accountInfo username msgType =
    Http.get
        { url = baseUrl ++ "/players/" ++ username
        , expect = Http.expectJson msgType parseAccountInfo
        }
