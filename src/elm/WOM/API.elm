module WOM.API exposing
    ( accountInfo
    , activityDecoder
    , baseUrl
    , bossDecoder
    , diffDecoder
    , ehpRates
    , gains
    , getTopActivity
    , getTopBoss
    , getTopItems
    , getTopSkill
    , parseGains
    , skillDecoder
    )

import Dict exposing (Dict)
import Http
import Json.Decode as JD
import List exposing (head)
import Types exposing (AccountInfo, SkillInfo, Skills)
import WOM.Period exposing (toValue)
import WOM.Types
    exposing
        ( ActivityObj
        , BossObj
        , DeltaObj(..)
        , DiffObj
        , EHPMethod
        , EHPObject
        , EHPRates
        , GainedData
        , Period
        , Skill(..)
        , SkillObj
        , TopItems
        )
import WOM.Utils exposing (toSkill)


baseUrl : String
baseUrl =
    "https://api.wiseoldman.net/v2"


log : String -> JD.Decoder a -> JD.Decoder a
log message =
    JD.map (Debug.log message)


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


getTopBoss : Dict String BossObj -> Maybe DeltaObj
getTopBoss =
    Dict.toList
        >> List.sortWith (\( _, a ) ( _, b ) -> compare b.kills.gained a.kills.gained)
        >> head
        >> Maybe.andThen
            (\( name, a ) ->
                if a.kills.gained <= 0 then
                    Nothing

                else
                    Just ( name, a )
            )
        >> Maybe.map Tuple.second
        >> Maybe.map BossDelta


getTopSkill : Dict String SkillObj -> Maybe DeltaObj
getTopSkill =
    Dict.toList
        >> List.filter (\( name, _ ) -> name /= "overall")
        >> List.sortWith (\( _, a ) ( _, b ) -> compare b.experience.gained a.experience.gained)
        >> head
        >> Maybe.andThen
            (\( name, a ) ->
                if a.experience.gained <= 0 then
                    Nothing

                else
                    Just ( name, a )
            )
        >> Maybe.map Tuple.second
        >> Maybe.map SkillDelta


getTopActivity : Dict String ActivityObj -> Maybe DeltaObj
getTopActivity =
    Dict.toList
        >> List.sortWith (\( _, a ) ( _, b ) -> compare b.score.gained a.score.gained)
        >> head
        >> Maybe.andThen
            (\( name, a ) ->
                if a.score.gained <= 0 then
                    Nothing

                else
                    Just ( name, a )
            )
        >> Maybe.map Tuple.second
        >> Maybe.map ActivityDelta


getTopItems : GainedData -> TopItems
getTopItems datta =
    let
        gainedExperience =
            Dict.get "overall" datta.skill
                |> Maybe.map SkillDelta
    in
    { gainedExperience = gainedExperience
    , topBoss = getTopBoss datta.boss
    , topSkill = getTopSkill datta.skill
    , topActivity = getTopActivity datta.activity
    }


parseGains : JD.Decoder GainedData
parseGains =
    JD.map3 GainedData
        (JD.at [ "data", "skills" ] skillDecoder)
        (JD.at [ "data", "activities" ] activityDecoder)
        (JD.at [ "data", "bosses" ] bossDecoder)


gains : String -> Period -> (Result Http.Error GainedData -> msg) -> Cmd msg
gains username period msgType =
    Http.get
        { url = baseUrl ++ "/players/" ++ username ++ "/gained?period=" ++ toValue period
        , expect = Http.expectJson msgType parseGains
        }


parseEHPObject : JD.Decoder EHPRates
parseEHPObject =
    let
        parseMethod : JD.Decoder EHPMethod
        parseMethod =
            JD.map3 EHPMethod
                (JD.field "description" JD.string)
                (JD.field "rate" JD.int)
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
