module WOM.API exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as JD
import List exposing (head)
import WOM.Period exposing (toValue)
import WOM.Types exposing (ActivityObj, BossObj, DeltaObj(..), DiffObj, GainedData, Period, Skill(..), SkillObj, TopItems)


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


getTopItems : GainedData -> JD.Decoder TopItems
getTopItems datta =
    let
        gainedExperience =
            Dict.get "overall" datta.skill
                |> Maybe.map SkillDelta
    in
    JD.succeed
        { gainedExperience = gainedExperience
        , topBoss = getTopBoss datta.boss
        , topSkill = getTopSkill datta.skill
        , topActivity = getTopActivity datta.activity
        }


parseGains : JD.Decoder TopItems
parseGains =
    JD.map3 GainedData
        (JD.at [ "data", "skills" ] skillDecoder)
        (JD.at [ "data", "activities" ] activityDecoder)
        (JD.at [ "data", "bosses" ] bossDecoder)
        |> JD.andThen getTopItems


gains : String -> Period -> (Result Http.Error TopItems -> msg) -> Cmd msg
gains username period msgType =
    Http.get
        { url = baseUrl ++ "/players/" ++ username ++ "/gained?period=" ++ toValue period
        , expect = Http.expectJson msgType parseGains
        }
