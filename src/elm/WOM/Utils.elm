module WOM.Utils exposing (getEHPRate, getProgressPercent, hasEhpRate, normalizedSkills, percentTowardsMax, remainingExp, ttm)

import Dict exposing (Dict)
import Types exposing (SkillInfo, Skills)
import WOM.Data exposing (maxExp)
import WOM.Types exposing (EHPRates)


getEHPRate : EHPRates -> String -> Int -> Int
getEHPRate rates skill exp =
    case Dict.get skill rates of
        Just entry ->
            List.filter (\method -> method.fromExp <= exp) entry.methods
                |> List.sortBy
                    .rate
                |> List.reverse
                |> List.head
                |> Maybe.map .rate
                |> Maybe.withDefault 0

        Nothing ->
            -1


hasEhpRate : EHPRates -> String -> Bool
hasEhpRate rates skillName =
    case Dict.get skillName rates of
        Just _ ->
            True

        Nothing ->
            False


ttm : EHPRates -> String -> Int -> Float
ttm rates skill currentExp =
    if currentExp >= maxExp then
        0

    else
        let
            remaining : Float
            remaining =
                maxExp - currentExp |> toFloat
        in
        remaining / (getEHPRate rates skill currentExp |> toFloat)


getProgressPercent : Int -> Float
getProgressPercent currentExp =
    ((currentExp |> toFloat) / (maxExp |> toFloat)) * 100


normalizedSkills : Skills -> Skills
normalizedSkills =
    let
        norm : SkillInfo -> SkillInfo
        norm s =
            if s.experience < 0 then
                { s | experience = 1 }

            else
                s
    in
    Dict.map (\_ v -> norm v)


remainingExp : Dict String { r | level : Int, experience : Int } -> Int
remainingExp skills =
    let
        remainingSkills : Dict String { r | level : Int, experience : Int }
        remainingSkills =
            Dict.filter (\_ v -> v.level < 99) skills
                |> Dict.filter (\_ v -> v.experience >= 0)

        maxedSkills : Int
        maxedSkills =
            Dict.filter (\_ v -> v.level == 99) skills |> Dict.size

        totalSkills : Int
        totalSkills =
            Dict.size remainingSkills + maxedSkills

        expNeededForMax : Int
        expNeededForMax =
            totalSkills * maxExp

        sumExperience : String -> { r | level : Int, experience : Int } -> Int -> Int
        sumExperience _ skill acc =
            acc + skill.experience

        expInNonMaxedSkills : Int
        expInNonMaxedSkills =
            Dict.foldl sumExperience 0 remainingSkills

        maxedSkillsExp : Int
        maxedSkillsExp =
            maxedSkills * maxExp
    in
    expNeededForMax - (maxedSkillsExp + expInNonMaxedSkills)


percentTowardsMax : Dict String { r | level : Int, experience : Int } -> Float
percentTowardsMax skills =
    let
        remainingSkills : Int
        remainingSkills =
            Dict.filter (\_ v -> v.level < 99) skills |> Dict.size

        maxedSkills : Int
        maxedSkills =
            Dict.filter (\_ v -> v.level == 99) skills |> Dict.size

        totalSkills : Int
        totalSkills =
            remainingSkills + maxedSkills

        expNeededForMax : Int
        expNeededForMax =
            totalSkills * maxExp

        percentLeft : Float
        percentLeft =
            (toFloat (remainingExp skills) / toFloat expNeededForMax) * 100
    in
    100 - percentLeft
