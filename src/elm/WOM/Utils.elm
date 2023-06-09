module WOM.Utils exposing (getEHPRate, getImg, getProgressPercent, imageBaseUrl, percentTowardsMax, remainingExp, ttm)

import Dict exposing (Dict)
import WOM.Data exposing (maxExp)
import WOM.Types exposing (EHPRates, Skill(..))


imageBaseUrl : String
imageBaseUrl =
    "https://wiseoldman.net/img/runescape/icons/"


getImg : String -> Maybe String
getImg s =
    case s of
        "N/A" ->
            Nothing

        a ->
            Just (imageBaseUrl ++ a ++ ".png")


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


remainingExp : Dict String { r | level : Int, experience : Int } -> Int
remainingExp skills =
    let
        remainingSkills : Dict String { r | level : Int, experience : Int }
        remainingSkills =
            Dict.filter (\_ v -> v.level < 99) skills

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
