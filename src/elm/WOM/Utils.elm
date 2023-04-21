module WOM.Utils exposing (getEHPRate, getImg, getProgressPercent, imageBaseUrl, toSkill, ttm)

import Dict
import WOM.Types exposing (EHPMethod, EHPObject, EHPRates, Skill(..))
import WOM.Data exposing (maxExp)


imageBaseUrl : String
imageBaseUrl =
    "https://wiseoldman.net/img/runescape/icons_small/"


getImg : String -> Maybe String
getImg s =
    case s of
        "N/A" ->
            Nothing

        a ->
            Just (imageBaseUrl ++ a ++ ".png")


toString : Skill -> String
toString skill =
    case skill of
        Overall ->
            "Overall"

        Attack ->
            "Attack"

        Construction ->
            "Construction"

        Agility ->
            "Agility"

        Cooking ->
            "Cooking"

        Crafting ->
            "Crafting"

        Defence ->
            "Defence"

        Strength ->
            "Strength"

        Hitpoints ->
            "Hitpoints"

        Ranged ->
            "Ranged"

        Prayer ->
            "Prayer"

        Magic ->
            "Magic"

        Woodcutting ->
            "Woodcutting"

        Fletching ->
            "Fletching"

        Fishing ->
            "Fishing"

        Firemaking ->
            "Firemaking"

        Smithing ->
            "Smithing"

        Mining ->
            "Mining"

        Herblore ->
            "Herblore"

        Thieving ->
            "Thieving"

        Slayer ->
            "Slayer"

        Farming ->
            "Farming"

        Runecrafting ->
            "Runecrafting"

        Hunter ->
            "Hunter"


toSkill : String -> Maybe Skill
toSkill skill =
    case skill of
        "overall" ->
            Just Overall

        "attack" ->
            Just Attack

        "construction" ->
            Just Construction

        "agility" ->
            Just Agility

        "cooking" ->
            Just Cooking

        "crafting" ->
            Just Crafting

        "defence" ->
            Just Defence

        "strength" ->
            Just Strength

        "hitpoints" ->
            Just Hitpoints

        "ranged" ->
            Just Ranged

        "prayer" ->
            Just Prayer

        "magic" ->
            Just Magic

        "woodcutting" ->
            Just Woodcutting

        "fletching" ->
            Just Fletching

        "fishing" ->
            Just Fishing

        "firemaking" ->
            Just Firemaking

        "smithing" ->
            Just Smithing

        "mining" ->
            Just Mining

        "herblore" ->
            Just Herblore

        "thieving" ->
            Just Thieving

        "slayer" ->
            Just Slayer

        "farming" ->
            Just Farming

        "runecrafting" ->
            Just Runecrafting

        "hunter" ->
            Just Hunter

        _ ->
            Nothing


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
            remainingExp : Float
            remainingExp =
                maxExp - currentExp |> toFloat
        in
        remainingExp / (getEHPRate rates skill currentExp |> toFloat)


getProgressPercent : Int -> Float
getProgressPercent currentExp =
    ((currentExp |> toFloat) / (maxExp |> toFloat)) * 100
