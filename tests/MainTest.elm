module MainTest exposing (suite)

-- import Html.Attributes as Attr
-- import Test.Html.Event as Event
-- import Test.Html.Query as Query
-- import Test.Html.Selector as Html

import Dict
import Expect
import Test exposing (Test)
import WOM.Utils exposing (percentTowardsMax)



-- queryHtmlInitModel : Query.Single Msg
-- queryHtmlInitModel =
--     initialModel
--         |> Tuple.first
--         |> Query.fromHtml
--
-- initialCommand : Cmd Msg
-- initialCommand =
--     initialModel
--         |> Tuple.second


suite : Test
suite =
    Test.describe "HelloWorld"
        [ Test.test "percent Done" <|
            \_ ->
                percentTowardsMax
                    (Dict.fromList
                        [ ( "agility", { experience = 13034869, level = 99, name = "agility" } )
                        , ( "attack", { experience = 26644604, level = 99, name = "attack" } )
                        , ( "construction", { experience = 5903925, level = 91, name = "construction" } )
                        , ( "cooking", { experience = 13573587, level = 99, name = "cooking" } )
                        , ( "crafting", { experience = 14185379, level = 99, name = "crafting" } )
                        , ( "defence", { experience = 18611437, level = 99, name = "defence" } )
                        , ( "farming", { experience = 13167248, level = 99, name = "farming" } )
                        , ( "firemaking", { experience = 5552437, level = 90, name = "firemaking" } )
                        , ( "fishing", { experience = 13035644, level = 99, name = "fishing" } )
                        , ( "fletching", { experience = 13045800, level = 99, name = "fletching" } )
                        , ( "herblore", { experience = 5800589, level = 90, name = "herblore" } )
                        , ( "hitpoints", { experience = 39598909, level = 99, name = "hitpoints" } )
                        , ( "hunter", { experience = 8970030, level = 95, name = "hunter" } )
                        , ( "magic", { experience = 14190082, level = 99, name = "magic" } )
                        , ( "mining", { experience = 13598193, level = 99, name = "mining" } )
                        , ( "overall", { experience = 9350528191, level = 2277, name = "overall" } )
                        , ( "prayer", { experience = 5543725, level = 90, name = "prayer" } )
                        , ( "ranged", { experience = 30561144, level = 99, name = "ranged" } )
                        , ( "runecrafting", { experience = 13035831, level = 99, name = "runecrafting" } )
                        , ( "slayer", { experience = 18538508, level = 99, name = "slayer" } )
                        , ( "smithing", { experience = 13044558, level = 99, name = "smithing" } )
                        , ( "strength", { experience = 30563859, level = 99, name = "strength" } )
                        , ( "thieving", { experience = 7020362, level = 92, name = "thieving" } )
                        , ( "woodcutting", { experience = 13307471, level = 99, name = "woodcutting" } )
                        ]
                    )
                    |> Expect.within (Expect.Absolute 0.3) 87.0

        -- , Test.test "Fetches data on launch" <|
        --     \_ ->
        --         Event.simulate initialCommand
        --             |> Event.expect GotGains
        -- , Test.test "Displays the current count" <|
        --     \_ ->
        --         queryHtmlInitModel
        --             |> Query.has [ Html.text "Count is: 0" ]
        -- , Test.test "clicking on the + button sends an increment message" <|
        --     \_ ->
        --         queryHtmlInitModel
        --             |> Query.find [ Html.tag "button", Html.containing [ Html.text "+" ] ]
        --             |> Event.simulate Event.click
        --             |> Event.expect Increment
        -- , Test.test "clicking on the - button sends a decrement message" <|
        --     \_ ->
        --         queryHtmlInitModel
        --             |> Query.find [ Html.tag "button", Html.containing [ Html.text "-" ] ]
        --             |> Event.simulate Event.click
        --             |> Event.expect Decrement
        -- , Test.test "Elm documentation link present" <|
        --     \_ ->
        --         queryHtmlInitModel
        --             |> Query.has [ Html.tag "a", Html.attribute (Attr.href "https://guide.elm-lang.org/") ]
        -- , Test.test "Vite documentation link present" <|
        --     \_ ->
        --         queryHtmlInitModel
        --             |> Query.has [ Html.tag "a", Html.attribute (Attr.href "https://vitejs.dev/guide/features.html") ]
        -- , Test.test "Tailwind CSS documentation link present" <|
        --     \_ ->
        --         queryHtmlInitModel
        --             |> Query.has [ Html.tag "a", Html.attribute (Attr.href "https://tailwindcss.com/docs/installation") ]
        -- , Test.test "Daisy UI documentation link present" <|
        --     \_ ->
        --         queryHtmlInitModel
        --             |> Query.has [ Html.tag "a", Html.attribute (Attr.href "https://daisyui.com/docs/use/") ]
        -- , Test.test "Project GitHub link present" <|
        --     \_ ->
        --         queryHtmlInitModel
        --             |> Query.has [ Html.tag "a", Html.attribute (Attr.href "https://github.com/gacallea/elm_vite_tailwind_template") ]
        ]
