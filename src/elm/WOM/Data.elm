module WOM.Data exposing (..)

import Color exposing (Color, rgb)
import Dict exposing (Dict)
import Color exposing (rgb255)


maxExp : Int
maxExp =
    13034431


colours : Dict String Color
colours =
    Dict.fromList
        [ ( "attack", rgb255 155 32 7 )
        , ( "defence", rgb255 98 119 190 )
        , ( "strength", rgb255 4 149 90 )
        , ( "hitpoints", rgb255 131 126 126 )
        , ( "ranged", rgb255 109 144 23 )
        , ( "prayer", rgb255 159 147 35 )
        , ( "magic", rgb255 50 80 193 )
        , ( "cooking", rgb255 112 35 134 )
        , ( "woodcutting", rgb255 52 140 37 )
        , ( "fletching", rgb255 3 141 125 )
        , ( "fishing", rgb255 106 132 164 )
        , ( "firemaking", rgb255 189 120 25 )
        , ( "crafting", rgb255 151 110 77 )
        , ( "smithing", rgb255 108 107 82 )
        , ( "mining", rgb255 93 143 167 )
        , ( "herblore", rgb255 7 133 9 )
        , ( "agility", rgb255 58 60 137 )
        , ( "thieving", rgb255 108 52 87 )
        , ( "slayer", rgb255 100 100 100 )
        , ( "farming", rgb255 101 152 63 )
        , ( "runecraft", rgb255 170 141 26 )
        , ( "hunter", rgb255 92 89 65 )
        , ( "construction", rgb255 130 116 95 )
        ]


colors : Dict String String
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
