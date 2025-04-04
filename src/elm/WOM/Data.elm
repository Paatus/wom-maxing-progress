module WOM.Data exposing (colors, maxExp)

import Color exposing (Color, rgb255)
import Dict exposing (Dict)


maxExp : Int
maxExp =
    13034431


colors : Dict String Color
colors =
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
