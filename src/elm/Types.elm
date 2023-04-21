module Types exposing (..)

import Dict exposing (Dict)
import WOM.Types exposing (Skill)


type Tab
    = Gains
    | MaxProgress


type alias SkillInfo =
    { experience : Int
    , level : Int
    , name : String
    }


type alias Skills =
    Dict String SkillInfo


type alias AccountInfo =
    { displayName : String
    , ttm : Float
    , exp : Int
    , skills : Skills
    }
