module Types exposing (AccountInfo, Msg(..), SkillInfo, Skills)

import Dict exposing (Dict)
import Http
import WOM.Types exposing (EHPRates, GainedData)


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


type Msg
    = DurationPicked String
    | GotGains (Result Http.Error GainedData)
    | GotEHPRates (Result Http.Error EHPRates)
    | GotAccountInfo (Result Http.Error AccountInfo)
    | SearchFieldChanged String
    | SubmitUsername
