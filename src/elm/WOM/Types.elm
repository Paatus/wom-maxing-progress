module WOM.Types exposing
    ( ActivityObj
    , BossObj
    , ComputedObj
    , DiffObj
    , EHPMethod
    , EHPObject
    , EHPRates
    , GainedData
    , Period(..)
    , SkillObj
    )

import Dict exposing (Dict)


type Period
    = Five_min
    | Day
    | Week
    | Month
    | Year


type alias DiffObj =
    { gained : Int
    , start : Int
    , end : Int
    }


type alias SkillObj =
    { name : String
    , experience : DiffObj
    , rank : DiffObj
    , level : DiffObj
    }


type alias ActivityObj =
    { name : String
    , score : DiffObj
    , rank : DiffObj
    }


type alias BossObj =
    { name : String
    , kills : DiffObj
    , rank : DiffObj
    }


type alias ComputedObj =
    { start : Float
    , end : Float
    , gained : Float
    }


type alias GainedData =
    { skill : Dict String SkillObj
    , activity : Dict String ActivityObj
    , boss : Dict String BossObj
    , computed : Dict String ComputedObj
    }


type alias EHPMethod =
    { methodName : String
    , rate : Int
    , fromExp : Int
    }


type alias EHPObject =
    { skill : String
    , methods : List EHPMethod
    }


type alias EHPRates =
    Dict String EHPObject
