module Theme exposing (..)


type alias Theme =
    { bg : String
    , text : String
    }


dark : Theme
dark =
    { bg = "bg-gray-900"
    , text = "text-gray-300"
    }


light : Theme
light =
    { bg = "bg-white"
    , text = "text-gray-900"
    }
