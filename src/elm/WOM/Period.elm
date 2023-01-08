module WOM.Period exposing (..)

import WOM.Types exposing (Period(..))


formatPeriod : Period -> String
formatPeriod p =
    case p of
        Five_min ->
            "Last five minutes"

        Day ->
            "Last day"

        Week ->
            "Last week"

        Month ->
            "Last month"

        Year ->
            "Last year"


toValue : Period -> String
toValue p =
    case p of
        Five_min ->
            "five_min"

        Day ->
            "day"

        Week ->
            "week"

        Month ->
            "month"

        Year ->
            "year"
