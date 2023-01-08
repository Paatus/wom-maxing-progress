module WOM.Utils exposing (..)


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
