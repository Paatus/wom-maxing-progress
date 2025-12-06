module PieChart exposing (PieChartData, chart)

import Array exposing (Array)
import Color exposing (Color)
import FormatNumber exposing (format)
import Html exposing (Html, div, li, section, span, ul)
import Html.Attributes exposing (class, style)
import Path
import Shape exposing (defaultPieConfig)
import String exposing (fromInt)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (dy, fill, textAnchor, transform, viewBox)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), em)
import Utils exposing (decimalLocale)


viewBoxWidth : Float
viewBoxWidth =
    990


viewBoxHeight : Float
viewBoxHeight =
    504


radius : Float
radius =
    min viewBoxWidth viewBoxHeight / 2


type alias PieChartData =
    { label : String
    , level : Int
    , value : Float
    , color : Color
    }


legend : List PieChartData -> Html msg
legend model =
    let
        items : List (Html msg)
        items =
            List.map
                (\{ label, color, level } ->
                    li [ class "flex items-center" ]
                        [ div [ class "rounded-full w-3 h-3 content-['']", style "background-color" (Color.toCssString color) ] []
                        , span [ class "ml-2" ]
                            [ text
                                (label
                                    ++ (" ("
                                            ++ fromInt level
                                            ++ ")"
                                       )
                                )
                            ]
                        ]
                )
                model
    in
    ul [ class "flex flex-col w-64" ] items


view : List PieChartData -> Html msg
view model =
    let
        colors : Array Color
        colors =
            model |> List.map .color |> Array.fromList

        pieData : List Shape.Arc
        pieData =
            model |> List.map .value |> Shape.pie { defaultPieConfig | outerRadius = radius, cornerRadius = 5, padRadius = 50, padAngle = 0.1, innerRadius = radius - 100 }

        arr : Array PieChartData
        arr =
            Array.fromList model

        makeSlice : Int -> Shape.Arc -> Svg msg
        makeSlice index datum =
            let
                ( x, y ) =
                    Shape.centroid { datum | innerRadius = radius - 60, outerRadius = radius - 40 }
            in
            g [ TypedSvg.Attributes.class [ "slice" ] ]
                [ Path.element (Shape.arc datum) [ fill <| Paint <| Maybe.withDefault Color.black <| Array.get index colors ]
                , text_
                    [ transform [ Translate x y ]
                    , dy (em 1)
                    , textAnchor AnchorMiddle
                    ]
                    [ text (format (decimalLocale 1) (Array.get index arr |> Maybe.map .value |> Maybe.withDefault 0) ++ " h") ]
                ]
    in
    section [ class "flex items-center flex-row" ]
        [ legend model
        , svg [ viewBox 0 0 viewBoxWidth viewBoxHeight ]
            [ TypedSvg.style [] [ text """
              .slice { cursor: pointer; }
              .slice text { display: none; }
              .slice:hover path { filter: brightness(1.5); }
              .slice:hover text { display: inline; }
              """ ]
            , g [ transform [ Translate (viewBoxWidth / 2) (viewBoxHeight / 2) ] ]
                [ g [] <| List.indexedMap makeSlice pieData

                -- , g [] <| List.map2 makeLabel pieData model
                ]
            ]
        ]


chart : List PieChartData -> Html msg
chart data =
    view data
