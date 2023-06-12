module BarChart exposing (ChartData, chart)

import Axis
import Color exposing (Color)
import FormatNumber exposing (format)
import Html exposing (Html)
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fill, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..))
import Utils exposing (decimalLocale)


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    40


xScale : List ChartData -> BandScale String
xScale model =
    List.map .label model
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * padding )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 100 )


dateFormat : String -> String
dateFormat =
    identity


xAxis : List ChartData -> Svg msg
xAxis model =
    Axis.bottom [] (Scale.toRenderable dateFormat (xScale model))


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 10 ] yScale


column : BandScale String -> ChartData -> Svg msg
column scale { label, value, color } =
    g [ class [ "column" ] ]
        [ rect
            [ x <| Scale.convert scale label
            , y <| Scale.convert yScale value
            , width <| Scale.bandwidth scale
            , height <| h - Scale.convert yScale value - 2 * padding
            , fill (Paint color)
            ]
            []
        , text_
            [ x <| Scale.convert (Scale.toRenderable dateFormat scale) label
            , y <| Scale.convert yScale value + 15
            , textAnchor AnchorMiddle
            ]
            [ text <| format (decimalLocale 1) value ++ "%" ]
        ]


type alias ChartData =
    { label : String
    , value : Float
    , color : Color
    }


view : List ChartData -> Html msg
view model =
    svg [ viewBox 0 0 w h ]
        [ style [] [ text """
            .axis, .axis line, .axis .domain { stroke: white; color: white; }
            .tick text { fill: white; font-size: 1rem; stroke: none; }
            .column { cursor: pointer; }
            .column text { display: none; }
            .column:hover rect { filter: brightness(1.5) }
            .column:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate (padding - 1) (h - padding) ], class [ "axis" ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ], class [ "axis" ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column (xScale model)) model
        ]


chart : List ChartData -> Html msg
chart data =
    view data
