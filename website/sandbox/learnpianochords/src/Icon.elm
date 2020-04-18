module Icon exposing (..)

import Svg exposing (node, svg)
import Svg.Attributes exposing (..)
import UI


svgColor color =
    let
        classes =
            case color of
                UI.Primary ->
                    [ "text-gray-500", "fill-current" ]

                UI.Secondary ->
                    [ "text-gray-300", "fill-current" ]
    in
    class <| String.join " " classes


cog =
    svg [ class "icon-cog", viewBox "0 0 24 24", xmlLang "http://www.w3.org/2000/svg" ]
        [ Svg.path
            [ svgColor UI.Primary
            , d "M6.8 3.45c.87-.52 1.82-.92 2.83-1.17a2.5 2.5 0 0 0 4.74 0c1.01.25 1.96.65 2.82 1.17a2.5 2.5 0 0 0 3.36 3.36c.52.86.92 1.8 1.17 2.82a2.5 2.5 0 0 0 0 4.74c-.25 1.01-.65 1.96-1.17 2.82a2.5 2.5 0 0 0-3.36 3.36c-.86.52-1.8.92-2.82 1.17a2.5 2.5 0 0 0-4.74 0c-1.01-.25-1.96-.65-2.82-1.17a2.5 2.5 0 0 0-3.36-3.36 9.94 9.94 0 0 1-1.17-2.82 2.5 2.5 0 0 0 0-4.74c.25-1.01.65-1.96 1.17-2.82a2.5 2.5 0 0 0 3.36-3.36zM12 16a4 4 0 1 0 0-8 4 4 0 0 0 0 8z"
            , fill "red"
            ]
            []
        , node "circle"
            [ svgColor UI.Secondary, cx "12", cy "12", r "2" ]
            []
        ]


close =
    svg [ class "icon-close", viewBox "0 0 24 24", xmlLang "http://www.w3.org/2000/svg" ]
        [ Svg.path
            [ svgColor UI.Primary
            , d "M15.78 14.36a1 1 0 0 1-1.42 1.42l-2.82-2.83-2.83 2.83a1 1 0 1 1-1.42-1.42l2.83-2.82L7.3 8.7a1 1 0 0 1 1.42-1.42l2.83 2.83 2.82-2.83a1 1 0 0 1 1.42 1.42l-2.83 2.83 2.83 2.82z"
            , fill "red"
            , fillRule "evenodd"
            ]
            []
        ]
