module Main exposing (main)

-- elm install avh4/elm-color

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Html.Attributes exposing (style)


view : Html msg
view =
    let
        width =
            500

        height =
            300
    in
    Canvas.toHtml ( width, height )
        [ style "border" "1px solid black"
        , style "display" "block"
        ]
        [ shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]
        , renderSquare
        ]


renderSquare =
    shapes [ fill (Color.rgba 0 0 0 1) ]
        [ rect ( 0, 0 ) 100 50 ]


main =
    view


type Turn
    = Left
    | Right


a n s =
    if n <= 0 then
        s
            |> draw

    else
        s
            |> a (n - 1)
            |> turn Right
            |> b (n - 1)
            |> draw
            |> turn Right


b n s =
    if n <= 0 then
        s
            |> draw

    else
        s
            |> turn Left
            |> draw
            |> a (n - 1)
            |> turn Left
            |> b (n - 1)
