module Main exposing (Orientation(..), dragonCurve, main)

import Canvas exposing (PathSegment, Point, Shape)
import Canvas.Settings
import Color
import Html exposing (Html)
import Html.Attributes


main : Html msg
main =
    view


view : Html msg
view =
    let
        width : number
        width =
            500

        height : number
        height =
            500
    in
    Canvas.toHtml ( width, height )
        [ Html.Attributes.style "border" "1px solid black"
        , Html.Attributes.style "display" "block"
        ]
        [ Canvas.clear ( 0, 0 ) width height
        , Canvas.shapes
            [ Canvas.Settings.stroke Color.black ]
            [ dragonCurveLines ( 250, 250 ) ]
        ]


type Turn
    = Left
    | Right


type Orientation
    = North
    | South
    | East
    | West


type alias Turtle =
    { orientation : Orientation
    , location : Point
    }


type alias Dragon =
    ( Turtle, List PathSegment ) -> ( Turtle, List PathSegment )


dragonCurveLines : Point -> Shape
dragonCurveLines start =
    dragonCurve
        ( { location = start
          , orientation = South
          }
        , []
        )
        |> Tuple.second
        |> List.reverse
        |> Canvas.path start


dragonCurve : Dragon
dragonCurve s =
    s
        |> draw
        |> a 12


a : Int -> Dragon
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


b : Int -> Dragon
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


turn : Turn -> Dragon
turn direction ( turtle, segments ) =
    let
        newOrientation =
            case ( direction, turtle.orientation ) of
                ( Left, North ) ->
                    West

                ( Left, West ) ->
                    South

                ( Left, South ) ->
                    East

                ( Left, East ) ->
                    North

                ( Right, North ) ->
                    East

                ( Right, East ) ->
                    South

                ( Right, South ) ->
                    West

                ( Right, West ) ->
                    North
    in
    ( { turtle | orientation = newOrientation }
    , segments
    )


draw : Dragon
draw ( turtle, segments ) =
    let
        startingLocation =
            turtle.location

        endingLocation =
            travel turtle.orientation startingLocation
    in
    ( { turtle | location = endingLocation }
    , Canvas.lineTo endingLocation :: segments
    )


travel : Orientation -> Point -> Point
travel orientation ( x, y ) =
    let
        length =
            1
    in
    case orientation of
        North ->
            ( x, y + length )

        South ->
            ( x, y - length )

        East ->
            ( x + length, y )

        West ->
            ( x - length, y )
