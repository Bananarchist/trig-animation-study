module Main exposing (main)

import Browser
import Browser.Events
import Html
import Json.Decode
import Svg
import Svg.Attributes


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


flip : (a -> b -> c) -> b -> a -> c
flip fn p2 p1 =
    fn p1 p2


type alias System =
    { mouseDown : Bool
    , keyDown : Bool
    , padBtnDown : Bool
    }


type alias Vector =
    { x : Float
    , y : Float
    , θ : Float
    }


type Model
    = Model System Vector


system : Model -> System
system (Model sys _) =
    sys


vector : Model -> Vector
vector (Model _ vec) =
    vec


setKeyDown : Bool -> Model -> Model
setKeyDown val (Model sys vec) =
    Model { sys | keyDown = val } vec


setMouseDown : Bool -> Model -> Model
setMouseDown val (Model sys vec) =
    Model { sys | mouseDown = val } vec


setPadDown : Bool -> Model -> Model
setPadDown val (Model sys vec) =
    Model { sys | padBtnDown = val } vec


type Status
    = Moving
    | Rotating


status : Model -> Status
status (Model { mouseDown, keyDown, padBtnDown } _) =
    if mouseDown || keyDown || padBtnDown then
        Moving

    else
        Rotating


type Msg
    = KeyDown
    | KeyUp
    | MouseDown
    | MouseUp
    | FrameΔ Float


rotationFrequency : Float
rotationFrequency =
    pi / 2


movementSpeed : Float
movementSpeed =
    50


xTrigFn : Float -> Float
xTrigFn θ =
    if θ == pi || θ == 0 then
        0

    else if θ == pi / 2 then
        1

    else if θ == 3 * pi / 2 then
        -1

    else if θ < pi / 2 then
        sin θ

    else if θ < pi then
        sin θ

    else if θ < 3 * pi / 2 then
        sin θ

    else
        sin θ



-- perhaps all of these are negated BECAUSE we get
-- weirdness with positive y being down?
-- and are we able to use the same function BECAUSE
-- of something to do with complementary math via
-- the quadrants?????


yTrigFn : Float -> Float
yTrigFn θ =
    if θ == pi / 2 || θ == 3 * pi / 2 then
        0

    else if θ == pi then
        1

    else if θ == 0 then
        -1

    else if θ < pi / 2 then
        cos θ |> negate

    else if θ < pi then
        cos θ |> negate

    else if θ < 3 * pi / 2 then
        cos θ |> negate

    else
        cos θ |> negate


width : Float
width =
    30


height : Float
height =
    30


addTheta : Float -> Float -> Float
addTheta θ δ =
    θ
        + δ
        |> (\v ->
                if v > 2 * pi then
                    v - 2 * pi

                else if v < 0 then
                    v + 2 * pi

                else
                    v
           )


initialAngle : Float
initialAngle =
    pi + (10 * pi / 12)


initialSystem : System
initialSystem =
    { keyDown = False
    , mouseDown = False
    , padBtnDown = False
    }


initialVector : Vector
initialVector =
    { x = 100
    , y = 100
    , θ = initialAngle
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model initialSystem initialVector, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta FrameΔ
        , Browser.Events.onKeyUp (Json.Decode.succeed KeyUp)
        , Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
        , Browser.Events.onKeyDown (Json.Decode.succeed KeyDown)
        , Browser.Events.onMouseDown (Json.Decode.succeed MouseDown)
        ]


sansCmd : Model -> ( Model, Cmd Msg )
sansCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Model sys vec) as model) =
    case msg of
        KeyUp ->
            setKeyDown False model |> sansCmd

        KeyDown ->
            setKeyDown True model |> sansCmd

        MouseUp ->
            setMouseDown False model |> sansCmd

        MouseDown ->
            setMouseDown True model |> sansCmd

        FrameΔ δ ->
            case status model of
                Moving ->
                    Model sys
                        { vec
                            | x = vec.x + (movementSpeed * δ / 1000) * xTrigFn vec.θ
                            , y = vec.y + (movementSpeed * δ / 1000) * yTrigFn vec.θ
                        }
                        |> sansCmd

                Rotating ->
                    Model sys
                        { vec
                            | θ = addTheta vec.θ (rotationFrequency * δ / 1000)
                        }
                        |> sansCmd


view : Model -> Html.Html Msg
view model =
    Html.main_ []
        [ Svg.svg
            [ Svg.Attributes.viewBox "0 0 1000 1000" ]
            [ Svg.defs [] [ gradient ]
            , viewSquare model
            ]
        ]


viewSquare : Model -> Html.Html Msg
viewSquare model =
    [ Svg.Attributes.stroke "black"
    , Svg.Attributes.fill "url('#fireButt')"
    , Svg.Attributes.width (String.fromFloat width)
    , Svg.Attributes.height (String.fromFloat height)
    ]
        ++ coords model
        ++ rotation model
        |> flip Svg.rect []


coords : Model -> List (Svg.Attribute Msg)
coords (Model _ vec) =
    [ Svg.Attributes.x (String.fromFloat vec.x), Svg.Attributes.y (String.fromFloat vec.y) ]


rotation : Model -> List (Svg.Attribute Msg)
rotation (Model _ vec) =
    let
        rotate x y θ =
            [ "rotate("
            , θ * 180 / pi |> String.fromFloat
            , String.fromFloat (x + (width / 2))
            , String.fromFloat (y + (height / 2))
            , ")"
            ]
                |> String.join " "
    in
    [ Svg.Attributes.transform (rotate vec.x vec.y vec.θ) ]


gradient : Svg.Svg Msg
gradient =
    Svg.linearGradient
        [ Svg.Attributes.id "fireButt"
        , Svg.Attributes.gradientTransform "rotate(-270)"
        ]
        [ Svg.stop
            [ Svg.Attributes.offset "65%", Svg.Attributes.stopColor "white" ]
            []
        , Svg.stop
            [ Svg.Attributes.offset "100%", Svg.Attributes.stopColor "red" ]
            []
        ]
