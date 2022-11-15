module Main exposing (main)

import Browser 
import Browser.Events
import Json.Decode
import Html
import Svg
import Svg.Attributes

main : Program () Model Msg
main = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

flip : (a -> b -> c) -> b -> a -> c
flip fn p2 p1 = fn p1 p2

type alias Position = (Float, Float)

type Model 
  = Rotating Position Float
  | Moving Position Float

type Msg
  = KeyDown
  | KeyUp
  | FrameΔ Float

rotationFrequency : Float
rotationFrequency = pi / 2

movementSpeed : Float
movementSpeed = 50

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
width = 30

height : Float
height = 30

addTheta : Float -> Float -> Float
addTheta θ δ = 
  θ + δ
  |> (\v -> if v > 2 * pi then v - 2 * pi else if v < 0 then v + 2 * pi else v)

initialAngle : Float
initialAngle =
  pi + (10 * pi / 12)


init : () -> (Model, Cmd Msg)
init _ =
  ( Rotating (100, 100) initialAngle, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Browser.Events.onAnimationFrameDelta FrameΔ
    , Browser.Events.onKeyUp (Json.Decode.succeed KeyUp)
    , Browser.Events.onKeyDown (Json.Decode.succeed KeyDown)
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model) of
    (KeyUp, Moving p θ) -> (Rotating p θ, Cmd.none)
    (KeyDown, Rotating p θ) -> (Moving p θ, Cmd.none)
    (FrameΔ δ, Rotating p θ) -> (Rotating p (addTheta θ (rotationFrequency * δ / 1000)), Cmd.none) 
    -- not done
    (FrameΔ δ, Moving (x, y) θ) -> (Moving (x + (movementSpeed * δ / 1000) * (xTrigFn θ), y + (movementSpeed * δ / 1000) * (yTrigFn θ)) θ, Cmd.none)
    _ -> (model, Cmd.none)

view : Model -> Html.Html Msg
view model = 
  Html.main_ [] 
  [
    Svg.svg
      [ Svg.Attributes.viewBox "0 0 1000 1000" ]
      [ Svg.defs [] [ gradient ]
      , viewSquare model 
      ]
  ]

viewSquare : Model -> Html.Html Msg
viewSquare model =
  [ 
  Svg.Attributes.stroke "black"
  , Svg.Attributes.fill "url('#fireButt')" 
  , Svg.Attributes.width (String.fromFloat width)
  , Svg.Attributes.height (String.fromFloat height)
  ]
  ++ coords model
  ++ rotation model
  |> flip Svg.rect [] 

coords : Model -> List (Svg.Attribute Msg)
coords model = 
  case model of
    Rotating (x,y) _ -> [ Svg.Attributes.x (String.fromFloat x), Svg.Attributes.y (String.fromFloat y) ]
    Moving (x,y) _ -> [ Svg.Attributes.x (String.fromFloat x), Svg.Attributes.y (String.fromFloat y) ]

rotation : Model -> List (Svg.Attribute Msg)
rotation model =
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
  case model of
    Rotating (x,y) θ -> [ Svg.Attributes.transform (rotate x y θ) ]
    Moving (x,y) θ -> [ Svg.Attributes.transform (rotate x y θ) ]


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