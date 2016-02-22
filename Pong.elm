module Main (..) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text exposing (..)
import Time exposing (..)
import Window


type alias Input =
  { space : Bool
  , paddle1 : Int
  , paddle2 : Int
  , delta : Time
  }


delta : Signal Time
delta =
  Signal.map inSeconds (fps 35)


input : Signal Input
input =
  Signal.sampleOn delta
    <| Signal.map4
        Input
        Keyboard.space
        (Signal.map .y Keyboard.wasd)
        (Signal.map .y Keyboard.arrows)
        delta


( gameWidth, gameHeight ) =
  ( 600, 400 )


( halfWidth, halfHeight ) =
  ( 300, 200 )


type alias Moveable a =
  { a
    | x : Float
    , y : Float
    , vx : Float
    , vy : Float
  }


type alias Ball =
  Moveable {}


type alias Player =
  Moveable { score : Int }



-- Make this type Running = True | False


type Running
  = IsRunning
  | NotRunning


type alias Game =
  { state : Running
  , ball : Ball
  , player1 : Player
  , player2 : Player
  }


player : Float -> Player
player x =
  { x = x, y = 0, vx = 0, vy = 0, score = 0 }


defaultGame : Game
defaultGame =
  { state = NotRunning
  , ball = { x = 0, y = 0, vx = 200, vy = 200 }
  , player1 = player (20 - halfWidth)
  , player2 = player (halfWidth - 20)
  }



-- are n and m near each other?
-- specifically are they within c of each other?


near : Float -> Float -> Float -> Bool
near n c m =
  m >= n - c && m <= n + c



-- is the ball within a paddle?


within : Ball -> Player -> Bool
within ball player =
  near player.x 8 ball.x
    && near player.y 20 ball.y



-- change the direction of a velocity based on collisions


stepV : Float -> Bool -> Bool -> Float
stepV v lowerCollision upperCollision =
  if lowerCollision then
    abs v
  else if upperCollision then
    -(abs v)
  else
    v



-- step the position of an object based on its velocity and a timestep


updatePosition : Time -> Moveable a -> Moveable a
updatePosition t ({ x, y, vx, vy } as moveable) =
  { moveable
    | x = x + vx * t
    , y = y + vy * t
  }



-- move a ball forward, detecting collisions with either paddle


stepBall : Time -> Ball -> Player -> Player -> Ball
stepBall t ({ x, y, vx, vy } as ball) player1 player2 =
  if ballOffCourt ball then
    { ball | x = 0, y = 0 }
  else
    updatePosition
      t
      { ball
        | vx =
            stepV vx (ball `within` player1) (ball `within` player2)
        , vy =
            stepV vy (ballAtTopOfCourt ball) (ballAtBottomOfCourt ball)
      }


ballOffCourt : Ball -> Bool
ballOffCourt ball =
  not <| near 0 halfWidth ball.x


ballAtTopOfCourt : Ball -> Bool
ballAtTopOfCourt ball =
  (ball.y < 7 - halfHeight)


ballAtBottomOfCourt : Ball -> Bool
ballAtBottomOfCourt ball =
  (ball.y > halfHeight - 7)



-- step a player forward, making sure it does not fly off the court


stepPlayer : Time -> Int -> Int -> Player -> Player
stepPlayer t dir points player =
  let
    player' =
      updatePosition t { player | vy = toFloat dir * 200 }

    y' =
      clamp (22 - halfHeight) (halfHeight - 22) player'.y

    score' =
      player.score + points
  in
    { player' | y = y', score = score' }


stepGame : Input -> Game -> Game
stepGame input game =
  let
    { space, paddle1, paddle2, delta } =
      input

    { state, ball, player1, player2 } =
      game

    -- let's extract this later.
    score1 =
      if ball.x > halfWidth then
        1
      else
        0

    score2 =
      if ball.x < -halfWidth then
        1
      else
        0

    -- this could be clearer
    state' =
      if space then
        IsRunning
      else if score1 /= score2 then
        NotRunning
      else
        state

    ball' =
      if state == NotRunning then
        ball
      else
        stepBall delta ball player1 player2

    player1' =
      stepPlayer delta paddle1 score1 player1

    player2' =
      stepPlayer delta paddle2 score2 player2
  in
    { game
      | state = state'
      , ball = ball'
      , player1 = player1'
      , player2 = player2'
    }


gameRunning : Signal Game
gameRunning =
  Signal.foldp stepGame defaultGame input



-- helper values


pongGreen =
  rgb 60 100 60


textGreen =
  rgb 160 200 160


txt f =
  leftAligned << f << monospace << Text.color textGreen << fromString


msg =
  "SPACE to start, WS and &uarr;&darr; to move"


pongCourt =
  filled pongGreen (rect gameWidth gameHeight)



-- shared function for rendering objects


displayMoveable : Moveable a -> Shape -> Form
displayMoveable moveable shape =
  move ( moveable.x, moveable.y ) (filled white shape)



-- display a game state


display : ( Int, Int ) -> Game -> Element
display ( w, h ) { state, ball, player1, player2 } =
  let
    scores : Element
    scores =
      toString player1.score
        ++ "  "
        ++ toString player2.score
        |> txt (Text.height 50)
  in
    container w h middle
      <| collage
          gameWidth
          gameHeight
          [ pongCourt
          , displayMoveable ball (oval 15 15)
          , displayMoveable player1 (rect 10 40)
          , displayMoveable player2 (rect 10 40)
          , toForm scores
              |> move ( 0, gameHeight / 2 - 40 )
          , toForm
              (if state == IsRunning then
                spacer 1 1
               else
                txt identity msg
              )
              |> move ( 0, 40 - gameHeight / 2 )
          ]


main =
  Signal.map2 display Window.dimensions gameRunning
