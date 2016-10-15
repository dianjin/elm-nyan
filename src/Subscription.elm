module Subscription exposing (..)

import Model exposing (..)
import Window
import AnimationFrame
import Keyboard exposing (KeyCode)
import Set
import Task
import Time exposing (Time, second)
import Model.Scene exposing (..)
import Model.Ui exposing (..)

type Msg
  = Tick Time
  | UpdateScoreLog Time
  | ResizeWindow (Int, Int)
  | KeyChange Bool KeyCode
  | StartGame
  | TogglePause
  | EndGame
  | ResetProjectile Projectile Int
  | NoOp

subscriptions : Model -> Sub Msg
subscriptions { ui } =
  let
    baseSubscriptions =
      [ Window.resizes (\{width, height} -> ResizeWindow (width, height))
      , Keyboard.downs (KeyChange True)
      , Keyboard.ups (KeyChange False)
      ]
  in
    let subscriptions =
      case ui.screen of
        PlayScreen ->
          let
            animation = AnimationFrame.diffs Tick
            secondTick = Time.every second UpdateScoreLog
          in
            baseSubscriptions ++ [ animation, secondTick ]
        _ ->
          baseSubscriptions
    in subscriptions |> Sub.batch

initialWindowSizeCommand : Cmd Msg
initialWindowSizeCommand =
  Task.perform (\_ -> NoOp) (\{width, height} -> ResizeWindow (width,height)) Window.size
