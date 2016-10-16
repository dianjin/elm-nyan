module Subscription exposing (..)

import Model exposing (..)
import Model.Scene exposing (Projectile)
import Model.Ui exposing (Ui, Screen(..), WindowSize)

import AnimationFrame
import Keyboard exposing (KeyCode)
import Set
import Task
import Time exposing (Time, second)
import Window

type Msg
  = Tick Time
  | UpdateScoreLog Time
  | ResizeWindow WindowSize
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
      , AnimationFrame.diffs Tick
      ]
  in
    let subscriptions =
      case ui.screen of
        PlayScreen ->
          let
            secondTick = Time.every second UpdateScoreLog
          in
            secondTick::baseSubscriptions
        _ ->
          baseSubscriptions
    in subscriptions |> Sub.batch

initialWindowSizeCommand : Cmd Msg
initialWindowSizeCommand =
  Task.perform (\_ -> NoOp) (\{width, height} -> ResizeWindow (width,height)) Window.size
