module Subscription exposing (..)

import Model exposing (..)
import Window
import AnimationFrame
import Keyboard exposing (KeyCode)
import Set
import Task
import Time exposing (Time, second)
import Model.Scene exposing (..)

type Msg
  = Tick Time
  | ResizeWindow (Int, Int)
  | KeyChange Bool KeyCode
  | StartGame
  | DispatchProjectiles (List Int)
  | ResetProjectile Projectile Int
  | TogglePause
  | NoOp

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    window = Window.resizes (\{width, height} -> ResizeWindow (width, height))
    animation = AnimationFrame.diffs Tick
    keyDown = Keyboard.downs (KeyChange True)
    keyUp = Keyboard.ups (KeyChange False)
  in
    [ window
    , animation
    , keyDown
    , keyUp
    ] |> Sub.batch

initialWindowSizeCommand : Cmd Msg
initialWindowSizeCommand =
  Task.perform (\_ -> NoOp) (\{width, height} -> ResizeWindow (width,height)) Window.size
