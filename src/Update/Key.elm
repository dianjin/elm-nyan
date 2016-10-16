module Update.Key exposing (..)

import Keyboard exposing (KeyCode)
import Model exposing (Model)
import Subscription exposing (Msg)
import Model.Ui exposing (keyPressed, pauseKeyCode, startKeyCode, resumeKeyCode, endKeyCode)
import Update.State exposing (..)

type alias StateTransition = (Model, Cmd Msg) -> (Model, Cmd Msg)
updateIfPressed : KeyCode -> (Model -> (Model, Cmd Msg)) -> StateTransition
updateIfPressed keycode updater (({ui} as model), cmd) =
  let
    {pressedKeys} = ui
  in
    if keyPressed keycode pressedKeys then
      updater model
    else
      (model, cmd)

updateIfPause : StateTransition
updateIfPause modelAndCmd =
  updateIfPressed pauseKeyCode togglePauseGameState modelAndCmd

updateIfResume : StateTransition
updateIfResume modelAndCmd =
  updateIfPressed resumeKeyCode togglePauseGameState modelAndCmd

updateIfStart : StateTransition
updateIfStart modelAndCmd =
  updateIfPressed startKeyCode resetGameState modelAndCmd

updateIfEnd : StateTransition
updateIfEnd modelAndCmd =
  updateIfPressed endKeyCode endGameState modelAndCmd
