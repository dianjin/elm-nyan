module Update.Key exposing (..)

import Keyboard exposing (KeyCode)
import Model exposing (Model)
import Subscription exposing (Msg)
import Model.Ui exposing (keyPressed)
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
  updateIfPressed 80 togglePauseGameState modelAndCmd

updateIfResume : StateTransition
updateIfResume modelAndCmd =
  updateIfPressed 82 togglePauseGameState modelAndCmd

updateIfStart : StateTransition
updateIfStart modelAndCmd =
  updateIfPressed 78 resetGameState modelAndCmd

updateIfEnd : StateTransition
updateIfEnd modelAndCmd =
  updateIfPressed 81 endGameState modelAndCmd
