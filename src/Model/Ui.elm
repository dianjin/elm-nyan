module Model.Ui exposing (..)

import Set exposing (Set)
import Keyboard exposing (KeyCode)
import Time exposing (Time)

type alias KeySet = Set KeyCode

type alias Ui =
  { windowSize : WindowSize
  , pressedKeys : KeySet
  , screen : Screen
  , playTime : Time
  }

type alias WindowSize = (Int, Int)

type Screen
  = StartScreen
  | PlayScreen
  | PauseScreen
  | GameOverScreen

initialUi : Ui
initialUi =
  { windowSize = (0, 0)
  , pressedKeys = Set.empty
  , screen = StartScreen
  , playTime = 0
  }


keyPressed : KeyCode -> Set KeyCode -> Bool
keyPressed keycode pressedKeys =
  Set.member keycode pressedKeys
