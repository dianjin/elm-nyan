module Model.Ui exposing (..)

import Set exposing (Set)
import Keyboard exposing (KeyCode)
import Time exposing (Time)

import Model.Scene exposing (..)

type alias Ui =
  { windowSize : (Int, Int)
  , pressedKeys : Set KeyCode
  , screen : Screen
  , playTime : Time
  }


type Screen
  = StartScreen
  | PlayScreen
  | PauseScreen
  | GameOverScreen

initialUi : Ui
initialUi =
  { windowSize = (500,500)
  , pressedKeys = Set.empty
  , screen = StartScreen
  , playTime = 0
  }


keyPressed : KeyCode -> Set KeyCode -> Bool
keyPressed keycode pressedKeys =
  Set.member keycode pressedKeys
