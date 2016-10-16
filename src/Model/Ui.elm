module Model.Ui exposing (..)

import Char exposing (toCode)
import Set exposing (Set)
import Keyboard exposing (KeyCode)
import Time exposing (Time)

-- Ui

type alias Ui =
  { windowSize : WindowSize
  , pressedKeys : KeySet
  , screen : Screen
  , playTime : Time
  }

initialUi : Ui
initialUi =
  { windowSize = (0, 0)
  , pressedKeys = Set.empty
  , screen = StartScreen
  , playTime = 0
  }

-- Supporting types

type Screen
  = StartScreen
  | PlayScreen
  | PauseScreen
  | GameOverScreen
type alias KeySet = Set KeyCode
type alias WindowSize = (Int, Int)

-- Constants

startKey = 'N'
startKeyCode = toCode startKey
pauseKey = 'P'
pauseKeyCode = toCode pauseKey
resumeKey = 'R'
resumeKeyCode = toCode resumeKey
endKey = 'Q'
endKeyCode = toCode endKey

-- Helpers

keyPressed : KeyCode -> Set KeyCode -> Bool
keyPressed keycode pressedKeys =
  Set.member keycode pressedKeys
