module Model exposing (..)

import Model.Ui exposing (..)
import Model.Scene exposing (..)


type alias Model =
  { ui : Ui
  , scene: Scene
  }


initialModel : Model
initialModel =
  { ui = initialUi
  , scene = initialScene
  }
