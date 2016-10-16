module Model exposing (..)

import Model.Ui exposing (Ui, initialUi)
import Model.Scene exposing (Scene, initialScene)

type alias Model =
  { ui : Ui
  , scene: Scene
  }


initialModel : Model
initialModel =
  { ui = initialUi
  , scene = initialScene
  }
