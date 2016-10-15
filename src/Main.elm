module Main exposing (..)

import Html.App as Html
import Model exposing (Model, initialModel)
import Subscription exposing (subscriptions, initialWindowSizeCommand)
import Update exposing (update)
import View exposing (view)

main =
  Html.program
    { init = (initialModel, initialWindowSizeCommand)
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
