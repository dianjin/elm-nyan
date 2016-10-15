module View exposing (view)

import Html exposing (..)

import Html.Attributes exposing (..)
import Html.Events exposing (..)

import VirtualDom
import Json.Encode as Json
import Model exposing (..)
import Model.Scene exposing (..)
import Model.Ui exposing (..)
import Subscription exposing (Msg(..))

view : Model -> Html Msg
view { ui, scene } =
  let
    {player, projectiles} = scene
    {screen} = ui
    projectileNodes =
      case screen of
        PlayScreen -> renderProjectiles projectiles
        _ -> []
    children =
      [ renderPlayer player
      , renderBanner ui
      ] ++ projectileNodes
  in
    div
      [ style (divAttrs ui.windowSize) ]
      children

renderProjectiles projectiles =
  let
    (projectileWidth, projectileHeight) = projectileSize
    projectileStyle {x, y} =
      [ ("width", toString projectileWidth ++ "px")
      , ("height", toString projectileHeight ++ "px")
      , ("background-color", "yellow")
      , ("position", "absolute")
      , ("top", toString y ++ "px")
      , ("left", toString x ++ "px")
      ]
    renderProjectile { position } =
      div
        [ style (projectileStyle position) ]
        [ text (toString position.y) ]
  in
    List.map renderProjectile projectiles

renderBanner : Ui -> Html Msg
renderBanner {screen} =
  let
    isDisabled =
      case screen of
        StartScreen -> False
        PlayScreen -> True
        _ -> False
    styleAttrs =
      [ ("margin", "auto")
      , ("top", "0px")
      , ("background-color", "yellow")
      , ("width", "200px")
      ]
  in
    div
      [ style styleAttrs, onClick StartGame ]
      [ button [ disabled isDisabled ] [ text "Start" ] ]

renderPlayer : Player -> Html Msg
renderPlayer {position} =
  let
    {x, y} = position
    (w, h) = playerSize
    imgAttrs =
      [ ("position", "absolute")
      , ("top", toString y ++ "px")
      , ("left", "-20px")
      , ("border", "1px solid white")
      , ("width", toString w ++ "px")
      , ("height", toString h ++ "px")
      ]
  in
    img
      [ src "assets/nyan.gif"
      , style (imgAttrs)
      ]
      []

divAttrs (w, h) =
  [ ("background-color", "navy")
  , ("height", toString h ++ "px")
  , ("width", toString w ++ "px")
  ]
