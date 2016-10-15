module View exposing (view)

import Html exposing (..)

import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)

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
    {screen, playTime} = ui
    projectileNodes =
      case screen of
        StartScreen -> []
        _ -> renderProjectiles projectiles
    children =
      [ renderPlayer player
      , renderBanner player ui
      ] ++ projectileNodes
  in
    div
      [ style (divAttrs ui.windowSize) ]
      children

renderProjectiles projectiles =
  let
    (projectileWidth, projectileHeight) = projectileSize
    projectileStyle {position, flavor} =
      let
        {x, y} = position
        color' = if flavor == Good then "green" else "red"
      in
        [ ("width", toString projectileWidth ++ "px")
        , ("height", toString projectileHeight ++ "px")
        , ("background-color", color')
        , ("position", "absolute")
        , ("top", toString y ++ "px")
        , ("left", toString x ++ "px")
        ]
    renderProjectile projectile =
      div [ style (projectileStyle projectile) ] []
  in
    List.map renderProjectile projectiles

renderBanner : Player -> Ui -> Html Msg
renderBanner {score} {screen, windowSize, playTime} =
  let
    startText =
      case screen of
        StartScreen -> "New game"
        PlayScreen -> "End game"
        _ -> "New game"
    pauseText =
      case screen of
        PlayScreen -> "Pause"
        PauseScreen -> "Resume"
        StartScreen -> "Pause"
        _ -> "Pause"
    (windowWidth, _) = windowSize
    outerAttrs =
      [ ("position", "fixed")
      , ("bottom", "0")
      , ("color", "white")
      , ("width", "100%")
      , ("font-size", "16px")
      , ("padding", "20px")
      ]
    inlineBlock isLink =
      let
        cursor = ("cursor", if isLink then "pointer" else "default")
        float = ("float", if isLink then "right" else "left")
      in cursor::float::[("padding", "0 10px"), ("display", "inline-block")]
  in
    div
      [ style outerAttrs ]
      [ div
        [ style [("width", "500px"), ("margin", "0 auto")] ]
        [ div
          [ style (inlineBlock True), onClick StartGame ]
          [ text startText ]
        , div
          [ style (inlineBlock True), onClick TogglePause ]
          [ text pauseText ]
        , div
          [ style (inlineBlock False) ]
          [ text "Seconds" ]
        , div
          [ style (inlineBlock False) ]
          [ text (playTime |> inSeconds |> round |> toString) ]
        , div
          [ style (inlineBlock False) ]
          [ text "Score" ]
        , div
          [ style (inlineBlock False) ]
          [ text (score |> toString) ]
        ]
      ]

renderPlayer : Player -> Html Msg
renderPlayer {position} =
  let
    {x, y} = position
    (w, h) = playerSize
    imgAttrs =
      [ ("position", "absolute")
      , ("top", toString y ++ "px")
      , ("left", "-20px")
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
