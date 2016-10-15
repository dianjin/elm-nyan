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
    baseNodes =
      [ playerNode player
      , bannerNode player ui
      , announcementNode ui
      ]
    children =
      case screen of
        StartScreen ->
          baseNodes
        PlayScreen ->
          baseNodes
          ++ projectileNodes projectiles
        PauseScreen ->
          baseNodes
          ++ projectileNodes projectiles
        GameOverScreen ->
          baseNodes
          ++ projectileNodes projectiles
  in
    div
      [ style (divAttrs ui.windowSize) ]
      children

announcementNode : Ui -> Html Msg
announcementNode {windowSize, screen, playTime} =
  let
    divAttrs =
      [ ("position", "absolute")
      , ("top", "50%")
      , ("left", "50%")
      , ("width", "400px")
      , ("height", "60px")
      , ("margin", "-30px 0 0 -200px")
      , ("font-size", "40px")
      , ("text-align", "center")
      , ("z-index", "500")
      ]
    block = [("display", "block")]
    subAttrs = [("font-size", "20px"), ("padding", "5px 0 0 0")]
    heading txt =
      div [ style block ] [ text txt ]
    subHeading txt =
      div [ style (block ++ subAttrs) ] [ text txt ]
  in
    case screen of
      StartScreen ->
        div [ style divAttrs ]
          [ heading "elm-nyan"
          , subHeading "press G to play"
          ]
      GameOverScreen ->
        div [ style divAttrs ]
          [ heading "Game over"
          , subHeading (toString (playTime |> inSeconds |> round) ++ " seconds!")
          ]
      PauseScreen ->
        div [ style divAttrs ]
          [ text "Paused"
          , subHeading "press R to resume"
          ]
      _ ->
        div [] []

projectileNodes projectiles =
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
    projectileNode projectile =
      div [ style (projectileStyle projectile) ] []
  in
    List.map projectileNode projectiles

bannerNode : Player -> Ui -> Html Msg
bannerNode {score} {screen, windowSize, playTime} =
  let
    (startText, startOnClick) =
      case screen of
        StartScreen -> ("Start game (G)", StartGame)
        PlayScreen -> ("End game (X)", EndGame)
        _ -> ("Start game (G)", StartGame)
    pauseText =
      case screen of
        PlayScreen -> "Pause (P)"
        PauseScreen -> "Resume (R)"
        StartScreen -> "Pause (P)"
        _ -> "Pause (P)"
    (windowWidth, _) = windowSize
    outerAttrs =
      [ ("position", "fixed")
      , ("bottom", "0")
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
          [ style (inlineBlock True), onClick startOnClick ]
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

playerNode : Player -> Html Msg
playerNode {position} =
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
