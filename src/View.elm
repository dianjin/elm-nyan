module View exposing (view)


import Model exposing (Model)
import Model.Scene exposing (..)
import Model.Ui exposing (..)
import Subscription exposing (Msg(..))

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (fromChar)
import Time exposing (..)

view : Model -> Html Msg
view { ui, scene } =
  let
    {player, projectiles} = scene
    {screen, playTime, windowSize} = ui
    baseNodes =
      [ playerNode player
      , footerNode ui player
      , bannerNode ui
      ]
    childNodes =
      case screen of
        StartScreen -> baseNodes
        _ -> baseNodes ++ projectileNodes projectiles
  in
    div [ style (viewOuterAttrs windowSize) ] childNodes

bannerNode : Ui -> Html Msg
bannerNode {windowSize, screen, playTime} =
  let
    divHeading txt =
      div [ style bannerBlockAttrs ]
        [ text txt ]
    divSubHeading txt =
      div [ style (bannerBlockAttrs ++ bannerSubAttrs) ]
      [ text txt ]
    outerDiv heading subHeading =
      div [ style bannerOuterAttrs ]
        [ divHeading heading , divSubHeading subHeading ]
  in
    case screen of
      StartScreen ->
        outerDiv "elm-nyan"
          ("press " ++ fromChar startKey ++ " to play")
      GameOverScreen ->
        outerDiv "Game over"
          (toString (playTime |> inSeconds |> round) ++ " seconds!")
      PauseScreen ->
        outerDiv "Paused"
          ("press " ++ fromChar resumeKey ++ " to resume")
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

footerNode : Ui -> Player -> Html Msg
footerNode {screen, windowSize, playTime} {score} =
  let
    (startText, startOnClick) =
      startButtonData screen
    (pauseText, pauseOnClick) =
      pauseButtonData screen
    (windowWidth, _) = windowSize

  in
    div [ style footerOuterAttrs ]
      [ div [ style footerWrapperAttrs ]
        [ div [ style (footerInnerAttrs True), onClick startOnClick ]
          [ text startText ]
        , div [ style (footerInnerAttrs True), onClick pauseOnClick ]
          [ text pauseText ]
        , div [ style (footerInnerAttrs False) ]
          [ text "Seconds" ]
        , div [ style (footerInnerAttrs False) ]
          [ text (playTime |> inSeconds |> round |> toString) ]
        , div [ style (footerInnerAttrs False) ]
          [ text "Energy" ]
        , div [ style (footerInnerAttrs False) ]
          [ text (score |> toString) ]
        ]
      ]

playerNode : Player -> Html Msg
playerNode {position} =
  img
    [ src "assets/nyan.gif"
    , style (playerImgAttrs playerSize position)
    ] []

-- Styles

type alias Styles = List (String, String)

viewOuterAttrs : WindowSize -> Styles
viewOuterAttrs (w, h) =
  [ ("background-color", "navy")
  , ("height", toString h ++ "px")
  , ("width", toString w ++ "px")
  ]

bannerOuterAttrs : Styles
bannerOuterAttrs =
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

bannerBlockAttrs =
  [ ("display", "block")
  ]

bannerSubAttrs =
  [ ("font-size", "20px")
  , ("padding", "5px 0 0 0")
  ]

footerOuterAttrs =
  [ ("position", "fixed")
  , ("bottom", "0")
  , ("width", "100%")
  , ("font-size", "16px")
  , ("padding-bottom", "30px")
  ]

footerWrapperAttrs =
  [ ("width", "500px")
  , ("margin", "0 auto")
  ]

footerInnerAttrs isLink =
  let
    cursor =
      ("cursor", if isLink then "pointer" else "default")
    float =
      ("float", if isLink then "right" else "left")
  in
    cursor::float::
      [("padding", "0 10px"), ("display", "inline-block")]

playerImgAttrs (w, h) {x, y} =
  [ ("position", "absolute")
  , ("top", toString y ++ "px")
  , ("left", "-20px")
  , ("width", toString w ++ "px")
  , ("height", toString h ++ "px")
  ]

-- Helpers

startButtonData : Screen -> (String, Msg)
startButtonData screen =
  let
    startTuple =
      ("Start game (" ++ fromChar startKey ++ ")", StartGame)
    endTuple =
      ("End game (" ++ fromChar endKey ++ ")", EndGame)
  in
    case screen of
      StartScreen -> startTuple
      GameOverScreen -> startTuple
      _ -> endTuple

pauseButtonData : Screen -> (String, Msg)
pauseButtonData screen =
  case screen of
    PlayScreen ->
      ("Pause (" ++ fromChar pauseKey ++ ")", TogglePause)
    PauseScreen ->
      ("Resume (" ++ fromChar resumeKey ++ ")", TogglePause)
    _ ->
      (" -- ", NoOp)
