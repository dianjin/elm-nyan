module Update exposing (..)

import Model exposing (Model)
import Model.Scene exposing (..)
import Model.Ui exposing (Ui, keyPressed, Screen(..), pauseKeyCode, endKeyCode)
import Subscription exposing (Msg(..))

import Update.State exposing (..)
import Update.Key exposing (..)

import Keyboard exposing (KeyCode)
import Set exposing (insert, remove)
import Time exposing (Time)

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ ui, scene } as model) =
  case msg of
    Tick delta ->
      case ui.screen of
        PlayScreen ->
          tickPlayScreen delta model
        StartScreen ->
          tickStartScreen model
        PauseScreen ->
          tickPauseScreen model
        GameOverScreen ->
          tickGameOverScreen model
    UpdateScoreLog _ ->
      let
        -- Data
        { player, scoreLog } = scene

        -- Primes
        scoreLog' = player.score::scoreLog
        scene' = { scene | scoreLog = scoreLog' }
      in
        ({ model | scene = scene' }, Cmd.none)
    ResizeWindow newWindowSize ->
      let
        -- Data
        {projectiles, player} = scene
        {screen} = ui

        -- Primes
        player' =
          case screen of
            PlayScreen -> player
            _ -> centerPlayer newWindowSize player
        projectiles' = setProjectiles newWindowSize projectiles
        ui' = { ui | windowSize = newWindowSize }
        scene' =
          { scene
          | projectiles = projectiles'
          , player = player'
          }
      in
        ({ model | ui = ui', scene = scene' }, Cmd.none)
    KeyChange pressed keycode ->
      let
        -- Data
        {pressedKeys} = ui

        -- Primes
        pressedKeys' =
          if pressed then
            Set.insert keycode pressedKeys
          else
            Set.remove keycode pressedKeys
        ui' = { ui | pressedKeys = pressedKeys' }
      in
        ({ model | ui = ui' }, Cmd.none)
    StartGame ->
      resetGameState model
    EndGame ->
      let
        -- Data
        {screen} = ui
        -- Primes
        ui' = { ui | screen = GameOverScreen }
      in
        ({ model | ui = ui' }, Cmd.none)
    ResetProjectile projectile newWait ->
      let
        -- Data
        {projectiles} = scene
        {windowSize, playTime} = ui

        -- Functions
        projectileUpdater ({position} as projectile') =
          if position.y == projectile.position.y then
            projectile'
              |> setWaitAndFlavor newWait
              |> moveToRightEdge windowSize
          else
            projectile'

        -- Primes
        projectiles' = List.map projectileUpdater projectiles
        scene' = { scene | projectiles = projectiles' }

      in
        ({ model | scene = scene' }, Cmd.none)
    TogglePause ->
      togglePauseGameState model
    _ ->
      (model, Cmd.none)


tickGameOverScreen : Model -> (Model, Cmd Msg)
tickGameOverScreen model =
  (model, Cmd.none) |> updateIfStart

tickPauseScreen : Model -> (Model, Cmd Msg)
tickPauseScreen ({ui} as model) =
  (model, Cmd.none) |> updateIfResume |> updateIfEnd

tickPlayScreen : Time -> Model -> (Model, Cmd Msg)
tickPlayScreen delta ({ui} as model) =
  let
    {pressedKeys} = ui
  in
    if keyPressed pauseKeyCode pressedKeys then
      togglePauseGameState model
    else if keyPressed endKeyCode pressedKeys then
      endGameState model
    else
      playStateTransition delta model

tickStartScreen : Model -> (Model, Cmd Msg)
tickStartScreen model =
  (model, Cmd.none) |> updateIfStart
