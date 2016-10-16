module Update.State exposing (..)

import Model exposing (Model)
import Model.Scene exposing (..)
import Model.Ui exposing (Ui, Screen(..))
import Subscription exposing (Msg(..))
import Settings exposing (numFlavors)

import Random exposing (generate, int)
import Time exposing (Time)

projectileToResetCommand : Projectile -> Cmd Msg
projectileToResetCommand projectile =
  Random.generate
    (ResetProjectile projectile)
    (Random.int 1 numFlavors)

updateProjectile : Time -> Ui -> Player -> Projectile -> (Projectile, Player -> Player, Cmd Msg)
updateProjectile delta { playTime } player projectile =
  let
    speed' = playTimeToSpeed playTime
  in
    if hasReachedLeftEdge projectile then
      let playerUpdater = applyMissScore projectile
      in (projectile, playerUpdater, projectileToResetCommand projectile)
    else if intersectWithPlayer player projectile then
      let playerUpdater = applyCollisionScore projectile
      in (projectile, playerUpdater, projectileToResetCommand projectile)
    else
      (waitOrMoveProjectile delta speed' projectile, identity, Cmd.none)


playStateTransition : Time -> Model -> (Model, Cmd Msg)
playStateTransition delta ({scene, ui} as model) =
  let
    -- Data
    {player, projectiles} = scene
    {screen, pressedKeys, windowSize, playTime} = ui

    -- Primes
    (projectiles', playerUpdaters, commands) = projectiles
      |> List.map (updateProjectile delta ui player)
      |> unzip3
    playerUpdater = List.foldl (>>) identity playerUpdaters
    player' = player
      |> steerPlayer pressedKeys
      |> placePlayer delta ui
      |> playerUpdater
    screen' =
      if insufficientScore player' then
        GameOverScreen
      else
        screen
    scene' =
      { scene
      | player = player'
      , projectiles = projectiles'
      }
    ui' =
      { ui
      | playTime = playTime + delta
      , screen = screen'
      }
    model' =
      { model
      | scene = scene'
      , ui = ui'
      }
  in
    (model', commands |> Cmd.batch)

endGameState : Model -> (Model, Cmd Msg)
endGameState ({ui} as model) =
  let
    -- Data
    {screen} = ui
    -- Primes
    ui' = { ui | screen = GameOverScreen }
  in
    ({ model | ui = ui' }, Cmd.none)

togglePauseGameState : Model -> (Model, Cmd Msg)
togglePauseGameState ({ui} as model) =
  let
    -- Data
    {screen} = ui

    -- Primes
    screen' =
      case screen of
        PlayScreen -> PauseScreen
        PauseScreen -> PlayScreen
        _ -> PauseScreen
    ui' = { ui | screen = screen' }
  in
    ({ model | ui = ui' }, Cmd.none)

resetGameState : Model -> (Model, Cmd Msg)
resetGameState ({scene, ui} as model) =
  let
    -- Data
    {projectiles, player} = scene
    {screen, windowSize, pressedKeys} = ui

    -- Primes
    commands = List.map projectileToResetCommand projectiles
    ui' =
      { ui
      | screen = PlayScreen
      , playTime = 0
      }
    player' = player |> resetScore
    scene' =
      { scene
      | player = player'
      , scoreLog = [ defaultScore ]
      }
  in
    ({ model | scene = scene', ui = ui' }, commands |> Cmd.batch)

unzip3 : List (a, b, c) -> (List a, List b, List c)
unzip3 xs = case xs of
  [] ->
    ([], [], [])
  x::xs ->
    let
      (x1, x2, x3) = x
      (l1, l2, l3) = unzip3 xs
    in
      (x1::l1, x2::l2, x3::l3)
