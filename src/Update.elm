module Update exposing (..)

import Model exposing (..)
import Model.Geometry exposing (..)
import Model.Scene exposing (..)
import Model.Ui exposing (..)
import Subscription exposing (..)

import Keyboard exposing (KeyCode)
import Random
import Set
import Time exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ ui, scene } as model) =
  case msg of
    Tick delta ->
      let
        -- Data
        {player, projectiles} = scene
        {screen, pressedKeys, windowSize, playTime} = ui

        -- Functions
        updateProjectile : Projectile -> (Projectile, Player -> Player, Cmd Msg)
        updateProjectile projectile =
          if hasReachedLeftEdge projectile then
            let playerUpdater = applyDodgeScore projectile
            in (projectile, playerUpdater, projectileToResetCommand projectile)
          else if intersectWithPlayer player projectile then
            let playerUpdater = applyCollisionScore projectile
            in (projectile, playerUpdater, projectileToResetCommand projectile)
          else
            (waitOrMoveProjectile delta projectile, identity, Cmd.none)

        -- Primes
        playerUpdater = List.foldl (>>) identity playerUpdaters
        player' = player
          |> steerPlayer pressedKeys
          |> placePlayer delta windowSize
          |> playerUpdater
        (projectiles', playerUpdaters, commands) = projectiles
          |> List.map updateProjectile
          |> unzip3
        scene' =
          { scene
          | player = player'
          , projectiles = projectiles'
          }
      in
        case screen of
          PlayScreen ->
            let
              ui' = { ui | playTime = playTime + delta }
              model' =
                { model
                | scene = scene'
                , ui = ui'
                }
            in
              (model', commands |> Cmd.batch)
          PauseScreen ->
            (model, Cmd.none)
          _ ->
            (model, Cmd.none)

    ResizeWindow newSizeTuple ->
      let
        -- Data
        {projectiles} = scene

        -- Primes
        projectiles' = updateProjectiles newSizeTuple projectiles
        model' =
          { model
          | ui = { ui | windowSize = newSizeTuple }
          , scene = { scene | projectiles = projectiles' }
          }
      in
        (model', Cmd.none)
    KeyChange pressed keycode ->
      let
        -- Data
        {pressedKeys} = ui

        -- Functions
        updateKeys = if pressed then Set.insert else Set.remove

        -- Primes
        pressedKeys' = updateKeys keycode pressedKeys
        ui' = { ui | pressedKeys = pressedKeys' }
      in
        ({ model | ui = ui' }, Cmd.none)
    StartGame ->
      let
        -- Data
        {projectiles} = scene
        {screen} = ui

        -- Primes
        commands = List.map projectileToResetCommand projectiles
        ui' = { ui | screen = PlayScreen }

      in
        ({ model | ui = ui' }, commands |> Cmd.batch)
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
              |> setVelocity (playTimeToVelocity playTime)
          else
            projectile'

        -- Primes
        projectiles' = List.map projectileUpdater projectiles
        scene' = { scene | projectiles = projectiles' }

      in
        ({ model | scene = scene' }, Cmd.none)
    TogglePause ->
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
    _ ->
      (model, Cmd.none)


-- Projectile updaters

setWaitAndFlavor : Int -> Projectile -> Projectile
setWaitAndFlavor newWait projectile =
  let flavor' = if rem newWait 2 == 0 then Good else Bad
  in { projectile | wait = newWait, flavor = flavor' }

setVelocity : Vector -> Projectile -> Projectile
setVelocity newVelocity projectile =
  { projectile | velocity = newVelocity }

moveToRightEdge : (Int, Int) -> Projectile -> Projectile
moveToRightEdge (windowWidth, _) ({position} as projectile) =
  { projectile | position = { position | x = toFloat windowWidth } }

decrementWait : Projectile -> Projectile
decrementWait ({ wait } as projectile) =
  { projectile | wait = wait-1 }

moveProjectile : Time -> Projectile -> Projectile
moveProjectile delta ({position, velocity} as projectile) =
  let
    {x, y} = position
    vx = velocity.x
    dx = delta * vx
    x' = x + dx
  in
    { projectile | position = { x = x', y = y } }

hasReachedLeftEdge : Projectile -> Bool
hasReachedLeftEdge {position} =
  let (projectileWidth, _) = projectileSize
  in position.x < -1 * projectileWidth

intersectWithPlayer : Player -> Projectile -> Bool
intersectWithPlayer player projectile =
  let
    (playerWidth, playerHeight) = playerSize
    playerX = player.position.x + playerWidth - 85
    playerY = player.position.y + playerHeight - 85
    {x, y} = projectile.position
    projectileX = x + 15
    projectileY = y + 15
    yTop = projectileY + 85 >= playerY
    yBottom = playerY + 85 >= projectileY
    xCond = playerX + 85 >= projectileX
    yCond = yTop && yBottom
  in
    yCond && xCond

waitOrMoveProjectile : Time -> Projectile -> Projectile
waitOrMoveProjectile delta ({wait} as projectile) =
  if wait > 0 then
    decrementWait projectile
  else
    moveProjectile delta projectile

updateProjectiles : (Int, Int) -> List Projectile -> List Projectile
updateProjectiles (_, windowHeight) existingProjectiles =
  let
    (_, projectileHeight) = projectileSize
    existingLength = List.length existingProjectiles
    targetLength = windowHeight // round projectileHeight
  in
    if targetLength < existingLength then
      List.take targetLength existingProjectiles
    else if targetLength > existingLength then
      List.append
        existingProjectiles
        (List.map
          defaultProjectile
          [existingLength..targetLength-1])
    else
      existingProjectiles

-- Player updaters

applyCollisionScore : Projectile -> Player -> Player
applyCollisionScore {flavor} ({score} as player) =
  let
    worth = 50
    scoreDelta = if flavor == Good then worth else -1 * worth
  in
    { player | score = score + scoreDelta }

applyDodgeScore : Projectile -> Player -> Player
applyDodgeScore {flavor} ({score} as player) =
  let
    worth = 50
    scoreDelta = if flavor == Bad then worth else 0
  in
    { player | score = score + scoreDelta }

steerPlayer : Set.Set KeyCode -> Player -> Player
steerPlayer pressedKeys ({velocity} as player) =
  let
    directionY =
      if keyPressed 38 pressedKeys then -1
      else if keyPressed 40 pressedKeys then 1
      else 0
    vy = directionY
    velocity' = { velocity | y = vy }
  in
    { player | velocity = velocity' }

placePlayer : Time -> (Int, Int) -> Player -> Player
placePlayer delta windowSize ({position, velocity} as player) =
  let
    (_, playerHeight) = playerSize
    (_, windowHeight) = windowSize
    maxY = (toFloat windowHeight) - playerHeight - 2
    {x, y} = position
    vx = velocity.x
    vy = velocity.y
    dy = vy * delta
    y' = y + dy
    y'' =
      if y' >= maxY then
        maxY
      else if y' <= 0 then
        0
      else
        y'
    position' = { x = x, y = y'' }
  in
    { player | position = position' }

projectileToResetCommand : Projectile -> Cmd Msg
projectileToResetCommand projectile =
  Random.generate
    (ResetProjectile projectile)
    (Random.int minWait maxWait)

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
