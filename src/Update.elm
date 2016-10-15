module Update exposing (..)

import Model exposing (..)
import Model.Geometry exposing (..)
import Model.Scene exposing (..)
import Model.Ui exposing (..)
import Subscription exposing (..)

import Keyboard exposing (KeyCode)
import Random
import Set
import Time exposing (Time, second)

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ ui, scene } as model) =
  case msg of
    Tick delta ->
      let
        -- Data
        {player, projectiles} = scene
        {screen, pressedKeys, windowSize} = ui

        -- Functions
        updateProjectile : Projectile -> (Projectile, Cmd Msg)
        updateProjectile projectile =
          if hasReachedLeftEdge projectile then
            (projectile, projectileToResetCommand projectile)
          else if intersectWithPlayer player projectile then
            (projectile, projectileToResetCommand projectile)
          else
            (waitOrMoveProjectile delta projectile, Cmd.none)

        -- Primes
        player' = player
          |> steerPlayer pressedKeys
          |> placePlayer delta windowSize
        (projectiles', commands) = projectiles
          |> List.map updateProjectile
          |> List.unzip
        scene' =
          { scene
          | player = player'
          , projectiles = projectiles'
          }
      in
        case screen of
          PlayScreen ->
            ({ model | scene = scene' }, commands |> Cmd.batch)
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
        ui' = { ui | screen = PlayScreen}

      in
        ({ model | ui = ui' }, commands |> Cmd.batch)
    ResetProjectile projectile newWait ->
      let
        -- Data
        {projectiles} = scene
        {windowSize} = ui

        -- Functions
        projectileUpdater ({position} as projectile') =
          if position.y == projectile.position.y then
            projectile'
              |> setWait newWait
              |> moveToRightEdge windowSize
              |> setVelocity projectileVelocity
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

setWait : Int -> Projectile -> Projectile
setWait newWait projectile =
  { projectile | wait = newWait }

setVelocity : Vector -> Projectile -> Projectile
setVelocity newVelocity projectile =
  { projectile | velocity = newVelocity }

moveToRightEdge : (Int, Int) -> Projectile -> Projectile
moveToRightEdge (windowWidth, _) ({position} as projectile) =
  let {x, y} = position
  in { projectile | position = { x = toFloat windowWidth, y = y } }

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
    box = 70
    (playerWidth, playerHeight) = playerSize
    playerX = player.position.x + playerWidth - 85
    playerY = player.position.y + playerHeight - 85
    {x, y} = projectile.position
    projectileX = x + 15
    projectileY = y + 15
    yTop = projectileY + 85 >= playerY
    yBottom = playerY + 85 >= projectileY
    xCond = playerX + box >= projectileX
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
