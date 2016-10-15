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
        {player, projectiles} = scene

        -- Functions
        updateProjectile : Projectile -> (Projectile, Cmd Msg)
        updateProjectile ({position} as projectile) =
          if hasReachedLeftEdge projectile then
            let
              resetWait = Random.generate
                (ResetProjectile position)
                (Random.int minWait maxWait)
            in
              (projectile, resetWait)
          else
            (waitOrMoveProjectile delta projectile, Cmd.none)

        -- Players
        player' = steerPlayer delta ui player
        player'' = placePlayer delta ui player'

        -- Projectiles
        (projectiles', commands) = List.map updateProjectile projectiles
          |> List.unzip

        -- Scene
        scene' =
          { scene
          | player = player''
          , projectiles = projectiles'
          }
      in
        ({ model | scene = scene' }, commands |> Cmd.batch)
    ResizeWindow newSizeTuple ->
      let
        {projectiles} = scene
        newProjectiles = calculateProjectiles newSizeTuple projectiles
        newModel =
          { model
          | ui = { ui | windowSize = newSizeTuple }
          , scene = { scene | projectiles = newProjectiles }
          }
      in
        (newModel, Cmd.none)
    KeyChange pressed keycode ->
      (handleKeyChange pressed keycode model, Cmd.none)
    StartGame ->
      let
        {projectiles} = scene
        randomListGenerator =
          Random.generate
            DispatchProjectiles
            (Random.list (List.length projectiles) (Random.int minWait maxWait))
        ui' = { ui | screen = PlayScreen }
      in
        ({ model | ui = ui' }, randomListGenerator)
    DispatchProjectiles randomList ->
      let
        {projectiles} = scene
        fn1 = setWait randomList
        fn2 = setVelocity projectileVelocity
        fn3 = setPosition ui.windowSize
        newProjectiles = projectiles |> fn1 |> fn2 |> fn3
        newModel =
          { model
          | scene = { scene | projectiles = newProjectiles }
        }
      in
        (newModel, Cmd.none)
    ResetProjectile {x, y} newWait ->
      let
        {projectiles} = scene
        {windowSize} = ui
        projectileUpdater ({position} as projectile) =
          if position.y == y then
            { projectile | wait = newWait } |> (resetProjectilePosition windowSize)
          else
            projectile
        projectiles' = List.map projectileUpdater projectiles
        scene' = { scene | projectiles = projectiles' }
      in
        ({ model | scene = scene' }, Cmd.none)
    _ ->
      (model, Cmd.none)

setWait : List Int -> List Projectile -> List Projectile
setWait randomList projectiles =
  let
    update randomInt projectile =
      { projectile | wait = randomInt }
  in
    List.map2 update randomList projectiles

setVelocity : Vector -> List Projectile -> List Projectile
setVelocity newVelocity projectiles =
  let
    update projectile  =
      { projectile | velocity = newVelocity }
  in
    List.map update projectiles

resetProjectilePosition : (Int, Int) -> Projectile -> Projectile
resetProjectilePosition (windowWidth, _) ({position} as projectile) =
  let {x, y} = position
  in { projectile | position = { x = toFloat windowWidth, y = y } }

setPosition : (Int, Int) -> List Projectile -> List Projectile
setPosition windowSize projectiles =
  List.map (resetProjectilePosition windowSize) projectiles

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

waitOrMoveProjectile : Time -> Projectile -> Projectile
waitOrMoveProjectile delta ({wait} as projectile) =
  if wait > 0 then
    decrementWait projectile
  else
    moveProjectile delta projectile

calculateProjectiles : (Int, Int) -> List Projectile -> List Projectile
calculateProjectiles (_, windowHeight) existingProjectiles =
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

steerPlayer : Time -> Ui -> Player -> Player
steerPlayer delta {pressedKeys} ({velocity} as player) =
  let
    directionY =
      if keyPressed 38 pressedKeys then -1
      else if keyPressed 40 pressedKeys then 1
      else 0
    vy = directionY
    velocity' = { velocity | y = vy }
  in
    { player | velocity = velocity' }

placePlayer : Time -> Ui -> Player -> Player
placePlayer delta {windowSize} ({position, velocity} as player) =
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

handleKeyChange : Bool -> KeyCode -> Model -> Model
handleKeyChange pressed keycode ({ui} as model) =
  let
    updateKeys =
      if pressed then Set.insert else Set.remove
    pressedKeys' = updateKeys keycode ui.pressedKeys
    ui' = { ui | pressedKeys = pressedKeys' }
  in
    { model | ui = ui' }
