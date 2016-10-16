module Model.Scene exposing (..)

import Model.Ui exposing (Ui, WindowSize, keyPressed, KeySet)
import Settings exposing (playerSize, projectileSize, numFlavors)

import Time exposing (Time, inSeconds)

-- Scene

type alias Scene =
  { player : Player
  , projectiles: List Projectile
  , scoreLog : List Score
  }

initialScene : Scene
initialScene =
  { player = initialPlayer
  , projectiles = []
  , scoreLog = []
  }

-- Player

type alias Player =
  { score : Score
  , position : Position
  , direction : Direction
  }

initialPlayer : Player
initialPlayer =
  { score = defaultScore
  , position = { x = 0, y = 0 }
  , direction = Rest
  }

-- Projectile

type alias Projectile =
  { wait : Wait
  , position : Position
  , flavor : Flavor
  }

initialProjectile : Int -> Projectile
initialProjectile number =
  let
    (_, projectileHeight) = projectileSize
    y = projectileHeight * (toFloat number)
  in
    { wait = 0
    , position = { x = -200, y = y }
    , flavor = Good 0
    }

-- Supporting Types

type Direction = Up | Down | Rest
type Flavor = Good Int | Bad Int
type alias Position = { x : Float , y : Float }
type alias Score = Int
type alias Speed = Float
type alias Wait = Int

-- Constants

defaultScore = 100
baseSpeed = 0.06

-- Player updaters

resetScore : Player -> Player
resetScore player =
  { player | score = defaultScore }

centerPlayer : WindowSize -> Player -> Player
centerPlayer (_, windowHeight) ({position} as player) =
  let
    (_, playerHeight) = playerSize
    h = (windowHeight // 2) - ((round playerHeight) // 2)
  in
    { player | position = { position | y = toFloat h } }

applyCollisionScore : Projectile -> Player -> Player
applyCollisionScore {flavor} ({score} as player) =
  let
    worth = 50
    scoreDelta =
      case flavor of
        Good _ -> worth
        _ -> -1 * worth
  in
    { player | score = score + scoreDelta }

applyMissScore : Projectile -> Player -> Player
applyMissScore {flavor} ({score} as player) =
  let
    punishment = 24
    scoreDelta =
      case flavor of
        Good _ -> -1 * punishment
        _ -> 0
  in
    { player | score = score + scoreDelta }

steerPlayer : KeySet -> Player -> Player
steerPlayer pressedKeys player =
  let
    direction' =
      if keyPressed 38 pressedKeys then Up
      else if keyPressed 40 pressedKeys then Down
      else Rest
  in
    { player | direction = direction' }

placePlayer : Time -> Ui -> Player -> Player
placePlayer delta {playTime, windowSize} ({position, direction} as player) =
  let
    -- Data
    (_, playerHeight) = playerSize
    (_, windowHeight) = windowSize
    maxY = (toFloat windowHeight) - playerHeight
    {x, y} = position
    speed = playTimeToSpeed playTime
    vy =
      case direction of
        Up -> -1.5 * speed
        Down -> 1.5 * speed
        Rest -> 0
    dy = vy * delta

    -- Primes
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

-- Projectile updaters

setWaitAndFlavor : Wait -> Projectile -> Projectile
setWaitAndFlavor waitSeed projectile =
  let
    flavor =
      if rem waitSeed 2 == 0 then
        Good waitSeed
      else
        Bad waitSeed
    wait = waitSeed * 50
  in { projectile | wait = wait, flavor = flavor }

moveToRightEdge : WindowSize -> Projectile -> Projectile
moveToRightEdge (windowWidth, _) ({position} as projectile) =
  { projectile | position = { position | x = toFloat windowWidth } }

decrementWait : Projectile -> Projectile
decrementWait ({ wait } as projectile) =
  { projectile | wait = wait-1 }

waitOrMoveProjectile : Time -> Speed -> Projectile -> Projectile
waitOrMoveProjectile delta speed ({wait} as projectile) =
  if wait > 0 then
    decrementWait projectile
  else
    moveProjectile delta speed projectile

moveProjectile : Time -> Speed -> Projectile -> Projectile
moveProjectile delta speed ({position} as projectile) =
  let
    {x, y} = position
    vx = -1 * speed
    dx = delta * vx
    x' = x + dx
  in
    { projectile | position = { x = x', y = y } }

-- Booleans

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

insufficientScore : Player -> Bool
insufficientScore {score} =
  score <= 0

-- Helpers

playTimeToSpeed : Time -> Speed
playTimeToSpeed playTime =
  let
    secondsPerLevel = 5
    increment = 0.035
    level = playTime |> inSeconds
  in
    baseSpeed + level * increment

setProjectiles : WindowSize -> List Projectile -> List Projectile
setProjectiles (_, windowHeight) existingProjectiles =
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
          initialProjectile
          [existingLength..targetLength-1])
    else
      existingProjectiles
