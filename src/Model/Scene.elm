module Model.Scene exposing (..)

import Keyboard exposing (KeyCode)
import Char exposing (toCode)
import Time exposing (Time, inSeconds)

import Model.Geometry exposing (..)

-- Scene

type alias Scene =
  { player : Player
  , projectiles: List Projectile
  }

initialScene : Scene
initialScene =
  { player = defaultPlayer
  , projectiles = []
  }

-- Player

playerSize : (Float, Float)
playerSize = (250, 100)

type alias Player =
  { score : Int
  , position : Vector
  , velocity : Vector
  }

defaultScore = 100

defaultPlayer : Player
defaultPlayer =
  { score = defaultScore
  , position = { x = 0, y = 0 }
  , velocity = { x = 0, y = 0 }
  }

-- Projectile

maxWait = 300
minWait = 0

projectileSize : (Float, Float)
projectileSize = (100, 100)

type Flavor
  = Good
  | Bad

type alias Projectile =
  { wait : Int
  , position : Vector
  , velocity : Vector
  , flavor : Flavor
  }

defaultProjectile : Int -> Projectile
defaultProjectile number =
  let
    (_, projectileHeight) = projectileSize
    y = projectileHeight * (toFloat number)
  in
    { wait = 0
    , position = { x = -200, y = y }
    , velocity = { x = 0, y = 0 }
    , flavor = Good
    }

baseProjectileVelocity : Vector
baseProjectileVelocity =
  { x = -0.6 , y = 0 }

playTimeToVelocity : Time -> Vector
playTimeToVelocity playTime =
  let
    secondsPerLevel = 5
    increment = -0.035
    level = playTime |> inSeconds
    {x, y} = baseProjectileVelocity
  in
    { x = x + level * increment, y = y}
