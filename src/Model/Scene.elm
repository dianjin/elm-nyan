module Model.Scene exposing (..)

import Keyboard exposing (KeyCode)
import Char exposing (toCode)
import Time exposing (Time, inSeconds)

import Model.Geometry exposing (..)

-- Scene

type alias Scene =
  { player : Player
  , projectiles: List Projectile
  , scoreLog : List Int
  }

initialScene : Scene
initialScene =
  { player = defaultPlayer
  , projectiles = []
  , scoreLog = []
  }

-- Player

playerSize : (Float, Float)
playerSize = (250, 100)

type Direction = Up | Down | Rest

type alias Player =
  { score : Int
  , position : Vector
  , direction : Direction
  }

defaultScore = 100

defaultPlayer : Player
defaultPlayer =
  { score = defaultScore
  , position = { x = 0, y = 0 }
  , direction = Rest
  }

-- Projectile

maxWait = 200
minWait = 0

projectileSize : (Float, Float)
projectileSize = (100, 100)

type Flavor
  = Good
  | Bad

type alias Projectile =
  { wait : Int
  , position : Vector
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
    , flavor = Good
    }

baseSpeed : Float
baseSpeed = 0.06

playTimeToSpeed : Time -> Float
playTimeToSpeed playTime =
  let
    secondsPerLevel = 5
    increment = 0.035
    level = playTime |> inSeconds
  in
    baseSpeed + level * increment
