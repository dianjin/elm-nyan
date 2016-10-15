module Model.Scene exposing (..)

import Keyboard exposing (KeyCode)
import Char exposing (toCode)
import Time exposing (Time)

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

defaultPlayer : Player
defaultPlayer =
  { score = 0
  , position = { x = 0, y = 0 }
  , velocity = { x = 0, y = 0 }
  }

-- Projectile

maxWait = 300
minWait = 0

projectileSize : (Float, Float)
projectileSize = (100, 100)

type alias Projectile =
  { wait : Int
  , position : Vector
  , velocity : Vector
  }

projectileVelocity : Vector
projectileVelocity =
  { x = -0.6 , y = 0 }

defaultProjectile : Int -> Projectile
defaultProjectile number =
  let
    (_, projectileHeight) = projectileSize
    y = projectileHeight * (toFloat number)
  in
    { wait = 0
    , position = { x = -200, y = y }
    , velocity = { x = 0, y = 0 }
    }
