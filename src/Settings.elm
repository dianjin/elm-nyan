module Settings exposing (..)

------------------------------------------------------
-- Physics
------------------------------------------------------
-- Height of player and projectile
bodySize = 100
-- Height and width of collidable area
collisionFrame = 60
-- Base speed
baseSpeed = 0.08
-- Speed of player =
-- Speed of projectile * playerSpeedFactor
playerSpeedFactor = 1.5
-- Wait time before projectile is dispatched =
-- Wait seed (random integer from 1 to numFlavors) * waitSeedFactor
waitSeedFactor = 50
-- Rate at which speed of projectiles increases
projectileAcceleration = 0.00005

------------------------------------------------------
-- Scoring
------------------------------------------------------
-- Player score at t = 0
defaultScore = 100
-- Added to score upon collision with Good
-- subtracted from score upon collision with Bad
projectileCost = 50
-- Subtracted from score upon missing Good
projectileOpportunityCost = 25

------------------------------------------------------
-- Flavors
------------------------------------------------------
-- Number of flavors available
-- See Model.Scene.setWaitAndFlavor
numFlavors = 6

------------------------------------------------------
-- Player
------------------------------------------------------
-- Width and height of player
playerSize = (250, bodySize)
-- Path to player asset
playerAssetUrl = "assets/player.gif"

------------------------------------------------------
-- Projectiles
------------------------------------------------------
-- Width and height of projectile
projectileSize = (100, bodySize)
-- Base path to projectile asset
projectileAssetBasePath = "assets/projectiles/"
-- Projectile asset extension
projectileAssetExtension = ".png"
