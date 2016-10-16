module View.Styles exposing (..)

import Model.Ui exposing (WindowSize)
import Model.Scene exposing (Flavor(..))
import Settings exposing (..)

type alias Styles = List (String, String)

viewOuterAttrs : WindowSize -> Styles
viewOuterAttrs (w, h) =
  [ ("height", toString h ++ "px")
  , ("width", toString w ++ "px")
  ]

bannerOuterAttrs : Styles
bannerOuterAttrs =
  [ ("position", "absolute")
  , ("top", "50%")
  , ("left", "50%")
  , ("width", "400px")
  , ("height", "60px")
  , ("margin", "-30px 0 0 -200px")
  , ("font-size", "40px")
  , ("text-align", "center")
  , ("z-index", "500")
  ]

bannerBlockAttrs =
  [ ("display", "block")
  ]

bannerSubAttrs =
  [ ("font-size", "20px")
  , ("padding", "5px 0 0 0")
  ]

footerOuterAttrs =
  [ ("position", "fixed")
  , ("bottom", "0")
  , ("width", "100%")
  , ("font-size", "16px")
  , ("padding-bottom", "30px")
  ]

footerWrapperAttrs =
  [ ("width", "550px")
  , ("margin", "0 auto")
  ]

footerInnerAttrs isLink =
  let
    cursor =
      ("cursor", if isLink then "pointer" else "default")
    float =
      ("float", if isLink then "right" else "left")
  in
    cursor::float::
      [("padding", "0 10px"), ("display", "inline-block")]

playerImgAttrs {x, y} =
  let
    (playerWidth, playerHeight) = playerSize
  in
    [ ("position", "absolute")
    , ("top", toString y ++ "px")
    , ("left", "-20px")
    , ("width", toString playerWidth ++ "px")
    , ("height", toString playerHeight ++ "px")
    ]

projectileAttrs {position, flavor} =
  let
    {x, y} = position
    (pWidth, pHeight) = projectileSize
    backgroundSize =
      (toString pWidth) ++ "px " ++ (toString pHeight) ++ "px"
    assetUrl name =
      projectileAssetBasePath ++ name ++ projectileAssetExtension
    assetName =
      case flavor of
        Good assetId -> toString assetId
        Bad assetId -> toString assetId
  in
    [ ("width", toString pWidth ++ "px")
    , ("height", toString pHeight ++ "px")
    , ("background-image", "url(\"" ++ assetUrl assetName ++ "\")")
    , ("background-size", backgroundSize)
    , ("position", "absolute")
    , ("top", toString y ++ "px")
    , ("left", toString x ++ "px")
    ]
