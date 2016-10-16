module View.Styles exposing (..)

import Model.Ui exposing (WindowSize)

type alias Styles = List (String, String)

viewOuterAttrs : WindowSize -> Styles
viewOuterAttrs (w, h) =
  [ ("background-color", "navy")
  , ("height", toString h ++ "px")
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
  [ ("width", "500px")
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

playerImgAttrs (w, h) {x, y} =
  [ ("position", "absolute")
  , ("top", toString y ++ "px")
  , ("left", "-20px")
  , ("width", toString w ++ "px")
  , ("height", toString h ++ "px")
  ]
