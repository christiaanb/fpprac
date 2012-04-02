{-# LANGUAGE PatternGuards #-}
module FPPrac.GUI.Prompt
  ( drawPrompt
  )
where

import Graphics.Gloss

titleshift :: Float
titleshift     = 4

titlebarheight :: Float
titlebarheight = 16

titlebarShift :: Float
titlebarShift  = 8

lblVshift :: Float
lblVshift      = -6

drawPrompt ::
  (String,String)
  -> String
  -> Picture
drawPrompt (title, info) promptContents
  = Pictures $
  [ Translate 0 titlebarShift $ Color white $ rectangleSolid 200 (50 + titlebarheight)
  , Translate 0 titlebarShift $ Color black $ rectangleWire  200 (50 + titlebarheight)
  , drawTitleBar 200 50 title
  , drawPromptText info promptContents
  ]

drawTitleBar ::
  Float
  -> Float
  -> String
  -> Picture
drawTitleBar w h title
  = Pictures
  [ Color black $ Line [(negate w/2, h/2), (w/2,h/2)]
  , Translate ((negate w/2)+5) (h/2 + titleshift) $ Color black $ Scale 0.1 0.1 $ Text title
  ]

drawPromptText ::
  String
  -> String
  -> Picture
drawPromptText info promptContents
  = Pictures
  [ Translate (-90) ((20) - titlebarShift + lblVshift) $ Color black $ Scale 0.1 0.1 $ Text info
  , Translate (-30) ((0) - titlebarShift) $ Color black $ rectangleWire 120 20
  , Translate (-85) ((0) - titlebarShift + lblVshift) $ Color black $ Scale 0.1 0.1 $ Text promptContents
  ]
