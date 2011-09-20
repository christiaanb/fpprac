{-# LANGUAGE PatternGuards #-}
module FPPrac.GUI.Panel 
  ( PanelItemType(..)
  , PanelContent
  , drawPanel
  , onItem
  , toggleItem
  , createDefState
  )
where

import Graphics.Gloss
import Graphics.Gloss.Data.Point

titleshift     = 4
titlebarheight = 16
titlebarShift  = 8

lblVshift      = -6

data PanelItemType      = CheckButton | Button
  deriving (Eq,Show)

type PanelItem     = (Int,String,PanelItemType,Float,Float,Float,Float)

type PanelContent  = (String 
                     ,Float, Float
                     ,[(String,[(String,Int)])]
                     ,[PanelItem]
                     )

createDefState ::
  PanelContent
  -> [(Int,String)]
createDefState (_, _, _, _, items) = map createDefStateItem items

createDefStateItem ::
  PanelItem
  -> (Int,String)
createDefStateItem (i,_,CheckButton,_,_,_,_)   = (i,"N")
createDefStateItem (i,_,_            ,_,_,_,_) = (i,"" )
                     
drawPanel ::
  PanelContent
  -> [(Int,String)]
  -> Picture
drawPanel (title, w, h, menu, items) itemStates
  = Pictures $
  [ Translate 0 titlebarShift $ Color white $ rectangleSolid w (h + titlebarheight)
  , Translate 0 titlebarShift $ Color black $ rectangleWire  w (h + titlebarheight)
  , drawTitleBar w h title
  ] ++ zipWith (drawItem w h) items itemStates

drawTitleBar w h title
  = Pictures
  [ Color black $ Line [(negate w/2, h/2), (w/2,h/2)]
  , Translate ((negate w/2)+5) (h/2 + titleshift) $ Color black $ Scale 0.1 0.1 $ Text title
  ]

drawItem bboxW bboxH (itemId, name, CheckButton, x, y, w, h) (_,itemState)
  | x < (bboxW / 2)
  , x > (negate bboxW / 2 )
  , (y - titlebarheight) < (bboxH / 2)
  , (y - titlebarheight) > (negate bboxH / 2)
  , (x + w) < (bboxW / 2)
  , ((y - titlebarheight) + h) < (bboxH / 2)
  = Pictures $
  [ Translate x     (y - titlebarShift) $ Color black $ rectangleWire w h
  , Translate (x+w) (y - titlebarShift + lblVshift) $ Color black $ Scale 0.1 0.1 $ Text name
  ] ++ if (itemState == "Y") then
    [Translate 0 (-titlebarShift) $ Color black $ Line [(x-w/2,y-h/2),(x+w/2,y+h/2)]
    ,Translate 0 (-titlebarShift) $ Color black $ Line [(x-w/2,y+h/2),(x+w/2,y-h/2)]
    ]
    else []
  
  | otherwise
  = Blank

drawItem bboxW bboxH (itemId, name, Button, x, y, w, h) _
  | x < (bboxW / 2)
  , x > (negate bboxW / 2 )
  , (y - titlebarheight) < (bboxH / 2)
  , (y - titlebarheight) > (negate bboxH / 2)
  , (x + w) < (bboxW / 2)
  , ((y - titlebarheight) + h) < (bboxH / 2)
  = Pictures
  [ Translate x      (y - titlebarShift) $ Color black $ rectangleWire w h
  , Translate xlabel (y - titlebarShift + lblVshift) $ Color black $ Scale 0.1 0.1 $ Text name
  ]
  
  | otherwise
  = Blank
  where
    xlabel = x - 3.5 * (fromIntegral $ length name)

-- drawItem _ _ _ _ = Blank

onItem ::
  PanelContent
  -> (Float,Float)
  -> Maybe (Int,PanelItemType)
onItem (title, w, h, menu, items) (x,y) = onItem' items (x,y)

onItem' ::
  [PanelItem]
  -> (Float,Float)
  -> Maybe (Int,PanelItemType)
onItem' [] _ = Nothing
onItem' ((itemId, name, itemType, x, y, w, h):is) p 
  | pointInBox p (x+w/2,y-h/2-titlebarShift) (x-w/2,y+h/2-titlebarShift)
  = Just (itemId,itemType)
  
  | otherwise = onItem' is p

toggleItem ::
  [(Int,String)]
  -> (Int,PanelItemType)
  -> [(Int,String)]
toggleItem [] _ = []
toggleItem ((i,val):is) (t,ttype) | i == t    = (i, toggleVal ttype val):is
                                  | otherwise = (i,val):(toggleItem is (t,ttype))
                                   
toggleVal CheckButton "N" = "Y"
toggleVal CheckButton "Y" = "N"
toggleVal Button n        = n