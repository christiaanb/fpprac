module FPPrac.Graphics 
  ( module Graphics.Gloss
  , graphicsout
  )
where

import Graphics.Gloss hiding (displayInWindow,animateInWindow)
import qualified Graphics.Gloss as Gloss

graphicsout :: Picture -> IO ()
graphicsout  = Gloss.displayInWindow "graphicsout" (640,480) (20,20) white
