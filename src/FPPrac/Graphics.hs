-- | Open a new window and display the given picture. Should only be called
-- once during the execution of a program!
module FPPrac.Graphics 
  ( module Graphics.Gloss
  , graphicsout
  )
where

import Graphics.Gloss hiding (displayInWindow,animateInWindow)
import qualified Graphics.Gloss as Gloss

-- | Open a new window and display the given picture. Should only be called
-- once during the execution of a program!
-- 
--   Use the following commands once the window is open:
--
-- 	* Close Window - esc-key.
--
--	* Move Viewport - left-click drag, arrow keys.
--
--	* Rotate Viewport - right-click drag, control-left-click drag, or home\/end-keys.
--
--	* Zoom Viewport - mouse wheel, or page up\/down-keys.
graphicsout :: Picture -> IO ()
graphicsout  = Gloss.displayInWindow "graphicsout" (640,480) (20,20) white
