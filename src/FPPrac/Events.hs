{-# LANGUAGE RankNTypes #-}
-- | The event mode lets you manage your own input. 
-- Pressing ESC will still closes the window, but you don't get automatic 
-- pan and zoom controls like with 'graphicsout'. Should only be called once
-- during the execution of a program!
module FPPrac.Events
  ( module Graphics.Gloss.Interface.Game
  , Input (..)
  , installEventHandler
  )
where

import Data.Maybe (fromMaybe)
import FPPrac.Graphics
import Graphics.Gloss.Interface.Game

data EventState a = EventState { picture   :: Picture
                               , userState :: a
                               }
-- | Possible input events
data Input -- | No input
					 = NoInput
					 -- | Keyboard key x is pressed down; ' ' for space, \\t for tab, \\n for enter
           | KeyIn 			 Char
					 -- | Left mouse button is pressed at location (x,y)
					 | MouseDown   (Float,Float)
					 -- | Left mouse button is released at location (x,y)
					 | MouseUp     (Float,Float)
					 -- | Mouse pointer is moved to location (x,y)
					 | MouseMotion (Float,Float)
	deriving (Eq,Show)

eventToInput (EventKey (Char x) 								Down _ _) = KeyIn x
eventToInput (EventKey (SpecialKey  KeySpace)   Down _ p) = KeyIn ' '
eventToInput (EventKey (SpecialKey  KeyTab)     Down _ p) = KeyIn '\t'
eventToInput (EventKey (SpecialKey  KeyEnter)   Down _ p) = KeyIn '\n'
eventToInput (EventKey (MouseButton LeftButton) Down _ p) = MouseDown p
eventToInput (EventKey (MouseButton LeftButton) Up   _ p) = MouseUp p
eventToInput (EventMotion p)															= MouseMotion p
eventToInput e                                            = NoInput

-- | The event mode lets you manage your own input. 
-- Pressing ESC will still abort the program, but you don't get automatic 
-- pan and zoom controls like with graphicsout. Should only be called once
-- during the execution of a program!
installEventHandler ::
  forall userState
  . String -- ^ Name of the window
  -> (userState -> Input -> (userState, Maybe Picture)) -- ^ Event handler that takes current state, input, and returns new state and maybe an updated picture
  -> userState -- ^ Initial state of the program
  -> IO ()
installEventHandler name handler initState = gameInWindow
  name
  (800,600)
  (20,20)
  white
  50
  (EventState Blank initState)
  picture
  (\e s -> let (s',g) = handler (userState s) (eventToInput e)
           in  EventState (fromMaybe (picture s) g) s')
  (\t s -> let (s',g) = handler (userState s) NoInput
           in  EventState (fromMaybe (picture s) g) s')
  