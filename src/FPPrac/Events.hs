module FPPrac.Events
  ( module Graphics.Gloss.Interface.Game
  , Input (..)
  , eventHandler
  )
where

import Data.Maybe (fromMaybe)
import FPPrac.Graphics
import Graphics.Gloss.Interface.Game

data EventState a = EventState { picture   :: Picture
                               , userState :: a
                               }

data Input = NoInput
           | KeyIn 				Char
					 | MouseDown    (Float,Float)
					 | MouseUp      (Float,Float)
					 | MouseDragged (Float,Float)
	deriving (Eq,Show)

eventToInput (EventKey (Char x) 								Down _ _) = KeyIn x
eventToInput (EventKey (SpecialKey  KeySpace)   Down _ p) = KeyIn ' '
eventToInput (EventKey (SpecialKey  KeyTab)     Down _ p) = KeyIn '\t'
eventToInput (EventKey (SpecialKey  KeyEnter)   Down _ p) = KeyIn '\n'
eventToInput (EventKey (MouseButton LeftButton) Down _ p) = MouseDown p
eventToInput (EventKey (MouseButton LeftButton) Up   _ p) = MouseUp p
eventToInput (EventMotion p)															= MouseDragged p
eventToInput e                                            = NoInput

eventHandler ::
  String
  -> (a -> Input -> (a, Maybe Picture))
  -> a
  -> IO ()
eventHandler name handler initState = gameInWindow
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
  