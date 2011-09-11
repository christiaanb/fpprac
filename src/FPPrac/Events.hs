module FPPrac.Events
  ( Event (..)
  , eventHandler
  )
where

import Data.Maybe (fromMaybe)
import FPPrac.Graphics
import qualified Graphics.Gloss.Interface.Game as Gloss

data EventState a = EventState { picture   :: Picture
                               , userState :: a
                               }

data Event = NoInput
           | KeyIn Char

inputToEvent (Gloss.EventKey (Gloss.Char x) Gloss.Down _ _) = KeyIn x
inputToEvent _                                              = NoInput

eventHandler ::
  String
  -> (a -> Event -> (a, Maybe Picture))
  -> a
  -> IO ()
eventHandler name handler initState = Gloss.gameInWindow
  name
  (640,480)
  (20,20)
  white
  50
  (EventState Gloss.Blank initState)
  picture
  (\e s -> let (s',g) = handler (userState s) (inputToEvent e)
           in  EventState (fromMaybe (picture s) g) s')
  (\t s -> let (s',g) = handler (userState s) NoInput
           in  EventState (fromMaybe (picture s) g) s')
  