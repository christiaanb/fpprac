{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- | The event mode lets you manage your own input. 
-- Pressing ESC will still closes the window, but you don't get automatic 
-- pan and zoom controls like with 'graphicsout'. Should only be called once
-- during the execution of a program!
module FPPrac.Events
  ( module Graphics.Gloss.Interface.Game
  , Input (..)
  , Output (..)
  , PanelItemType (..)
  , installEventHandler
  )
where

import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
import FPPrac.Graphics
import FPPrac.GUI.Panel
import FPPrac.GUI.Prompt
import Graphics.Gloss.Interface.Game

type PromptInfo = (String,String)

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
					 | MouseDoubleClick (Float,Float)
					 | Prompt PromptInfo
					 | Panel Int [(Int,String)]
	deriving (Eq,Show)

data Output = DrawOnBuffer Bool
            | DrawPicture  Picture
            | GraphPrompt  PromptInfo
            | PanelCreate  PanelContent
            | PanelUpdate  Bool [(Int,String)]
            | ScreenClear
  deriving (Eq,Show)

data GUIMode = PanelMode | PromptMode PromptInfo String | FreeMode
  deriving (Eq,Show)

data EventState a = EventState { screen       :: Picture
                               , buffer       :: Picture
                               , drawOnBuffer :: Bool
                               , storedInputs :: [Input]
                               , doubleClickT :: Int
                               , guiMode      :: GUIMode
                               , panel        :: Maybe (PanelContent,[(Int,String)])
                               , userState    :: a
                               }

eventToInput (EventKey (Char x) 								Down _ _) = KeyIn x
eventToInput (EventKey (SpecialKey  KeySpace)   Down _ p) = KeyIn ' '
eventToInput (EventKey (SpecialKey  KeyTab)     Down _ p) = KeyIn '\t'
eventToInput (EventKey (SpecialKey  KeyEnter)   Down _ p) = KeyIn '\n'
eventToInput (EventKey (SpecialKey  KeyBackspace) Down _ p) = KeyIn '\b'
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
  -> (userState -> Input -> (userState, [Output])) -- ^ Event handler that takes current state, input, and returns new state and maybe an updated picture
  -> userState -- ^ Initial state of the program
  -> Picture -- ^ Initial Picture
  -> Int -- ^ doubleclick speed
  -> IO ()
installEventHandler name handler initState p dcTime = gameInWindow
  name
  (800,600)
  (20,20)
  white
  50
  (EventState p p True [] 0 FreeMode Nothing initState)
  screen
  (\e s -> handleInput handler dcTime s (eventToInput e))
  (\t s -> handleInput handler dcTime s NoInput)

handleInput ::
  forall userState
  . (userState -> Input -> (userState, [Output]))
  -> Int
  -> EventState userState
  -> Input
  -> EventState userState
handleInput handler dcTime s@(EventState {guiMode = FreeMode, ..}) i 
  = s' {userState = userState', doubleClickT = doubleClickT'}
  where
    (doubleClickT',dc)    = registerDoubleClick dcTime doubleClickT i
    remainingInputs       = storedInputs ++ (i:dc)
    (userState',outps)    = mapAccumL handler userState remainingInputs
    s'                    = foldl handleOutput s $ concat outps 

handleInput handler dcTime s@(EventState {guiMode = PanelMode, panel = Just (panelContents,itemState), ..}) (MouseDown (x,y)) 
  | isClicked /= Nothing = s''
  | otherwise            = s
  where
    isClicked          = onItem panelContents (x,y)
    (Just itemClicked) = isClicked
    itemState'         = toggleItem itemState itemClicked
    (userState',outps) = handler userState (Panel (fst itemClicked) $ filter ((/= "") . snd) itemState')
    s'                 = s {screen = Pictures [buffer,drawPanel panelContents itemState'], panel = Just (panelContents,itemState'), userState = userState'}
    s''                = foldl handleOutput s' outps
    

handleInput handler dcTime s@(EventState {guiMode = PromptMode pInfo pContent, ..}) (KeyIn '\b')
  | pContent /= [] = s'
  | otherwise      = s
  where
    pContent' = init pContent
    screen'   = Pictures [buffer,drawPrompt pInfo pContent']
    s'        = s {guiMode = PromptMode pInfo pContent', screen = screen'}

handleInput handler dcTime s@(EventState {guiMode = PromptMode (pName,pInfo) pContent, ..}) (KeyIn '\n')
  = s''
  where
    (userState',outps) = handler userState (Prompt (pName,pContent))
    s'                 = s {guiMode = FreeMode, screen = buffer, userState = userState'}
    s''                = foldl handleOutput s' outps

handleInput handler dcTime s@(EventState {guiMode = PromptMode pInfo pContent, ..}) (KeyIn x)
  = s'
  where
    pContent' = pContent ++ [x]
    screen'   = Pictures [buffer,drawPrompt pInfo pContent']
    s'        = s {guiMode = PromptMode pInfo pContent', screen = screen'}

handleInput handler dcTime s i = s
    
registerDoubleClick d 0 (MouseDown _)     = (d,[])
registerDoubleClick _ n (MouseDown (x,y)) = (0, [MouseDoubleClick (x,y)])
registerDoubleClick _ 0 NoInput           = (0,[])
registerDoubleClick _ n NoInput           = (n-1,[])
registerDoubleClick _ n _                 = (n,[])

handleOutput s (DrawOnBuffer b) = s {drawOnBuffer = b}
handleOutput s ScreenClear      = s {buffer = Blank, screen = Blank}
handleOutput s@(EventState {..}) (DrawPicture p) = 
  s { buffer = if drawOnBuffer 
        then Pictures [buffer, p] 
        else buffer
    , screen = Pictures [buffer, p]
    }
handleOutput s@(EventState {..}) (PanelCreate panelContent) 
  = s {panel = Just (panelContent,defItemState)}
  where
    defItemState = createDefState panelContent

handleOutput s@(EventState {panel = Just (panelContents,itemState), ..}) (PanelUpdate True _) 
  = s {guiMode = PanelMode, screen = Pictures [buffer,drawPanel panelContents itemState]}

handleOutput s@(EventState {panel = Nothing}) (PanelUpdate True _) 
  = s

handleOutput s@(EventState {panel = Just (panelContents,itemState), ..}) (PanelUpdate False _) 
  = s {guiMode = FreeMode, screen = buffer, panel = Just (panelContents,defItemState)}
  where
    defItemState = createDefState panelContents

handleOutput s@(EventState {panel = Nothing, ..}) (PanelUpdate False _) 
  = s {guiMode = FreeMode, screen = buffer}

handleOutput s@(EventState {..}) (GraphPrompt promptInfo) 
  = s {guiMode = PromptMode promptInfo "", screen = Pictures [buffer,drawPrompt promptInfo ""]}
