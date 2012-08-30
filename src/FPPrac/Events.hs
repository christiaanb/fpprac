{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The event mode lets you manage your own input.
-- Pressing ESC will still closes the window, but you don't get automatic
-- pan and zoom controls like with 'graphicsout'. Should only be called once
-- during the execution of a program!
module FPPrac.Events
  ( FileType (..)
  , Input (..)
  , Output (..)
  , PanelItemType (..)
  , PromptInfo
  , PanelContent
  , PanelItem
  , installEventHandler
  )
where

import Data.List (mapAccumL)
import FPPrac.Graphics
import FPPrac.GUI.Panel
import FPPrac.GUI.Prompt
import Graphics.Gloss.Interface.Pure.Game hiding (play)
import Graphics.Gloss.Interface.IO.Game (playIO)
import Data.Time (getCurrentTime,utctDayTime)
import Control.Exception as X

type PromptInfo = (String,String)

-- | Possible filetypes
data FileType
  -- | Text file
  = TXTFile String
  -- | Bitmap file
  | BMPFile Picture
  deriving (Eq,Show)

-- | Possible input events
data Input -- | No input
           --
           -- Generated every refresh of the eventhandler
           = NoInput
           -- | Keyboard key x is pressed down; ' ' for space, \\t for tab, \\n for enter
           | KeyIn       Char
           -- | Left mouse button is pressed at location (x,y)
           | MouseDown   (Float,Float)
           -- | Left mouse button is released at location (x,y)
           | MouseUp     (Float,Float)
           -- | Mouse pointer is moved to location (x,y)
           | MouseMotion (Float,Float)
           -- | Mouse is double-clicked at location (x,y)
           | MouseDoubleClick (Float,Float)
           -- | Prompt (windowname,textbox content)
           --
           -- Content returned from textbox in promptwindow with 'windowname'
           | Prompt PromptInfo
           -- | Panel buttonId [(controlId, value)]
           --
           -- Event indicating that in the panel, the button with buttonId is
           -- pressed and that at the time the controls had the given value
           --
           -- Note: the list is ordered by controlId
           --
           -- - For checkboxes a value \"Y\" indicates that they are checked and
           -- a value of \"N\" indicates they are unchecked
           --
           -- - Buttons have no controlstate
           | Panel Int [(Int,String)]
           -- | File name content
           --
           -- The found file with given name, and found content
           | File FilePath FileType
           -- | Indicates if saving of file at filepath succeeded
           | Save FilePath Bool
           -- | Response to GetTime
           --
           -- The time from midnight, 0 <= t < 86401s (because of leap-seconds)
           -- It has a precision of 10^-12 s. Leap second is only added if day
           -- has leap seconds
           | Time Float
           -- | Invalid / Unknown input
           | Invalid
  deriving (Eq,Show)

data Output -- | Command to change the drawing mode
            --
            -- Pictures returned from the eventhandler will normally be drawn
            -- on the screen and in a buffer, so that the window can be quickly
            -- redrawn.
            --
            -- A DrawOnBuffer command can change this default behavior, If the
            -- parameter is False, pictures are only drawn on the screen. If the
            -- parameter is True, drawing will be down on both the buffer and the
            -- screen. This can be useful in response to MouseMotion Events.
            --
            -- Example of rubber banding in line drawing program:
            --
            -- @
            -- handler (p1:ps) (MouseDown p2)
            --   = (p1:ps, [DrawOnBuffer False, DrawPicture (Color black $ Line [p1,p2])])
            -- handler (p1:ps) (MouseMotion p2)
            --   = (p1:ps, [DrawOnBuffer False, DrawPicture (Color black $ Line [p1,p2])])
            -- handler (p1:ps) (MouseUp p2)
            --   = (p2:p1:ps, [DrawOnBuffer True, DrawPicture (Color black $ Line [p1,p2])])
            -- @
            = DrawOnBuffer Bool
            -- | Draw the picture
            | DrawPicture  Picture
            -- | GraphPrompt (windowName,info)
            --
            -- Create a graphical prompt window which asks the user to enter
            -- a string in a textbox. The user can be informed about valid
            -- entries through the 'info' field.
            --
            -- Note: the entered string is recorded as the following input event:
            -- 'Prompt (windowName,enteredText)'
            | GraphPrompt  PromptInfo
            -- | Command to create a panel with the given panel content, must be
            -- actived with the 'PanelUpdate' command
            | PanelCreate  PanelContent
            -- | PanelUpdate visible [(identifier, value)]
            --
            -- Command to change visibility and the content of a panel.
            --
            -- Note: not all controls need to be listed, the order can be
            -- arbitrary
            --
            -- - For checkboxes, a value \"Y\" checks them, a value \"N\" unchecks them
            --
            -- - Buttons can not be altered
            | PanelUpdate  Bool [(Int,String)]
            -- | Clear the screen and buffer
            | ScreenClear
            -- | ReadFile fileName default
            --
            -- Read the file of the given filetype at the filename, if it fails
            -- The default content is returned
            --
            -- Note: the read file command generates following input event:
            -- 'File fileName content'
            | ReadFile FilePath FileType
            -- | SaveFile fileName content
            --
            -- Save the file of the given filetype at the filename location
            --
            -- Note: the save file command generates following input event:
            -- Save fileName success (True/False)
            | SaveFile FilePath FileType
            -- | Request the current time of day in seconds
            --
            -- Note: the gettime command generates the following input event:
            -- 'Time timeOfDay'
            | GetTime
  deriving (Eq,Show)

data GUIMode = PanelMode | PromptMode PromptInfo String | FreeMode | PerformIO
  deriving (Eq,Show)

data EventState a = EventState { screen       :: Picture
                               , buffer       :: Picture
                               , drawOnBuffer :: Bool
                               , storedInputs :: [Input]
                               , storedOutputs :: [Output]
                               , doubleClickT :: Int
                               , guiMode      :: GUIMode
                               , panel        :: Maybe (PanelContent,[(Int,String)])
                               , userState    :: a
                               }

eventToInput ::
  Event
  -> Input
eventToInput (EventKey (Char x)                   Down _ _) = KeyIn x
eventToInput (EventKey (SpecialKey  KeySpace)     Down _ _) = KeyIn ' '
eventToInput (EventKey (SpecialKey  KeyTab)       Down _ _) = KeyIn '\t'
eventToInput (EventKey (SpecialKey  KeyEnter)     Down _ _) = KeyIn '\n'
eventToInput (EventKey (SpecialKey  KeyBackspace) Down _ _) = KeyIn '\b'
eventToInput (EventKey (MouseButton LeftButton)   Down _ p) = MouseDown p
eventToInput (EventKey (MouseButton LeftButton)   Up   _ p) = MouseUp p
eventToInput (EventMotion p)                                = MouseMotion p
eventToInput _                                              = Invalid

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
installEventHandler name handler initState p dcTime = playIO
  (InWindow name (800,600) (20,20))
  white
  50
  (EventState p p True [] [] 0 FreeMode Nothing initState)
  (return . screen)
  (\e s -> handleInputIO handler dcTime s (eventToInput e))
  (\_ s -> handleInputIO handler dcTime s NoInput)

handleInputIO ::
  forall userState
  . (userState -> Input -> (userState, [Output]))
  -> Int
  -> EventState userState
  -> Input
  -> IO (EventState userState)
handleInputIO handler dcTime s@(EventState {guiMode = PerformIO,..}) i = do
  inps <- fmap (filter (/= Invalid)) $ mapM handleIO storedOutputs
  let s' = s {guiMode = FreeMode, storedOutputs = [], storedInputs = storedInputs ++ inps}
  return $ handleInput handler dcTime s' i

handleInputIO handler dcTime s i = return $ handleInput handler dcTime s i

handleInput ::
  forall userState
  . (userState -> Input -> (userState, [Output]))
  -> Int
  -> EventState userState
  -> Input
  -> EventState userState
handleInput handler dcTime s@(EventState {guiMode = FreeMode, ..}) i
  = s' {userState = userState', doubleClickT = doubleClickT', storedInputs = []}
  where
    (doubleClickT',dc)    = registerDoubleClick dcTime doubleClickT i
    remainingInputs       = storedInputs ++ (if null dc then [i] else dc)
    (userState',outps)    = mapAccumL handler userState remainingInputs
    s'                    = foldl handleOutput s $ concat outps

handleInput handler _ s@(EventState {guiMode = PanelMode, panel = Just (panelContents,itemState), ..}) (MouseDown (x,y))
  | isClicked /= Nothing = s''
  | otherwise            = s
  where
    isClicked          = onItem panelContents (x,y)
    (Just itemClicked) = isClicked
    itemState'         = toggleItem itemState itemClicked
    (userState',outps) = handler userState (Panel (fst itemClicked) $ filter ((/= "") . snd) itemState')
    s'                 = s {screen = Pictures [buffer,drawPanel panelContents itemState'], panel = Just (panelContents,itemState'), userState = userState'}
    s''                = foldl handleOutput s' outps


handleInput _ _ s@(EventState {guiMode = PromptMode pInfo pContent, ..}) (KeyIn '\b')
  | pContent /= [] = s'
  | otherwise      = s
  where
    pContent' = init pContent
    screen'   = Pictures [buffer,drawPrompt pInfo pContent']
    s'        = s {guiMode = PromptMode pInfo pContent', screen = screen'}

handleInput handler _ s@(EventState {guiMode = PromptMode (pName,_) pContent, ..}) (KeyIn '\n')
  = s''
  where
    (userState',outps) = handler userState (Prompt (pName,pContent))
    s'                 = s {guiMode = FreeMode, screen = buffer, userState = userState'}
    s''                = foldl handleOutput s' outps

handleInput _ _ s@(EventState {guiMode = PromptMode pInfo pContent, ..}) (KeyIn x)
  = s'
  where
    pContent' = pContent ++ [x]
    screen'   = Pictures [buffer,drawPrompt pInfo pContent']
    s'        = s {guiMode = PromptMode pInfo pContent', screen = screen'}

handleInput _ _ s _ = s

registerDoubleClick ::
  Int
  -> Int
  -> Input
  -> (Int,[Input])
registerDoubleClick d 0 (MouseDown _)     = (d  ,[])
registerDoubleClick _ _ (MouseDown (x,y)) = (0  ,[MouseDoubleClick (x,y)])
registerDoubleClick _ 0 NoInput           = (0  ,[])
registerDoubleClick _ n NoInput           = (n-1,[])
registerDoubleClick _ n _                 = (n  ,[])

handleOutput ::
  EventState a
  -> Output
  -> EventState a
handleOutput s (DrawOnBuffer b) = s {drawOnBuffer = b}
handleOutput s ScreenClear      = s {buffer = Blank, screen = Blank}
handleOutput s@(EventState {..}) (DrawPicture p) =
  s { buffer = if drawOnBuffer
        then Pictures [buffer, p]
        else buffer
    , screen = Pictures [buffer, p]
    }

handleOutput s@(EventState {guiMode = FreeMode, ..}) i@(ReadFile _ _) =
  s {guiMode = PerformIO, storedOutputs = storedOutputs ++ [i]}

handleOutput s@(EventState {guiMode = FreeMode, ..}) i@(SaveFile _ _) =
  s {guiMode = PerformIO, storedOutputs = storedOutputs ++ [i]}

handleOutput s@(EventState {guiMode = FreeMode, ..}) i@(GetTime) =
  s {guiMode = PerformIO, storedOutputs = storedOutputs ++ [i]}

handleOutput s@(EventState {..}) (PanelCreate panelContent)
  = s {panel = Just (panelContent,defItemState)}
  where
    defItemState = createDefState panelContent

handleOutput s@(EventState {panel = Just (panelContents,itemState), ..}) (PanelUpdate True _)
  = s {guiMode = PanelMode, screen = Pictures [buffer,drawPanel panelContents itemState]}

handleOutput s@(EventState {panel = Nothing}) (PanelUpdate True _)
  = s

handleOutput s@(EventState {panel = Just (panelContents,_), ..}) (PanelUpdate False _)
  = s {guiMode = FreeMode, screen = buffer, panel = Just (panelContents,defItemState)}
  where
    defItemState = createDefState panelContents

handleOutput s@(EventState {panel = Nothing, ..}) (PanelUpdate False _)
  = s {guiMode = FreeMode, screen = buffer}

handleOutput s@(EventState {..}) (GraphPrompt promptInfo)
  = s {guiMode = PromptMode promptInfo "", screen = Pictures [buffer,drawPrompt promptInfo ""]}

handleOutput s _ = s

handleIO :: Output -> IO Input
handleIO (ReadFile filePath (TXTFile defContents)) =
  (do f <- readFile filePath
      return $ File filePath $ TXTFile f
  ) `X.catch`
  (\(_ :: IOException) -> return (File filePath $ TXTFile defContents))

handleIO (ReadFile filePath (BMPFile defContents)) =
  (do f <- loadBMP filePath
      return $ File filePath $ BMPFile f
  ) `X.catch`
  (\(_ :: IOException) -> return (File filePath $ BMPFile defContents))

handleIO (SaveFile filePath (TXTFile content)) =
  ( do writeFile filePath content
       return $ Save filePath True
  ) `X.catch`
  (\(_ :: IOException) -> return $ Save filePath False)

handleIO (SaveFile filePath (BMPFile _)) = return $ Save filePath False

handleIO GetTime = do
  t <- fmap utctDayTime $ getCurrentTime
  return $ Time (fromRational $ toRational t)

handleIO _  = return Invalid
