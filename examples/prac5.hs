module Prac5 where

import Prelude
import FPPrac.Graphics
import FPPrac.Events
import Graphics

import System.FilePath (splitPath, dropExtension)

import CreateGraph
import Debug.Trace

data MyStore = MyStore
  { myGraph :: Graph
  }
  
initPrac5 graph = MyStore {myGraph = graph}

main = doGraph doPrac5 initPrac5 myGraph drawMypracBottomLine

doPrac5 :: MyStore -> Input -> (MyStore,[Output])
-- =======================================
-- = Voeg hier extra doPrac5 clauses toe =
-- =======================================
-- doPrac5 myStore (KeyIn 'r') = (myStore', o)
--   where
--     myStore' = ...
--     o        = ...
--

doPrac5 myStore i = (myStore,[])

drawMypracBottomLine :: Graph -> Picture
drawMypracBottomLine graph = 
  Pictures
    [ Translate 0 (-300 + bottomLineHeight / 2) $ Color white $ rectangleSolid 800 bottomLineHeight
    , Color black $ Line [(-400,height1),(400,height1)]
    , Color black $ Line [(-240,height1),(-240,-300)]
    , Translate (-392) height2 $ Color black $ Scale 0.11 0.11 $ Text "myprac:"
    , Translate (-332) height2 $ Color red   $ Scale 0.11 0.11 $ Text $ (case (name graph) of "" -> "" ; xs -> dropExtension $ last $ splitPath xs)
    -- Voeg hier, indien nodig, extra informatie toe
    -- , Translate (-235) height2 $ Color black $ Scale 0.11 0.11 $ Text "..."
    ]
    where
      height1 = -300 + bottomLineHeight
      height2 = -300 + bottomTextHeight
