{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
module CreateGraph where

import Prelude
import FPPrac.Graphics
import FPPrac.Events
import Graphics

import Data.Char
import Data.List(sort,(\\))

import Debug.Trace

data Process = MakingNode | MakingEdge | DoingNothing
  deriving (Eq,Show)

data Store a = Store
  { graph              :: Graph
  , startNode          :: Node
  , endNode            :: Node
  , process            :: Process
  , userHandler        :: a -> Input -> (a,[Output])
  , userHandlerEnabled :: Bool
  , userState          :: a
  , userStateInit      :: Graph -> a
  , userGraphLens      :: a -> Graph
  , userBottomLine     :: Graph -> Picture
  }

startGraph   = Graph {name="",directed=False,weighted=False,nodes=[],edges=[]}
nullNode     = ('-',white,(0,0))
clearStore s = s {startNode=nullNode, endNode=nullNode, process=DoingNothing, userHandlerEnabled = False}
startStore h i l b = Store 
               { graph = startGraph
               , startNode = nullNode
               , endNode = nullNode
               , process = DoingNothing
               , userHandler = h
               , userHandlerEnabled = False
               , userState = i startGraph
               , userStateInit = i
               , userGraphLens = l
               , userBottomLine = b
               }

distance (x0,y0) (x1,y1) = sqrt ((x1-x0)^2 + (y1-y0)^2)

freespace graph p = nodes==[] || minimum [distance p (x,y) | (_,_,(x,y)) <- nodes] > distfactor*nodeRadius
                  where
                    Graph {directed=directed,weighted=weighted,nodes=nodes}=graph
                    distfactor | directed && weighted = 7
                               | directed || weighted = 5
                               | otherwise            = 3


node2text (lbl,col,(x,y)) = lbl : (show $ rgbaOfColor col) ++ ":" ++ show x ++ "," ++ show y ++ "\n"

edge2text (lbl1,lbl2,col,wght)
  = [lbl1,lbl2] ++ show (rgbaOfColor col) ++ ":" ++ show wght ++ "\n"

text2node :: String -> Node
text2node line = (lbl, col, (read x, read (tail y)))
               where
                 lbl   = head line
                 col   = (\(r,g,b,a) -> makeColor r g b a) $ read $ takeWhile (/=':') (tail line)
                 (x,y) = span (/=',') (tail (dropWhile (/=':') line))
      
text2edge :: String -> Edge           
text2edge line = (lbl1,lbl2, col, read (tail awght))
               where
                 (lbl1,lbl2)  = (head line, head (tail line))
                 (acol,awght) = span (/=':') (drop 2 line)
                 col          = (\(r,g,b,a) -> makeColor r g b a) $ read acol
                 
createGraph :: 
  forall a 
  . Store a 
  -> Input 
  -> (Store a,[Output])
  
createGraph store@(Store {userHandlerEnabled=True,..}) (KeyIn 'q') 
  = (s', [DrawPicture $ drawBottomLine graph'])
  where
    graph'     = userGraphLens userState
    userState' = userStateInit graph'
    s'         = clearStore $ store {graph=graph',userState=userState'}

createGraph store@(Store {userHandlerEnabled=True, ..}) i
  = (store {userState = userState'}, o) 
  where
    (userState',o) = userHandler userState i

createGraph store (MouseDown (x,y))
  | nodeClicked /= Nothing = (newStoreE, [DrawOnBuffer False])
  | otherwise              = (newStoreN, [])
  where
    Store {graph=graph} = store
    Graph {nodes=nodes} = graph
    
    nodeClicked  = onNode nodes (x,y)
    Just newNode = nodeClicked
    
    newStoreN = store {process=MakingNode}
    newStoreE = store {startNode=newNode, process=MakingEdge}

createGraph store (MouseMotion (x,y))
  | process == MakingEdge = (store,[DrawOnBuffer False, DrawPicture $ drawEdgeTemp black (startPoint,(x,y))])
  | otherwise             = (store,[])
  where
    Store {startNode=(_,_,startPoint),process=process} = store

createGraph store (MouseUp (x,y))
  | process == MakingNode
  , freespace graph (x,y)
  , freeLabels /= ""
  = (newStoreN, [DrawPicture $ drawNode newNode])
  
  | process == MakingEdge
  , nodeClicked == Nothing
  = (storeCl, [DrawOnBuffer True, DrawPicture $ Pictures [drawGraph graph, drawBottomLine graph]])
  
  | process == MakingEdge
  , weighted
  , lblE /= lblS
  , not alreadyConnected
  = (newStoreE1, [DrawOnBuffer True, GraphPrompt ("weight", "maximum 99")])
  
  | process == MakingEdge
  , not weighted
  , lblE /= lblS
  , not alreadyConnected
  = (newStoreE2, [DrawOnBuffer True, DrawPicture $ Pictures [drawGraph newGraphE2, drawBottomLine newGraphE2]])
  
  | otherwise
  = (storeCl, [DrawOnBuffer True, DrawPicture $ Pictures [drawGraph graph, drawBottomLine graph]])
    
  where
    Store {startNode=(lblS,colS,crdsS),..} = store
    Graph {..} = graph
    
    -- New node
    freeLabels = alphabet \\ [lbl | (lbl,_,_) <- nodes]
    newLabel   = head (sort freeLabels)
    newNode    = (newLabel, white, (x,y))
    
    newGraphN = graph {nodes=newNode:nodes}
    newStoreN = clearStore $ store {graph=newGraphN}
    
    -- New edge
    nodeClicked = onNode nodes (x,y)
    Just (lblE,colE,crdsE) = nodeClicked
    
    alreadyConnected | directed  = elem lblE [ lbl2 | (lbl1,lbl2,col,wght) <- edges
                                                    , lbl1 == lblS ]
                     | otherwise = not $ null 
                                     [lbl1 | (lbl1,lbl2,coll,wght) <- edges
                                           , (lbl1 == lblS && lbl2 == lblE)
                                           || (lbl1 == lblE && lbl2 == lblS)
                                           ]
    
    newStoreE1 = store {endNode = (lblE,colE,crdsE)}
    
    newEdge    = (lblS, lblE, black, 0)
    newGraphE2 = graph {edges = newEdge:edges}
    newStoreE2 = clearStore $ store {graph = newGraphE2}
    
    -- Clear store
    storeCl   = clearStore store

createGraph store (MouseDoubleClick (x,y))
  | nodeClicked /= Nothing
  = (newStore, [DrawPicture $ Pictures [drawGraph newGraph, drawBottomLine newGraph]])
  | otherwise
  = (store, [])
  where
    Store {graph=graph} = store
    Graph {nodes=nodes,edges=edges} = graph
    
    nodeClicked = onNode nodes (x,y)
    Just (lblClicked,_,_) = nodeClicked
    
    newNodes = [ (lbl,col,(x,y)) | (lbl,col,(x,y)) <- nodes , lbl /= lblClicked ]
    newEdges = [ (lbl1,lbl2,c,w) | (lbl1,lbl2,c,w) <- edges 
                                 , lbl1 /= lblClicked
                                 , lbl2 /= lblClicked 
                                 ]
    
    newGraph = graph {nodes=newNodes, edges=newEdges}
    newStore = store {graph = newGraph}

createGraph store (Prompt ("weight", weight))
  | weight /= ""
  , all isDigit weight
  = (newStoreE, [DrawPicture $ Pictures [drawGraph newGraph, drawBottomLine newGraph]])
  | otherwise
  = (newStore, [DrawPicture $ Pictures [drawGraph graph, drawBottomLine graph]])
  where
    Store {graph=graph, startNode=(lblS,_,_), endNode=(lblE,_,_)} = store
    Graph {edges=edges} = graph
    
    newEdge = (lblS, lblE, black, read weight)
    
    newGraph = graph {edges = newEdge:edges}
    newStoreE = store {graph=newGraph, startNode = nullNode, endNode = nullNode, process=DoingNothing}
    
    newStore = store {startNode = nullNode, endNode = nullNode, process = DoingNothing}

createGraph store (KeyIn 'n')
  = (store,[newPanel,newPanelVisible])
  
createGraph store (Panel 1 [(60,dir), (70,wghtd)])
  = (newStore,[newPanelInvisible,DrawPicture $ Pictures [drawGraph newGraph,drawBottomLine newGraph]])
  where
    newGraph = Graph
               { name = ""
               , directed = dir   == "Y"
               , weighted = wghtd == "Y"
               , nodes = []
               , edges = []
               }
    newStore = store {graph=newGraph}

createGraph store (KeyIn 'r')
  = (store,[GraphPrompt ("Read graph", "filename")])

createGraph store (Prompt ("Read graph", fileName))
  | fileName /= "" = (store, [ReadFile fileName (TXTFile "")])
  | otherwise      = (store, [])
  
createGraph store (File fileName (TXTFile input))
  | input /= "" 
  = (newStore, [DrawPicture $ Pictures [drawGraph newGraph, drawBottomLine newGraph]])
  | otherwise   
  = (store,[])
  where
    (ns,bs) = (span (/="--") . drop 2 . lines) input
    directed = (head . lines) input == "directed:yes"
    weighted = (head . tail . lines) input == "weighted:yes"
    
    newGraph = Graph 
               { name     = fileName
               , directed = directed
               , weighted = weighted
               , nodes    = map text2node ns
               , edges    = map text2edge (tail bs)
               }
    
    newStore = clearStore $ store {graph=newGraph}

createGraph store (KeyIn 's')
  | nm /= ""  = (store, [SaveFile nm (TXTFile savetext)])
  | otherwise = (store, [GraphPrompt ("save as", "filename")])
  where
    Graph {name=nm,directed=directed,weighted=weighted,nodes=nodes,edges=edges} = graph store
    
    dirweight | directed && weighted = "directed:yes\nweighted:yes\n"
              | directed             = "directed:yes\nweighted:no\n"
              | weighted             = "directed:no\nweighted:yes\n"
              | otherwise            = "directed:no\nweighted:no\n"

    savetext = dirweight
               ++ concat (map node2text nodes) ++ "--\n"
               ++ concat (map edge2text edges)

createGraph store (KeyIn 'a')
  = (store,[GraphPrompt ("save as", "filename")])

createGraph store (Prompt ("save as", nm))
  = (newStore, [SaveFile nm (TXTFile savetext), DrawPicture $ drawBottomLine newGraph])
  where
    Graph {name=nm0,directed=directed,weighted=weighted,nodes=nodes,edges=edges} = graph store
    
    dirweight | directed && weighted = "directed:yes\nweighted:yes\n"
              | directed             = "directed:yes\nweighted:no\n"
              | weighted             = "directed:no\nweighted:yes\n"
              | otherwise            = "directed:no\nweighted:no\n"

    savetext = dirweight
               ++ concat (map node2text nodes) ++ "--\n"
               ++ concat (map edge2text edges)
    
    newGraph = (graph store) {name=nm}
    newStore = store {graph=newGraph}

createGraph store@(Store {..}) (KeyIn '6') 
  = (store {userHandlerEnabled = True, userState = userStateInit graph}, [DrawPicture $ userBottomLine graph])

createGraph store _ = (store,[])

newPanel
  = PanelCreate 
    ("New Graph", 160, 45
    ,[]
    ,[ (60, "directed", CheckButton, -65, 20, 10, 10)
     , (70, "weighted", CheckButton, 5  , 20, 10, 10)
     , (1 , "OK"      , Button     , 0  , 0 , 60, 20)
     ]
    )
    
newPanelVisible   = PanelUpdate True  []
newPanelInvisible = PanelUpdate False []
    
onNode :: [Node] -> Point -> Maybe Node
onNode [] p = Nothing
onNode (n@(_,_,q):ns) p | distance p q <= nodeRadius = Just n
                        | otherwise                  = onNode ns p

doGraph h i l b = installEventHandler "createGraph" createGraph (startStore h i l b) startPic 10
  where
    startPic = Pictures
      [ drawGraph startGraph
      , drawBottomLine startGraph
      ]
