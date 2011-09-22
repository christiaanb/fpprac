module Graphics where

import Prelude
import FPPrac.Graphics
import System.FilePath (splitPath, dropExtension)

data Thickness = Thin | Thick
  deriving (Eq,Show)

alphabet = ['a'..'z']

type Node  = (Char,Color,Point)
type Edge  = (Char,Char,Color,Int)

data Graph = Graph
  { name     :: String
  , directed :: Bool
  , weighted :: Bool
  , nodes    :: [Node]
  , edges    :: [Edge]
  } deriving (Eq,Show)

data EdgeDrawingData = EdgeDrawingData
  { edgeStartpunt   :: Point -- | (tx,ty): startpunt van edge
  , edgeEndpunt     :: Point -- | (px,py): eindpunt van edge
  , innerPijlpunt   :: Point -- | (qx,qy): binnenhoek van pijlpunt
  , achterPijlpunt  :: Point -- | (sx,sy): achterpunten van pijlpunt
  , breedtePijlpunt :: Point -- | (wx,wy): breedte pijlputn
  , edgeDikte       :: Point -- | (dx,dy): dikte van de edge
  , weightAfstand   :: Point -- | (ax,ay): afstand gewicht tot pijlpunt
  , weightMidden    :: Point -- | (mx,my): midden van edge  
  }

edgeWidthThin  = 1
edgeWidthThick = 1.5
nodeRadius     = 15
weightDistance = 6

arrowHeight    = 20
arrowDepth     = 9
arrowWidth     = 9

lblBoxW = 20
lblBoxH = 19


lblHshift      = 8
lblVshift      = -6

bottomLineHeight = 25
bottomTextHeight = 10

allEdgeDrawingData thickness graph (lbl1,lbl2,col,wght)
    = EdgeDrawingData
      { edgeStartpunt   = (tx,ty)                -- startpunt van edge
      , edgeEndpunt     = (px,py)                -- eindpunt van edge
      , innerPijlpunt   = (qx,qy)                -- binnenhoek van pijlpunt
      , achterPijlpunt  = (sx,sy)                -- achterpunten van pijlpunt
      , breedtePijlpunt = (wx,wy)                -- breedte pijlpunt
      , edgeDikte       = (dx,dy)                -- dikte van de edge
      , weightAfstand   = (ax,ay)                -- afstand gewicht tot pijlpunt
      , weightMidden    = (mx,my)                -- midden van edge
      }
    where
      Graph {directed=directed,nodes=nodes} = graph
      (x1,y1) = head [ (x,y) | (lbl,_,(x,y)) <- nodes, lbl==lbl1 ]
      (x2,y2) = head [ (x,y) | (lbl,_,(x,y)) <- nodes, lbl==lbl2 ]

      rico  = (y2-y1) / (x2-x1)
      alpha | x2 > x1              = atan rico
            | x2 == x1 && y2 > y1  = pi/2
            | x2 < x1              = pi + atan rico
            | x2 == x1 && y2 <= y1 = 3*pi/2
      sina  = sin alpha
      cosa  = cos alpha

      (xr1,yr1) = (nodeRadius * cosa , nodeRadius * sina)
      (tx ,ty ) = (x1+xr1,y1+yr1)                                               -- start of edge
      (px ,py ) = (x2-xr1,y2-yr1)                                               -- outer arrow point

      (xr2,yr2) = (arrowDepth * cosa , arrowDepth * sina)
      (qx,qy)   = (px-xr2,py-yr2)                                               -- inner arrow point

      (xh ,yh ) = (arrowHeight * cosa , arrowHeight * sina)
      (sx ,sy ) = (px-xh,py-yh)                                                 -- back arrowpoints

      (wx ,wy ) = (arrowWidth * sina , arrowWidth * cosa)                       -- width of arrowpoint

      (dx ,dy ) | thickness == Thick = (edgeWidthThick * sina , edgeWidthThick * cosa)
                | otherwise          = (edgeWidthThin  * sina , edgeWidthThin  * cosa) -- edge thickness

      (xwd,ywd) = (weightDistance * cosa , weightDistance * sina)
      (ax ,ay ) = (px-xwd,py-ywd)                                               -- distance of weight from arrowpoint

      (mx ,my ) = ((x2+x1)/2,(y2+y1)/2)                                       -- mid of (undirected) edge

drawNode :: Node -> Picture
drawNode (lbl,col,(x,y))
    = Pictures
      [ Translate x y $ Color col $ circleSolid r
      , Translate x y $ Color black $ Circle r
      , Translate (x-lblHshift) (y+lblVshift) $ Color black $ Scale 0.15 0.15 $ Text [lbl]
      ]
    where
      r = nodeRadius
      
drawEdgeTemp :: Color -> (Point,Point) -> Picture
drawEdgeTemp col (p1,p2)
  = Color col $ Line [p1,p2]
  
drawEdge :: Graph -> Edge -> Picture
drawEdge graph edge
    | directed =
      Pictures
      [ Color col   $ Polygon [ (tx+dx,ty-dy), (tx-dx,ty+dy), (qx-dx,qy+dy), (qx+dx,qy-dy), (tx+dx,ty-dy) ]
      , Color white $ Polygon [ (px+dx,py-dy), (px-dx,py+dy), (qx-dx,qy+dy), (qx+dx,qy-dy), (px+dx,py-dy) ]
      , Color col   $ Polygon [ (px,py), (sx-wx,sy+wy), (qx,qy), (sx+wx,sy-wy), (px,py) ]
      ]
    | otherwise =
      Color col $ Polygon [ (tx+dx,ty-dy), (tx-dx,ty+dy), (px-dx,py+dy), (px+dx,py-dy), (tx+dx,ty-dy) ]
    where
      Graph {directed=directed} = graph
      (_,_,col,_)               = edge

      EdgeDrawingData
        { edgeStartpunt   = (tx,ty)                -- startpunt van edge
        , edgeEndpunt     = (px,py)                -- eindpunt van edge
        , innerPijlpunt   = (qx,qy)                -- binnenhoek van pijlpunt
        , achterPijlpunt  = (sx,sy)                -- achterpunten van pijlpunt
        , breedtePijlpunt = (wx,wy)                -- breedte pijlpunt
        , edgeDikte       = (dx,dy)                -- dikte van de edge
        } = allEdgeDrawingData Thin graph edge
        
drawThickEdge :: Graph -> Edge -> Picture
drawThickEdge graph edge
    | directed =
      Pictures
      [ Color col   $ Polygon [ (tx+dx,ty-dy), (tx-dx,ty+dy), (qx-dx,qy+dy), (qx+dx,qy-dy), (tx+dx,ty-dy) ]
      , Color white $ Polygon [ (px+dx,py-dy), (px-dx,py+dy), (qx-dx,qy+dy), (qx+dx,qy-dy), (px+dx,py-dy) ]
      , Color col   $ Polygon [ (px,py), (sx-wx,sy+wy), (qx,qy), (sx+wx,sy-wy), (px,py) ]
      ]
    | otherwise =
      Color col $ Polygon [ (tx+dx,ty-dy), (tx-dx,ty+dy), (px-dx,py+dy), (px+dx,py-dy), (tx+dx,ty-dy) ]
    where
      Graph {directed=directed} = graph
      (_,_,col,_)               = edge

      EdgeDrawingData
        { edgeStartpunt   = (tx,ty)                -- startpunt van edge
        , edgeEndpunt     = (px,py)                -- eindpunt van edge
        , innerPijlpunt   = (qx,qy)                -- binnenhoek van pijlpunt
        , achterPijlpunt  = (sx,sy)                -- achterpunten van pijlpunt
        , breedtePijlpunt = (wx,wy)                -- breedte pijlpunt
        , edgeDikte       = (dx,dy)                -- dikte van de edge
        } = allEdgeDrawingData Thick graph edge
        
drawWeight :: Graph -> Edge -> Picture
drawWeight graph edge
  | not weighted = Blank
  | directed     = Pictures
    [ Translate ax ay $ Color white $ rectangleSolid lblBoxW lblBoxH
    , Translate (ax-lblHshift) (ay+lblVshift) $ Color black $ Scale 0.11 0.11 $ Text (show wght)
    ]
  | otherwise    = Pictures
    [ Translate mx my $ Color white $ rectangleSolid lblBoxW lblBoxH
    , Translate (mx-lblHshift) (my+lblVshift) $ Color black $ Scale 0.11 0.11 $ Text (show wght)
    ]
  where
    Graph {directed=directed,weighted=weighted}=graph
    (_,_,_,wght) = edge

    EdgeDrawingData
      { weightAfstand   = (ax,ay)                -- afstand gewicht tot pijlpunt
      , weightMidden    = (mx,my)                -- midden van edge
      }
      = allEdgeDrawingData Thin graph edge
      
drawGraph :: Graph -> Picture
drawGraph graph 
    = Pictures $
      Color white (rectangleSolid 800 564)
      :  (map drawNode nodes)
      ++ (map (drawEdge graph) edges)
      ++ (map (drawWeight graph) edges)
    where
      Graph {name=name,nodes=nodes,edges=edges}=graph

drawBottomLine :: Graph -> Picture
drawBottomLine graph
    = Pictures
      [ Translate 0 (-300 + bottomLineHeight / 2) $ Color white $ rectangleSolid 800 bottomLineHeight
      , Color black $ Line [(-400,height1),(400,height1)]
      , Color black $ Line [(-240,height1),(-240,-300)]
      , Color black $ Line [(100,height1),(100,-300)]
      , Translate (-392) height2 $ Color black $ Scale 0.11 0.11 $ Text "create:"
      , Translate (-332) height2 $ Color red   $ Scale 0.11 0.11 $ Text $ (case (name graph) of "" -> "" ; xs -> dropExtension $ last $ splitPath xs)
      , Translate (-235) height2 $ Color black $ Scale 0.11 0.11 $ Text "click: node; drag: edge; double: remove node"
      , Translate 120    height2 $ Color black $ Scale 0.11 0.11 $ Text "[n]ew; [r]ead; [s]ave; save [a]s; prac[6]"
      ]
    where
      height1 = -300 + bottomLineHeight
      height2 = -300 + bottomTextHeight
