module RBgraphics where
-- Grafische weergave van rood-zwart bomen. Werkt alleen voor *binaire* bomen.
--
-- Jan Kuper, 5 mei 2008
-- ============================================================================
import FPPrac.Graphics

data RbTreeG = RBnode Color String [RbTreeG]
  deriving Eq

---------------schalingsfactoren----------------------------------------------

letterheight = 2
nodewidth    = 30
hfactor      = 1

---------------aspecten ve boom berekenen-------------------------------------


verticalstep (RBnode _ "" _) = 25
verticalstep (RBnode _ _ _ ) = 30

nodespace    (RBnode _ "" _) = nodewidth / 4
nodespace    (RBnode _ _ _ ) = nodewidth+1


ycrd m y t | colour t == red && m = y
           | otherwise             = y - verticalstep t

leftTree  (RBnode _ _ [t0,_]) = t0
rightTree (RBnode _ _ [_,t1]) = t1

subtrees   (RBnode _ _ ts)    = ts

colour     (RBnode c _ _ )    = c

treeheight _ (RBnode _ _ [])  = 0
treeheight m (RBnode _ _ ts)  | not m     = 1 + maximum (map (treeheight m) ts)
                              | otherwise = maximum [ if (colour t == red) then
                                                         (treeheight m t)   else
                                                         (1 + treeheight m t)
                                                    | t <- ts
                                                    ]

treewidth :: RbTreeG -> Float
treewidth (RBnode c a ts) = sum (map treewidth ts) + nodespace (RBnode c a ts)

xPosleft  x t | subtrees t /= [] = x - treewidth (rightTree t) - nodespace t
              | otherwise        = x - nodespace t
xPosright x t | subtrees t /= [] = x + treewidth (leftTree  t) + nodespace t
              | otherwise        = x + nodespace t



---------------graphics-------------------------------------------------------

drawnode :: Color -> (Float,Float) -> String -> Picture
drawnode c (x,y) a | a /= ""   = Pictures
																 [ Translate xcentre y $ Color c $ circleSolid rbig
                                 , Translate xlabel (y-h) $ Scale 0.1 0.1 $ Color (if (c == black) then white else black) $ Text a
                                 ]
                   | otherwise = Translate xcentre y $ Color c $ circleSolid rsmall
  where
    xcentre = hfactor * x
    xlabel  = hfactor * (x - 3 * (fromIntegral $ length a))
    rbig    = hfactor*nodewidth/2
    rsmall  = hfactor*nodewidth/5
    h       = letterheight/1.5



drawrootedge (x,y) = Color black $ Line [(hfactor*x,y), (hfactor*x,y+hfactor*nodewidth)]


drawedge :: (Float,Float) -> (Float,Float) -> Picture
drawedge (x,y) (x1,y1) = Color red $ Line [(hfactor*x,y), (hfactor*x1,y1)]


drawbridge :: (Float,Float) -> (Float,Float) -> Picture
drawbridge (x,y) (x1,y1) = Color red $ Line (map (\(x,y)->(hfactor*x,y)) [(x,y), (x+d, yh), (x1-d,yh), (x1,y1)])
                         where
                           d = (x1-x)/3
                           yh = y+hfactor*nodewidth/1.5


drawTree :: Bool -> ((Float,Float) , RbTreeG) -> Picture
drawTree m ((x,y), (RBnode c a [ ]    )) = drawnode c (x,y) a
drawTree m ((x,y), (RBnode c a [tl,tr]))

     = Pictures $ 
				[ drawGenEdgeL (x,y) (xleft , yleft )
        , drawGenEdgeR (x,y) (xright, yright)

        , drawTree m ((xleft ,yleft ) , tl)
        , drawTree m ((xright,yright) , tr)

        , drawnode c (x,y) a
				]
     where
       xleft  = xPosleft  x tl
       xright = xPosright x tr

       yleft  = ycrd m y tl
       yright = ycrd m y tr

       drawGenEdgeL | m &&
                      colour tl == red &&
                      subtrees tl /= [] &&
                      colour (rightTree tl) == red = drawbridge
                    | otherwise                     = drawedge

       drawGenEdgeR | m
                      && colour tr == red
                      && subtrees tr /= []
                      && colour (leftTree tr) == red = drawbridge
                    | otherwise                       = drawedge



drawTrees :: Bool -> Float -> [RbTreeG] -> Picture
drawTrees m y0 [] = Blank
drawTrees m y0 (t:ts) = Pictures
  												[ drawrootedge (x0,y0)
                          , drawTree m ((x0,y0), t)
                          , drawTrees m (y0 - (treeheight m t + 1) * verticalstep t) ts
													]
                      where
                        x0 = xcoordMainRoot t



drawTreesTxts :: Bool -> Float -> [String] -> [RbTreeG] -> Picture
drawTreesTxts m y0  []         []    = Blank
drawTreesTxts m y0 (str:strs) (t:ts) = Pictures
   																			[ drawrootedge (x0,y0)
                                        , drawTree m ((x0,y0), t)
                                        , Translate (-150) y0 $ Scale 0.1 0.1 $ Color black $ Text str
                                        , drawTreesTxts m (y0 - (treeheight m t + 1) * verticalstep t) strs ts
																				]
                      where
                        x0 = xcoordMainRoot t




xcoordMainRoot (RBnode c a [tl,tr]) = (treewidth tl - treewidth tr)/2
xcoordMainRoot t = 0
