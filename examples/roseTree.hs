module RoseTree where
-- Grafische weergave van tamelijk algemene bomen (zie typedef).
-- Aanroepen:
--     voor enkele boom t:  showTree t
--     voor lijst bomen ts: showTreeList ts
--
-- Jan Kuper, 10 januari 2003

import FPPrac.Graphics

---------------boomtype-----------------------------------------
data RoseTree = RoseNode String [RoseTree]
  deriving (Show, Eq)

---------------schalingsfactoren--------------------------------

verticaleftshift = 50.0
leftshift        = 6.0
horizontalfactor = 19.0
letterheight     = 16.0

---------------breedte ve boom berekenen------------------------
-- in principe: breedte = het aantal characters (fixed font)
-- opletten: string aan interne knoop kan meer ruimte nodig hebben
--    dan alle subbomen op dat punt samen.
-- opletten: ook bij de lege string een positieve treewidth (1) opleveren

treewidth :: RoseTree -> Float
treewidth (RoseNode a ts) = maximum [1.0, fromIntegral $ length a, sum (map treewidth ts)]


---------------coordinaten van de wortels ve (sub)boom----------
-- hor_pos berekent de horizontale positie van de wortel van de
--    i-de boom uit een lijst ts van bomen - BINNEN die lijst van bomen.
--    Die positie is de totale breedte van alle bomen links van
--    boom i, plus de halve breedte van boom i zelf (in gehele getallen).
-- hor_poss berekent de horizontale posities van alle bomen in een
--    lijst ts - uitgaande van het midden van de lijst ts.

hor_pos :: [RoseTree] -> Int -> Float
hor_pos ts i = (sum . map treewidth . take i) ts + treewidth (ts!!i) / 2.0

hor_poss :: Float -> [RoseTree] -> [Float]
hor_poss midden ts                                      -- (x,y): midden
     = ( map (+links) . map (hor_pos ts) ) [0..(length ts)-1]
     where
       links = midden - sum (map treewidth ts) / 2.0

---------------deelgraphics-------------------------------------
-- textgraphical, linegraphical produceren graphics-waarden voor
--    een string, resp edge ve boom.
--    Zetten bovendien de horizontale positie om in een
--    x-coordinaat in het grafische vlak (bij Amanda 1.29
--    loopt dat vlak van x=-1 tot x=+1, en van y=-1 tot y=+1).

textgraphical :: (Float,Float) -> String -> Picture
textgraphical (x,y) a = Translate xlabel (y - 12.0) $ Scale 0.1 0.1 $ Color black $ Text a
                      where
                        xlabel = horizontalfactor * (x - (fromIntegral $ length a) / leftshift)

linegraphical :: (Float,Float) -> (Float,Float) -> Picture
linegraphical (x,y) (x1,y1) = Color red $ Line [(horizontalfactor*x,y), (horizontalfactor*x1,y1)]

---------------graphics ve boom, resp lijst v bomen-------------
-- drawTree, drawTreeList zetten een boom (bomen) om in grafische waarden,
--    uitgaande van de coordinaten (x,y) van de wortel van de boom.
--    Daarbij maakt drawTree recursief gebruik van drawTreeList, met een
--    kleinere y-coordinaat (rekening houdend met een eventuele
--    lege string aan een knoop).
--    Op zijn beurt maakt drawTreeList gebruik van drawTree voor elke boom
--    uit een lijst van bomen apart.

drawTree :: ((Float,Float) , RoseTree) -> Picture
drawTree ((x,y), (RoseNode a ts))
     = Pictures ((textgraphical (x,y) a
                 :  map (linegraphical (x,yc)) (zip (hor_poss x ts) [y-verticaleftshift|i<-[1..]]))
            ++ [drawTreeList ((x,y-verticaleftshift), ts)])
     where
       yc | a == ""   = y
          | otherwise = y-letterheight

drawTreeList :: ((Float,Float) , [RoseTree]) -> Picture
drawTreeList ((x,y), ts)
     = (Pictures . map drawTree) (zip xy_coords ts)
     where
       xy_coords = zip (hor_poss x ts) [y|i<-[1.0 ..]]

-- =============output===========================================
-- Let op: keuzes voor GraphResize, GraphFont, en bovenstaande
-- schalingsfactoren zijn op elkaar afgestemd. Verandering daarvan
-- kan de netheid van de output beinvloeden.

startpuntMainRoot = (0, 0.9)         -- midden bovenaan het scherm

showTreeList :: [RoseTree] -> IO ()
showTreeList ts = graphicsout $ drawTreeList (startpuntMainRoot, ts)

showTree :: RoseTree -> IO ()
showTree t = showTreeList [t]


-- ======voorbeeldboom===========================================

exampleTree = RoseNode "p"
                        [ RoseNode "aaa"
                                    [ RoseNode "bbb"
                                                [ RoseNode "ccc" [],
                                                  RoseNode "ddd" []
                                                ],
                                      RoseNode ""
                                                [RoseNode "fff" [],
                                                 RoseNode "ggg" [],
                                                 RoseNode "hhh" []
                                                ],
                                      RoseNode "iii"
                                                [RoseNode "" []
                                                ]
                                    ],
                          RoseNode "kkk"
                                    [RoseNode "lll" [],
                                     RoseNode "mmm"
                                               [RoseNode "nnn"
                                                          [RoseNode "q" [],
                                                           RoseNode "r" []
                                                          ],
                                                RoseNode "ooo" [],
                                                RoseNode "ppp" []
                                               ]
                                    ]
                        ]
