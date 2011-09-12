module RBrun where
-- Grafische weergave van rood-zwart bomen. Werkt alleen voor *binaire* bomen.
--
-- Jan Kuper, 5 mei 2008
-- ============================================================================

import FPPrac.Events
import FPPrac.Graphics
import RBgraphics
import Prelude

-- ============= types ========================================================
-- RBnode c v ts:   c=colour, v=value, ts=subtrees

data StateTp = StateTp { mode :: Bool
                       , rbts :: [RbTreeG]
                       }

initstate = StateTp { mode = False
                    , rbts = [ exampleTree, exampleTree, exampleTree ]
                    }

main = eventHandler "RBrun" doE initstate
   where
      StateTp { mode = m, rbts = ts} = initstate

---- ============= event handler ================================================

doE :: StateTp -> Input -> (StateTp, Maybe Picture)
doE s (KeyIn 'm') = (s {mode  = not (mode s)}, Just (drawTrees (not (mode s)) 200 (rbts s)))
doE s e           = (s, Just (drawTrees (not (mode s)) 200 (rbts s)))

-- ======voorbeeldboom=========================================================
-- Let op: deze boom is slechts ter illustratie van de grafische weergave,
--         hij voldoet *niet* aan de rood-zwart eis

exampleTree = RBnode black "9"
                     [ RBnode red "99"
                              [ RBnode red "99"
                                       [ RBnode black "9"  []
                                       , RBnode black  "99" []
                                       ]
                              , RBnode red "ii"
                                       [ RBnode black "99"  []
                                       , RBnode black "9" []
                                       ]
                              ]
                     , RBnode red "k"
                              [ RBnode black "ll" [],
                                RBnode black  "m"
                                       [ RBnode red "nn"
                                                [ RBnode red "q" [ RBnode black "nn" []
                                                                , RBnode black "q"  []
                                                                ]
                                                , RBnode red "r" []
                                                ]
                                       , RBnode red "pp"
                                                [ RBnode black "r" [ RBnode red "nn" []
                                                               , RBnode  (dark $ dark white)  "" []
                                                               ]
                                                , RBnode black "r" [ RBnode red "nn" []
                                                               , RBnode  (dark white)  "" []
                                                               ]
                                                ]
                                       ]
                              ]
                     ]
