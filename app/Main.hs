{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Prelude hiding (Right, Left)
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Core.Names

data Edge = Top | Right | Bottom | Left

data Arrow = Arrow { arrowId :: String, arrowFrom :: (String, Edge), arrowTo :: (String, Edge) }

component :: String -> Diagram B
component n = rect 1 1 # named n # frame 1 # lwN 0.002 # lc black


connectComponents (Arrow arrowId (a, aEdge) (b, bEdge)) = 
    withName a $ \ aDiagram ->
    withName b $ \ bDiagram ->
      atop (arrowBetween' (with & arrowHead .~ noHead
                                & shaftStyle %~ lw veryThin) 
                          (boundaryFrom aDiagram (edge aEdge)) 
                          (boundaryFrom bDiagram (edge bEdge)) 
                          # svgId arrowId)
    where
        edge Top    = unitY
        edge Right  = unitX
        edge Bottom = unit_Y
        edge Left   = unit_X

--    connectPerim' (with & arrowHead .~ noHead

--                        & shaftStyle %~ lw veryThin) a b (edge aEdge) (edge bEdge)
             

distDiagram :: Diagram B
distDiagram = 
    components # connectComponents (Arrow "a" ("1", Right) ("2", Left))
               # connectComponents (Arrow "b" ("1", Bottom) ("4", Top))
    where
        components :: Diagram B
        components = 
            component "1" ||| component "2" ||| component "3"
            ===
            component "4" ||| component "5" ||| component "6"

dev = renderSVG "test.svg" (mkWidth 800) distDiagram

main = renderSVG "test.svg" (mkWidth 250) distDiagram