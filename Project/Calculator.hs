import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.Maybe

import Pages

import Expr

canWidth  = 300
canHeight = 300

readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw input canvas =
    do
        text <- getValue input
        let expr = fromJust (readExpr (fromJust text))
        render canvas (stroke (path (points expr 0.04 (canWidth, canHeight))))

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 40 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_    -> readAndDraw input can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input can
      -- "Enter" key has code 13

points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width,height) = calculatePoints 0

    where 
        calculatePoints x 
            | x <= fromIntegral width = if y <= fromIntegral height && y >= 0 then (x, y):calculatePoints (x+1) else calculatePoints (x+1)
            | otherwise = []
            where y = realToPix (eval exp (pixToReal x))
        pixToReal x = x * scale - fromIntegral width * scale / 2
        realToPix y = (- y / scale) + fromIntegral height / 2

