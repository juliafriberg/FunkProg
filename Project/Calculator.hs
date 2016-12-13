import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.Maybe
import Data.Char

import Pages

import Expr

canWidth  = 300
canHeight = 300

originalScale = 0.04

readAndDraw :: Elem -> Canvas -> Double -> IO ()
readAndDraw input canvas scale =
    do
        text <- getValue input
        case text of 
            Nothing -> return ()
            _ -> do
                let expr = readExpr (fromJust text)
                renderCanvas canvas scale expr

renderCanvas :: Canvas -> Double -> Maybe Expr -> IO ()
renderCanvas canvas scale Nothing = render canvas (stroke (path []))
renderCanvas canvas scale expr = render canvas (stroke (path (points (fromJust expr) scale (canWidth, canHeight))))
        

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 40 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
    dropdown <- mkDropDown
    zoom1   <- mkOption "50%"
    zoom2   <- mkOption "100%"
    zoom3   <- mkOption "150%"
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    dropDown dropdown [zoom2, zoom1, zoom3] 
    row formula [fx,input]
    column documentBody [canvas,dropdown,formula,draw]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_    -> readAndDraw input can originalScale
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input can originalScale
    onEvent dropdown Change $ \_ -> zoom input dropdown can
      -- "Enter" key has code 13

zoom :: Elem -> Elem -> Canvas -> IO ()
zoom input dropdown can = 
    do
        value <- getValue dropdown
        let value' = fromJust value
        let scale = case value' of
                        "50%" -> 2
                        "100%" -> 1
                        "150%" -> 0.5
        readAndDraw input can (scale * originalScale)

points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width,height) = calculatePoints 0

    where 
        calculatePoints x 
            | x <= fromIntegral width = if y <= fromIntegral height && y >= 0 then (x, y):calculatePoints (x+1) else calculatePoints (x+1)
            | otherwise = []
            where y = realToPix (eval exp (pixToReal x))
        pixToReal x = x * scale - fromIntegral width * scale / 2
        realToPix y = (- y / scale) + fromIntegral height / 2

