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

origScale = 0.04

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    f'x     <- mkHTML ""                     -- The text for f'(x), not shown
                                             -- before diff is pressed
    input   <- mkInput 40 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
    diff    <- mkButton "Differentiate"      -- The differentiate button
    dropdown <- mkDropDown                   -- The dropdown element
    zoom1   <- mkOption "50%"                -- Elements in the dropdown list
    zoom2   <- mkOption "100%"
    zoom3   <- mkOption "150%"
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    buttons <- mkDiv
    dropDown dropdown [zoom2, zoom1, zoom3] 
    row formula [fx,input]
    row buttons [draw,diff]
    column documentBody [canvas,dropdown,formula,buttons,f'x]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    setStyle f'x "color" "darkmagenta"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_    -> do 
                                    setProp f'x "innerHTML" ""
                                    readAndDraw input can
    onEvent input KeyUp $ \code -> do
                                    setProp f'x "innerHTML" ""
                                    when (code==13) $ readAndDraw input can
      -- "Enter" key has code 13
    onEvent dropdown Change $ \_ -> zoom input dropdown can
    onEvent diff Click $ \_ -> drawDiff can input f'x


-- Calculate all points in the graph in terms of pixels
points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width,height) = calculatePoints 0

    where 
        calculatePoints x 
            | x <= fromIntegral width = 
                if y <= fromIntegral height && y >= 0 
                    then (x, y):calculatePoints (x+1) 
                    else calculatePoints (x+1)
            | otherwise = []
            where y = realToPix (eval exp (pixToReal x))
        pixToReal x = x * scale - fromIntegral width * scale / 2
        realToPix y = (- y / scale) + fromIntegral height / 2


-- Reads expression from the given input element and draws on given canvas
readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw input canvas =
    do
        text <- getValue input
        render canvas (exprToPic (getExpr text) origScale)


-- Returns an expression
getExpr :: Maybe String -> Maybe Expr
getExpr = maybe Nothing readExpr      
   

-- Zooms in and out around the middle of the graph
zoom :: Elem -> Elem -> Canvas -> IO ()
zoom input dropdown can = 
    do
        value <- getValue dropdown
        text <- getValue input
        let value' = fromJust value
        let scale = case value' of
                        "50%" -> 2
                        "100%" -> 1
                        "150%" -> 0.5
        render can (exprToPic (getExpr text) (scale * origScale))


exprToPic :: Maybe Expr -> Double -> Picture ()
exprToPic expr scale = maybe (stroke (path [])) (\e -> 
    stroke (path (points e scale (canWidth, canHeight)))) expr


-- Draws differentiate of the function in the 
-- input box over the original graph.
-- Displayes the differentiate beneath the graph
drawDiff :: Canvas -> Elem -> Elem -> IO ()
drawDiff can input f'x = do
    readAndDraw input can 
    text <- getValue input
    let expr = getExpr text
    let (textExpr, diffExpr) = case expr of
            Nothing -> ("",Nothing)
            _ -> do
                let diffEq = differentiate (fromJust expr)
                let diffText = "<i>f'</i>(<i>x</i>)=" ++ showExpr diffEq
                (diffText, Just diffEq)
 

    setProp f'x "innerHTML" textExpr
    renderOnTop can (color (RGB 139 0 139) (exprToPic diffExpr origScale))

