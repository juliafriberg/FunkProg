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

readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw input canvas =
    do
        text <- getValue input
        renderCanvas canvas originalScale (getExpr text)

renderCanvas :: Canvas -> Double -> Maybe Expr -> IO ()
renderCanvas canvas scale Nothing = render canvas (stroke (path []))
renderCanvas canvas scale expr = render canvas (stroke (path (points (fromJust expr) scale (canWidth, canHeight))))

getExpr :: Maybe String -> Maybe Expr
getExpr input = maybe Nothing readExpr input      


main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    f'x     <- mkHTML "<i>f'</i>(<i>x</i>)="
    input   <- mkInput 40 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
    diff    <- mkButton "Differentiate"
    dropdown <- mkDropDown
    zoom1   <- mkOption "50%"
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
    column documentBody [canvas,dropdown,formula,buttons]

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
    onEvent dropdown Change $ \_ -> zoom input dropdown can
    onEvent diff Click $ \_ -> drawDiff can input
      -- "Enter" key has code 13

drawDiff :: Canvas -> Elem -> IO ()
drawDiff can input = do
    readAndDraw input can 
    text <- getValue input
    let expr = getExpr text
    let pic = case expr of
            Nothing -> stroke (path [])
            _ -> stroke (path (points (differentiate (fromJust expr)) originalScale (canWidth, canHeight)))
    
    renderOnTop can (color (RGB 0 255 0) pic)


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
        renderCanvas can (scale * originalScale) (getExpr text)

points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width,height) = calculatePoints 0

    where 
        calculatePoints x 
            | x <= fromIntegral width = if y <= fromIntegral height && y >= 0 then (x, y):calculatePoints (x+1) else calculatePoints (x+1)
            | otherwise = []
            where y = realToPix (eval exp (pixToReal x))
        pixToReal x = x * scale - fromIntegral width * scale / 2
        realToPix y = (- y / scale) + fromIntegral height / 2

