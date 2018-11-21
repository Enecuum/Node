module Graphics.GD.Extra where

import           Universum
import qualified Graphics.GD as GD
import           Data.Complex

type Point = Complex Double

testPng :: IO ()
testPng = makeImage (1000, 1000) "test.png" $ \image ->
    --drawArrow (100 :+ 500) (900 :+ 500) (rgb 0 0 0) image
    mkCircle (500 :+ 500) 300 image


makeImage :: GD.Size -> FilePath -> (GD.Image -> IO a) -> IO ()
makeImage size filePath f = do
    image <- GD.newImage size
    GD.fillImage white image
    void $ f image
    GD.savePngFile filePath image

drawCircle :: Point -> Int -> GD.Color -> GD.Image -> IO ()
drawCircle (x :+ y ) s = GD.drawFilledEllipse (fromEnum x, fromEnum y) (s, s)

rgb :: Word8 -> Word8 -> Word8 -> GD.Color
rgb r g b = GD.rgb (fromEnum r) (fromEnum g) (fromEnum b)

mkCircle :: Point -> Point -> GD.Image -> IO ()
mkCircle center r image = forM_ [0..19] $ \f -> do
    drawCircle (center + mkPolar 1 (f*pi/10)*r) 10 black image
    drawArrow (center + mkPolar 1 ((f + 9)*pi/10)*r) (center + mkPolar 1 (f*pi/10)*r) black image

drawArrow :: Point -> Point -> GD.Color -> GD.Image -> IO ()
drawArrow x1 x2 c img = do
    drawLine x1 x2 c img
    let st = 0.04*x1 + 0.96*x2
    drawLine (x2 + (st - x2) * mkPolar 1 (pi/32)) x2 c img
    drawLine (x2 + (st - x2) * mkPolar 1 (-pi/32)) x2 c img

drawLine :: Point -> Point -> GD.Color -> GD.Image -> IO ()
drawLine (x1 :+ y1) (x2 :+ y2) = GD.drawLine (fromEnum x1, fromEnum y1) (fromEnum x2, fromEnum y2)

black :: GD.Color
black = rgb 0 0 0

white :: GD.Color
white = rgb 255 255 255