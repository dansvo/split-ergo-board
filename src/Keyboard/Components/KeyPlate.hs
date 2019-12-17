module Keyboard.Components.KeyPlate
( model
, urFlangePost
, lrFlangePost
, llFlangePost
, ulFlangePost
, urPost
, lrPost
, llPost
, ulPost
, urPin
, lrPin
, llPin
, ulPin
) where

import Graphics.OpenSCAD

keyplateDepth = 2.5

tinyDistance = 0.000001

model = difference
    (translate (-totalWidth/2, -totalHeight/2, -keyplateDepth) (box totalWidth totalHeight keyplateDepth))
    (translate (-holeWidth/2,  -holeWidth/2,  -keyplateDepth-1) (box holeWidth holeWidth (keyplateDepth+2)))
  where
    totalWidth  = 19
    totalHeight = 18
    holeWidth = 13.8
    

urFlangePost :: Model3d
urFlangePost = translate (9.5, 9, -keyplateDepth) $
    union
    [ cylinder 1 6 (fn 40)
    , translate (0,0,6) (sphere 1 (fn 40))
    ]
ulFlangePost = mirror (1,0,0) urFlangePost
llFlangePost = rotate (0,0,180) urFlangePost
lrFlangePost = mirror (1,0,0) llFlangePost

urPost :: Model3d
urPost = translate (9.5 - tinyDistance, 9 - tinyDistance, -keyplateDepth) $
    box tinyDistance tinyDistance keyplateDepth
ulPost = mirror (1,0,0) urPost
llPost = rotate (0,0,180) urPost
lrPost = mirror (1,0,0) llPost

urPin :: Model3d
urPin = translate (9.5, 9, -keyplateDepth) $
    box tinyDistance tinyDistance tinyDistance
ulPin = mirror (1,0,0) urPin
llPin = rotate (0,0,180) urPin
lrPin = mirror (1,0,0) llPin
