module Keyboard.Components.KeyPlate
( model
, urFlangePost
, lrFlangePost
, llFlangePost
, ulFlangePost
, urShortPost
, lrShortPost
, llShortPost
, ulShortPost
, urTallPost
, lrTallPost
, llTallPost
, ulTallPost
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
    [ cylinder 1 6 (fn 20)
    , translate (0,0,6) (sphere 1 (fn 20))
    ]
ulFlangePost = mirror (1,0,0) urFlangePost
llFlangePost = rotate (0,0,180) urFlangePost
lrFlangePost = mirror (1,0,0) llFlangePost

urShortPost :: Model3d
urShortPost = translate (9.5 - tinyDistance, 9 - tinyDistance, -keyplateDepth) $
    box tinyDistance tinyDistance keyplateDepth
ulShortPost = mirror (1,0,0) urShortPost
llShortPost = rotate (0,0,180) urShortPost
lrShortPost = mirror (1,0,0) llShortPost

urTallPost :: Model3d
urTallPost = translate (9.5 - tinyDistance, 9 - tinyDistance, -keyplateDepth) $
    box tinyDistance tinyDistance keyplateDepth
ulTallPost = mirror (1,0,0) urTallPost
llTallPost = rotate (0,0,180) urTallPost
lrTallPost = mirror (1,0,0) llTallPost

urPin :: Model3d
urPin = translate (9.5, 9, -keyplateDepth) $
    box tinyDistance tinyDistance tinyDistance
ulPin = mirror (1,0,0) urPin
llPin = rotate (0,0,180) urPin
lrPin = mirror (1,0,0) llPin
