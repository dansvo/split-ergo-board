module Keyboard.Components.PowerSwitch 
( model
, blockage
) where

import Graphics.OpenSCAD

-- Model of a SPDT Slide Switch

handle = translate (0.5,0,-1) (box 2 2 2)

handleBlockage = translate (-2.5, 0, -1.2) (box 5 2.2 2.4)

pin = translate (-0.4, -12, -0.25) (box 0.8 12 0.5)

pins = translate (-1.8, 0, 0) pin <> pin <> translate (1.8, 0, 0) pin

pinBlockage = translate (-2.75, -12, -0.25) (box 5.5 12 0.5)

body = translate (-width/2,  -depth, -height/2) offsetSwitchBody
    where
        offsetSwitchBody = box width depth height
        width = 11.8
        height = 4
        depth = 6.4

model = union [body, handle, pins ]

blockage =  union [body, handleBlockage, pinBlockage]
