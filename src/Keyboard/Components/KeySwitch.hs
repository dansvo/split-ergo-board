module Keyboard.Components.KeySwitch
( model
, blockage
) where

import Graphics.OpenSCAD

keyCap = translate (0, 0, 4.5) $ union [upperBit, lowerBit]
    where
        upperBit = translate (0, 0, 1.5) $ linearExtrude 1.5 0 (0.8,0.8) 3 1 (fn 2) $ translate (-8.7, -8.2) (rectangle 17.4 16.4)
        lowerBit = linearExtrude 1.5 0 (1,1) 3 1 (fn 2) $ translate (-8.7, -8.2) (rectangle 17.4 16.4)

model = keyCap

blockage = translate (-9.5,-9,1.2) (box 19 18 10)
