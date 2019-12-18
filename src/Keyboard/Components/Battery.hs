module Keyboard.Components.Battery
( model
) where

import Graphics.OpenSCAD

-- Model of an ICR18650 2200mAh 3.7C battery

model = cylinder 9.1 69.3 (fn 40)
