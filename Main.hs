{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Implicit

switch_width = 18
switch_clearance = 2
keyhole_depth = 4

out = union
    [ rect3R 0 (0,0,0) (20,20,20)
    , translate (20,20,20) (sphere 15)
    ]

keyhole = difference
    [ rect3R 0 (-half_switch_width-2, -half_switch_width-2, -half_keyhole_depth) (half_switch_width+2, half_switch_width+2, half_keyhole_depth) 
    , rect3R 0 (-half_switch_width, -half_switch_width, -half_keyhole_depth-1) (half_switch_width, half_switch_width, half_keyhole_depth+1) 
    ]
  where
    half_switch_width = switch_width / 2
    half_keyhole_depth = keyhole_depth / 2
    

main :: IO ()
main = do
    writeSCAD3 1 "keyhole.scad" keyhole
    writeSCAD3 1 "test.scad" out
    putStrLn "Done writing scad files"

