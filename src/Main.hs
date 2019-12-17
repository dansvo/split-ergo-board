{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.OpenSCAD

import Keyboard

import qualified Keyboard.Components.PowerSwitch as PowerSwitch
import qualified Keyboard.Components.KeyPlate as KeyPlate


main :: IO ()
main = do
    writeFile "keyboard.scad" $ render $ Keyboard.model
    putStrLn "Done writing scad files"

