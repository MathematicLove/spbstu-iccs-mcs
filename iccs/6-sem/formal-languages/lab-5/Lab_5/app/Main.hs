{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import           Lib                      (setup)
import           Graphics.UI.Threepenny   (defaultConfig, startGUI)

main :: IO ()
main = do
  putStrLn "Starting Pixel Pattern Generator on http://localhost:8081"
  startGUI defaultConfig setup
