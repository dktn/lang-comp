module Main where

import Universum

import Replace
import BSF

main :: IO ()
main = do
  replaceTest
  bsfTest
