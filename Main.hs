{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text      (Text)
import FileSystem
import Commands

main :: IO ()
main = input "/" fileSystem