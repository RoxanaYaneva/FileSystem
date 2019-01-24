{-# LANGUAGE OverloadedStrings #-}
module Types where

import Prelude hiding (FilePath)
import Data.Text      (Text)
import qualified Data.Text as Text

type FileName = Text
type FileData = Text
type FilePath = Text
type FileSize = Int

data File = FEmpty | OFile FileName FileData FileSize | Directory FileName [File] deriving (Show, Eq)
newtype FileSystem = FileSystem {root :: File} deriving Show
            