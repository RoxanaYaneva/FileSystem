module Types where

type FileName = String
type FileData = String
type FileSize = Int

data File = FEmpty | OFile FileName FileData FileSize | Directory FileName [File] deriving (Show, Eq)
newtype FileSystem = FileSystem {root :: File} deriving Show