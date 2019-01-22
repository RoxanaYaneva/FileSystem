module Types where

type FileName = String
type FileData = String
type FileSize = Int

data FileSystem = FEmpty | File FileName FileData FileSize | Directory FileName [FileSystem] deriving (Show, Eq)