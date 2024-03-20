module Types where

type Version = String

type Name = String

type Url = String

class Buildable a where
  prefix :: a -> FilePath
  srcDir :: a -> FilePath
  buildDir :: a -> FilePath
  download :: a -> IO ()
  build :: a -> IO ()
