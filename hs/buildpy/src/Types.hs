module Types where

type Version = String

type Name = String

type Url = String

type Platform = String

data BuildType
    = Static
    | Shared
    | Framework
    deriving (Eq, Ord, Enum, Show)

data SizeType
    = Max
    | Mid
    | Min
    deriving (Eq, Ord, Enum, Show)

data PythonVersion
    = Py311
    | Py312
    | Py313
    deriving (Eq, Ord, Enum, Show)

class Buildable a where
    prefix :: a -> FilePath
    srcDir :: a -> FilePath
    buildDir :: a -> FilePath
    download :: a -> IO ()
    build :: a -> IO ()
