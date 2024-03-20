module Main where

import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)

data Flag
  = Verbose
  | Version
  | Input String
  | Output String
  | LibDir String
  deriving (Show)

options :: [OptDescr Flag]
options =
  [ Option ['v'] ["verbose"] (NoArg Verbose) "chatty output on stderr"
  , Option ['V', '?'] ["version"] (NoArg Version) "show version number"
  , Option ['o'] ["output"] (OptArg outp "FILE") "output FILE"
  , Option ['c'] [] (OptArg inp "FILE") "input FILE"
  , Option ['L'] ["libdir"] (ReqArg LibDir "DIR") "library directory"
  ]

inp, outp :: Maybe String -> Flag
outp = Output . fromMaybe "stdout"

inp = Input . fromMaybe "stdin"

-- compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts :: [String] -> IO ()
compilerOpts argv = do
  prog <- getProgName
  case getOpt Permute options argv of
      (o, n, []) -> dump o n
      (_, _, errs) ->
        ioError (userError (concat errs ++ usageInfo (header prog) options))
  where
      header name = "Usage: " ++ name ++ " [OPTION...] files..."
      dump fs args = do
        print fs
        print args

-- main :: IO ([Flag], [String])
main :: IO ()
main = do
  args <- getArgs
  compilerOpts args