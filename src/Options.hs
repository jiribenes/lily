{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Options
  ( Command(..)
  , Options
  , optSource
  , optVerbose
  , optClangArguments
  , optCommand
  , parseOptions
  )
where

import           Control.Lens
import           Options.Applicative

data Command = Elaborate
             | Infer
             | Lint
  deriving stock (Show, Eq, Ord)

data Options = Options {
    _optCommand :: !Command,
    _optSource :: !FilePath,
    _optVerbose :: !Bool,
    _optClangArguments :: ![String]
} deriving stock (Show, Eq, Ord)
makeLenses ''Options

parseOptions :: IO Options
parseOptions = execParser optionsParser

optionsParser :: ParserInfo Options
optionsParser = info
  (    (   Options
       <$> commandOption
       <*> sourceOption
       <*> verboseOption
       <*> clangArgumentsOption
       )
  <**> helper
  )
  (fullDesc <> progDesc "lily: C++ linear linter" <> header
    "lily - a research linter for C++ based on linear types"
  )
 where
  commandOption  = hsubparser (commandElaborate <> commandInfer <> commandLint)
  commandElaborate = command
    "elaborate"
    (info (pure Elaborate) (progDesc "Just elaborate the program into Core"))
  commandInfer = command
    "infer"
    (info (pure Infer)
          (progDesc "Elaborate the program into Core and infer Core types")
    )
  commandLint = command
    "lint"
    (info
      (pure Lint)
      (progDesc "Elaborate the program, infer Core types and lint the C++ file")
    )

  sourceOption :: Parser FilePath
  sourceOption =
    strArgument (metavar "SOURCE_FILE" <> help "Path to the source file")

  verboseOption :: Parser Bool
  verboseOption = switch
    (short 'v' <> long "verbose" <> help "Verbose (debug) output to stdin")

  clangArgumentsOption :: Parser [String]
  clangArgumentsOption =
    many $ strArgument (help "Leftover arguments for Clang")
