{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Monad
import Data.Makefile (Makefile)
import Data.Makefile.Parse (parseAsMakefile)
import Options.Applicative
import PyEncode (
  asString,
  toPyEntries,
 )
import System.Directory (doesFileExist)
import System.Exit (die)
import Text.Pretty.Simple

appVersion :: String
appVersion = "v0.1.5"

data Args = Args
  { file :: String
  , debug :: Bool
  , parseOnly :: Bool
  , force :: Bool
  , output :: FilePath
  }

input :: Parser Args
input =
  Args
    <$> strArgument
      ( showDefault
          <> value "Makefile"
          <> metavar "MAKEFILE"
          <> showDefault
          <> help "Makefile to read"
      )
    <*> switch (long "debug" <> short 'd' <> help "Produce debug output")
    <*> switch
      ( long "parse-only"
          <> short 'p'
          <> help
            "Implies --debug, produces only parsed structure"
      )
    <*> switch
      (long "force" <> short 'f' <> help "Overwrite tasks.py if it exists")
    <*> strOption
      ( long "output"
          <> short 'o'
          <> showDefault
          <> value "tasks.py"
          <> help
            "tasks file to write"
      )

parseFile :: FilePath -> IO Makefile
parseFile fpath = do
  parsed <- parseAsMakefile fpath
  case parsed of
    Left _ -> error $ "unable to parse " ++ fpath ++ ", aborting"
    Right mf -> return mf

run :: Args -> IO ()
run Args{file = f, debug = False, output = optOutfile, force = optForce, parseOnly = False} =
  abortIfExists optOutfile optForce
    >> parseFile f
    >>= writer optOutfile
      . asString
      . toPyEntries
run Args{file = f, debug = True, output = optOutfile, force = optForce, parseOnly = False} =
  do
    abortIfExists optOutfile optForce
    mkFl <- parseFile f
    debugParsed mkFl
    (writer optOutfile . asString . toPyEntries) mkFl
run Args{file = f, parseOnly = True} = parseFile f >>= debugParsed

debugParsed :: Makefile -> IO ()
debugParsed mkFl = do
  pPrint mkFl
  putStrLn "=>"
  pPrint $ toPyEntries mkFl

abortIfExists :: FilePath -> Bool -> IO ()
abortIfExists optOutfile optForce = do
  v <- doesFileExist optOutfile
  Control.Monad.when (v && not optForce) $
    die "tasks.py already exist, aborting (use --force to override)"

-- | "-" causes strings to be printed to stdout
writer :: FilePath -> String -> IO ()
-- TODO: use Data.Text.IO instead of String functions
writer outFile = case outFile of
  "-" -> putStrLn
  _ -> writeFile outFile

main :: IO ()
main = run =<< execParser opts
 where
  version = infoOption appVersion (long "version" <> help "Show version")
  opts =
    info (helper <*> version <*> input) $
      fullDesc
        <> header "demake -- Makefile to invoke converter"
        <> progDesc "Generate PyInvoke stub from given Makefile"
