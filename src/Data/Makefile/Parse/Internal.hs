{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
 Original by: Nicolas Mattia <nicolas@nmattia.com>
 URL: https://github.com/nmattia/makefile
 License: MIT

 Parsing functions modified by Edvard Majakari <edvard@majakari.net> to provide basic
 support for include directives, comments, per-rule assignments and filtering conditionals
-}

-- TODO: backport changes to Nicolas' original parser

module Data.Makefile.Parse.Internal where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text as A
import Data.Foldable
import Data.Functor (($>))
import Data.Makefile
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prelude hiding (takeWhile)

parseMakefile :: IO (Either String Makefile)
parseMakefile = parseOnly makefile <$> T.readFile "Makefile"

parseAsMakefile :: FilePath -> IO (Either String Makefile)
parseAsMakefile f = parseOnly makefile <$> T.readFile f

parseMakefileContents :: T.Text -> Either String Makefile
parseMakefileContents = parseOnly makefile

parseAll :: Parser a -> T.Text -> Either String a
parseAll p = parseOnly (p <* endOfInput)

-- | Parsers
makefile :: Parser Makefile
makefile = Makefile <$> many' entry

entry :: Parser Entry
entry =  conditional <|> include <|> comment <|> assignment <|> ruleName <|>rule <|> otherLine

assignment :: Parser Entry
assignment = do
    (asgnT, var, val) <- assignmentM
    return $ Assignment asgnT var val

assignmentM :: Parser (AssignmentType, T.Text, T.Text)
assignmentM = do
    var <- variableName
    asgnT <- assignmentType
    val <- toEscapedLineEnd
    return (asgnT, var, val)

ruleName :: Parser Entry
ruleName = do
    tgt <- target
    (asgnT, var, val) <- assignmentM
    return $ RuleRef tgt asgnT var val

{- | Read chars while some ('Parser', monadic) predicate is 'True'.
 XXX: extremely inefficient.
-}
takeWhileM :: (Char -> Parser Bool) -> Parser T.Text
takeWhileM a = T.pack . reverse <$> go []
  where
    go cs = do
        c <- anyChar
        True <- a c
        go (c : cs) <|> pure (c : cs)

variableName :: Parser T.Text
variableName = stripped $ takeWhileM go
  where
    go '+' =
        peekChar' >>= \case
            '=' -> return False
            _c -> return True
    go '?' =
        peekChar' >>= \case
            '=' -> return False
            _c -> return True
    go '!' =
        peekChar' >>= \case
            '=' -> return False
            _c -> return True
    -- those chars are not allowed in variable names
    go ':' = return False
    go '#' = return False
    go '=' = return False
    go (isEndOfLine -> True) = return False
    go _c = return True

assignmentType :: Parser AssignmentType
assignmentType =
    ("=" $> RecursiveAssign)
        <|> ("+=" $> AppendAssign)
        <|> ("?=" $> ConditionalAssign)
        <|> ("!=" $> ShellAssign)
        <|> (":=" $> SimpleAssign)
        <|> ("::=" $> SimplePosixAssign)

rule :: Parser Entry
rule =
    Rule
        <$> target
        <*> (many' dependency <* (takeWhile (not . isEndOfLine) <* endOfLine'))
        <*> many' command

-- | Succeeds on 'endOfLine' (line end) or if the end of input is reached.
endOfLine' :: Parser ()
endOfLine' = endOfLine <|> (atEnd >>= check)
  where
    check True = pure ()
    check False = mzero

command :: Parser Command
command = Command <$> recipeLine

recipeLine :: Parser T.Text
recipeLine = char '\t' *> recipeLineContents ""
  where
    recipeLineContents pre = do
        cur <- takeWhile $ \c -> c /= '\\' && not (isEndOfLine c)
        asum -- Multi-line
            [ char '\\'
                *> endOfLine
                *> (void (char '\t') <|> pure ())
                *> recipeLineContents (pre <> cur <> "\\\n")
            , -- Just EOL or EOF
              endOfLine' $> (pre <> cur)
            , -- It was just a backslash within a recipe line, we're not doing
              -- anything particular
              char '\\' *> recipeLineContents (pre <> cur <> "\\")
            ]

-- | Parser for a (rule) target
target :: Parser Target
target = Target <$> go (stripped (takeWhile1 (`notElem` [':', ' ']) <* char ':'))
  where
    -- takes care of some makefile target quirks
    go :: Parser a -> Parser a
    go p =
        takeWhile (liftA2 (||) (== ' ') (== '\t'))
            *> ( peekChar >>= \case
                    Just '#' -> mzero
                    Just '\n' -> mzero
                    _ -> p
               )

-- | Parser for a (rule) dependency
dependency :: Parser Dependency
dependency = Dependency <$> (sameLine <|> newLine)
  where
    sameLine =
        takeWhile (== ' ') *> takeWhile1 (`notElem` [' ', '\n', '#', '\\'])

    newLine =
        takeWhile (== ' ') *> char '\\' *> char '\n' *> (sameLine <|> newLine)

include :: Parser Entry
include =
    Include
        <$> ((string "include " <|> string "-include") *> A.skipSpace *> consumeUntil [' ', '\n', '#'])
        <* toLineEnd
        <*> optional (LineComment <$> commentTextP)

comment :: Parser Entry
comment = Comment <$> commentTextP

commentTextP :: Parser T.Text
commentTextP =
    some "#"
        *> many' (char ' ')
        *> takeWhile1 (`notElem` ['\n', '\\'])
        <* char
            '\n'

consumeUntil :: Foldable t => t Char -> Parser T.Text
consumeUntil cs = takeWhile1 (`notElem` cs)

conditional :: Parser Entry
conditional = do
  (string "ifeq" <|> string "ifneq") *> manyTill anyChar (string "endif") *> endOfLine
  return Conditional

{- | Catch all, used for
    * comments, empty lines
    * lines that failed to parse
-}
otherLine :: Parser Entry
otherLine = OtherLine <$> go
  where
    go =
        asum -- Typical case of empty line
            [ endOfLine $> ""
            , -- Either a line of spaces and/or comment, or a line that we failed to
              -- parse
              takeWhile1 (not . isEndOfLine) <* endOfLine
            ]


toLineEnd :: Parser T.Text
toLineEnd = takeWhile (`notElem` ['\n', '#'])

{- | Get the contents until the end of the (potentially multi) line. Multiple
 lines are separated by a @\\@ char and individual lines will be stripped and
 spaces will be interspersed.

 The final @\n@ character is consumed.
-}
toEscapedLineEnd :: Parser T.Text
toEscapedLineEnd = T.unwords . filter (not . T.null) <$> go
  where
    go = do
        l <- toLineEnd <* (void (char '\n') <|> pure ())
        case T.stripSuffix "\\" l of
            Nothing -> return [T.strip l]
            Just l' -> (T.strip l' :) <$> go

-- Helpers
stripped :: Parser T.Text -> Parser T.Text
stripped = fmap T.strip
