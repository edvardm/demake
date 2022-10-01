{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
 Original by: Nicolas Mattia <nicolas@nmattia.com>
 URL: https://github.com/nmattia/makefile
 License: MIT

 Parsing functions modified by Edvard Majakari <edvard@majakari.net> to provide basic
 support for include directives, comments and filtering conditionals
-}
module Data.Makefile where

import Data.String (IsString)
import qualified Data.Text as T

type Text = T.Text

newtype Makefile = Makefile {entries :: [Entry]}
  deriving (Show, Read, Eq)

newtype LineComment = LineComment Text
  deriving (Show, Read, Eq)

data Entry
  = Rule Target [Dependency] [Command]
  | RuleRef Target AssignmentType T.Text T.Text
  | Assignment AssignmentType Text Text
  | Comment Text
  | Include Text (Maybe LineComment)
  | Conditional -- for now just ignored
  | OtherLine Text
  deriving (Show, Read, Eq)

data AssignmentType
  = RecursiveAssign
  | SimpleAssign
  | SimplePosixAssign
  | ConditionalAssign
  | ShellAssign
  | AppendAssign
  deriving (Show, Read, Eq, Enum, Bounded)

newtype Target = Target Text
  deriving (Show, Read, Eq, IsString)

newtype Dependency = Dependency Text
  deriving (Show, Read, Eq, IsString)

newtype Command = Command Text
  deriving (Show, Read, Eq, IsString)
