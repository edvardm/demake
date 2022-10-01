{- |
 Original by: Nicolas Mattia <nicolas@nmattia.com>
 URL: https://github.com/nmattia/makefile
 License: MIT

 Parsing functions modified by Edvard Majakari <edvard@majakari.net> to provide basic
 support for include directives, comments, per-rule assignments and filtering conditionals
-}

module Data.Makefile.Parse (
    I.parseMakefile,
    I.parseAsMakefile,
    I.parseMakefileContents,
    I.makefile,
    I.entry,
    I.assignment,
    I.variableName,
    I.assignmentType,
    I.rule,
    I.command,
    I.target,
    I.dependency,
    I.otherLine,
    I.toEscapedLineEnd,
) where

import qualified Data.Makefile.Parse.Internal as I
