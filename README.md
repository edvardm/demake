# demake

Version 0.1.0

## Overview

Helper to convert Makefiles to Python invoke files.

## Rationale

Makefiles typically contain rules to build files based on dependencies, whereas invoke is used for task automation. These are two separate things.

GNU Make is a powerful tool, so it is often used for task automation as well, mainly because it is already present in many development environments, and most people are familiar with the basic syntax

Problem is, Make is not very suitable for complex rules. Error handling, condition logic, passing arbitrary command line arguments etc is not where it really shines. 

In the end there shouldn't even be battle between Make and task automation tools. In fact, in some cases it makes totally sense to have both PyInvoke for convenient task automation, and GNU Make for incremental builds. PyInvoke, like many task automation tools, doesn't offer support for building items automatically based on extension, or running build targets based on file modification time. In such cases, it would be totally reasonable to use PyInvoke for front-end to developers, and let Make handle incremental builds and nothing else.

Sure, complex make tasks can be extracted to shell scripts, but then the same arguments about awkward programmability would arise as with Make.

## Purpose of demake

demake tries to help you here by converting existing Makefile to PyInvoke tasks.py. It is by no means complete and it is quite likely that you need to modify generated `tasks.py` before it is useful, with the exception of very trivial Makefiles. It should still save you from some manual work, and for me it was a good excuse to something bit more serious with Haskell than just to dabble with ghci solving Project Euler problems.

## Sample run

## TODO

Ideas only, haven't committed to anything yet

- [Justfile](https://github.com/casey/just) support
- [Shake](https://github.com/casey/just) support 

## Credits

Nicolas Mattia's [Makefile library](https://github.com/nmattia/makefile), 
which I modified a bit to support parsing of top-level and inline comments

