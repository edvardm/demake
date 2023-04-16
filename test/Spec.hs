{-# LANGUAGE OverloadedStrings #-}

import Data.Makefile
import Data.Makefile.Parse (parseMakefileContents)
import qualified Data.Text as T
import PyEncode
import Test.Hspec

-- import           Test.QuickCheck
firstTask :: Makefile -> PyEntry
firstTask = head . toPyEntries

textToMake :: [T.Text] -> Either String Makefile
textToMake = parseMakefileContents . T.unlines

taskNames :: [PyEntry] -> [TaskName]
taskNames = map f
 where
  f (PyTask t) = name t
  f _ = error "internal logic error, should be used with tasks only"

makeRulesToTasknames :: Makefile -> [TaskName]
makeRulesToTasknames = taskNames . toPyEntries

lineComment :: Text -> Maybe LineComment
lineComment = Just . LineComment

main :: IO ()
main = hspec $ do
  describe "Parsing Makefile" $ do
    it "parses includes" $ do
      textToMake
        [ "include foo"
        , "-include bar"
        ]
        `shouldBe` Right Makefile{entries = [Include "foo" Nothing, OtherLine "", Include "bar" Nothing, OtherLine ""]}

    it "ditches ifeq/ifneq" $ do
      textToMake
        [ "ifeq (,$(findstring t,$(MAKEFLAGS)))"
        , "\tkoi"
        , "else"
        , "\tbar"
        , "endif"
        ]
        `shouldBe` Right Makefile{entries = [Conditional]}

      textToMake
        [ "ifneq (,...,...))"
        , "\tkoi"
        , "else"
        , "\tbar"
        , "endif"
        ]
        `shouldBe` Right Makefile{entries = [Conditional]}

    it "parses comments" $ do
      textToMake
        ["include foo  # a comment"]
        `shouldBe` Right Makefile{entries = [Include "foo" (lineComment "a comment")]}

    it "parses exported constants" $ do
      textToMake
        ["export DEBUG:=1"]
        `shouldBe` Right Makefile{entries = [Assignment SimpleAssign "DEBUG" "1"]}

  describe "Makefile to Pyinvoke" $ do
    it "skips .PHONY declarations" $ do
      toPyEntries
        ( Makefile
            { entries = [Rule (Target ".PHONY") [Dependency "clean"] []]
            }
        )
        `shouldBe` []

    it "skips ifeq" $ do
      toPyEntries
        (Makefile{entries = [Rule (Target "ifeq ..") [] [Command "a"]]})
        `shouldBe` []

    it "skips ifneq" $ do
      toPyEntries
        (Makefile{entries = [Rule (Target "ifneq ..") [] [Command "a"]]})
        `shouldBe` []

    it "parses global assignments" $ do
      toPyEntries
        ( Makefile
            { entries =
                [ Assignment SimpleAssign "BIN" "myfile"
                , OtherLine ""
                , Rule (Target "clean") [] [Command "rm -rf $(BIN)"]
                ]
            }
        )
        `shouldBe` [ PyConst (Const "BIN" "myfile")
                   , PyTask
                      defTask
                        { name = "clean"
                        , commands = [InvCmd "rm -rf $(BIN)"]
                        }
                   ]

    it "uses f-string interpolation for constants" $ do
      asString
        ( Makefile
            { entries =
                [ Assignment SimpleAssign "BIN" "myfile"
                , OtherLine ""
                , Rule (Target "clean") [] [Command "rm -rf $(BIN)"]
                ]
            }
        )
        `shouldBe` unlines
          [ "from invoke import task"
          , ""
          , "BIN = \"myfile\""
          , ""
          , "@task"
          , "def clean(c):"
          , "    c.run(f\"rm -rf {BIN}\")"
          ]

    it "replaces all vars in a string" $ do
      asString
        ( Makefile
            { entries =
                [ Assignment SimpleAssign "BIN" "myfile"
                , OtherLine ""
                , Assignment SimpleAssign "target" "progname"
                , Rule (Target "clean") [] [Command "rm -rf $(BIN) $(target)"]
                ]
            }
        )
        `shouldBe` unlines
          [ "from invoke import task"
          , ""
          , "BIN = \"myfile\""
          , "target = \"progname\""
          , ""
          , "@task"
          , "def clean(c):"
          , "    c.run(f\"rm -rf {BIN} {target}\")"
          ]

    -- FIXME: opts should be split to different types, not just simple string
    it "parses per-rule assignments" $
      do
        toPyEntries
          ( Makefile
              { entries =
                  [ RuleRef
                      (Target "foo")
                      ConditionalAssign
                      "opts"
                      "fname --bar --foo"
                  , Rule (Target "foo") [] [Command "cmd $(opts)"]
                  ]
              }
          )
        `shouldBe` [ PyTask
                      defTask
                        { name = "foo"
                        , parameters = [Str "fname --bar --foo"]
                        , commands = [InvCmd "cmd $(opts)"]
                        }
                   ]
    it "converts rules without dependencies" $ do
      firstTask
        (Makefile{entries = [Rule (Target "clean") [] [Command "rm"]]})
        `shouldBe` PyTask defTask{name = "clean", commands = [InvCmd "rm"]}

    it "adds single dependency as pre-task" $ do
      toPyEntries
        ( Makefile
            { entries =
                [ Rule (Target "a") [] [Command "run_a"]
                , Rule (Target "dev_init") [Dependency "a"] []
                ]
            }
        )
        `shouldBe` map
          PyTask
          [ defTask{name = "a", commands = [InvCmd "run_a"]}
          , defTask
              { name = "dev_init"
              , dependencies = [InvDep "a"]
              }
          ]

    it "adds multiple dependencies as pre-tasks" $ do
      makeRulesToTasknames
        Makefile
          { entries =
              [ Rule (Target "a") [] []
              , Rule (Target "b") [] []
              , Rule (Target "dev_init") [Dependency "a", Dependency "b"] []
              ]
          }
        `shouldBe` ["a", "b", "dev_init"]

    it "can sort tasks topologically by dependencies" $ do
      map
        name
        ( topoSort
            [ defTask{name = "c", dependencies = [InvDep "b"]}
            , defTask{name = "b", dependencies = [InvDep "d", InvDep "a"]}
            , defTask{name = "a"}
            , defTask{name = "d"}
            ]
        )
        `shouldBe` ["a", "d", "b", "c"]

    it "filters out dependencies if couldn't work out how to create" $
      do
        toPyEntries
          ( Makefile
              { entries =
                  [ Rule
                      (Target "foo")
                      [Dependency "foo.c", Dependency "bar"]
                      [Command "run_foo"]
                  , Rule (Target "bar") [] [Command "run_bar"]
                  ]
              }
          )
        `shouldBe` [ PyTask
                      (defTask{name = "bar", commands = [InvCmd "run_bar"]})
                   , PyTask
                      ( defTask
                          { name = "foo"
                          , dependencies = [InvDep "bar"]
                          , commands = [InvCmd "run_foo"]
                          }
                      )
                   ]

  describe "Tasks" $ do
    it "converts dashes to underscores" $ do
      asString (PyTask defTask{name = "a-name"})
        `shouldBe` "@task\ndef a_name(c):\n    pass"

    it "sets 'pass' as function body for empty tasks" $ do
      asString defTask{name = "fun"}
        `shouldBe` "@task\ndef fun(c):\n    pass"

    it "replaces dot in task names and deps with underscore" $ do
      asString (defTask{name = ".hidden"})
        `shouldBe` "@task\ndef _hidden(c):\n    pass"

      asString (defTask{name = "foo.o", dependencies = [InvDep "foo.c"]})
        `shouldBe` "@task(pre=[foo_c])\ndef foo_o(c):\n    pass"

  describe "Dependencies" $ do
    it "adds single depenency to pre list" $ do
      asString [InvDep "dep"] `shouldBe` "@task(pre=[dep])"

    it "converts dashes to underscores" $ do
      asString [InvDep "dev-init"] `shouldBe` "@task(pre=[dev_init])"

    it "joins multiple deps by comma" $ do
      asString [InvDep "a", InvDep "b"] `shouldBe` "@task(pre=[a, b])"

  describe "Commands" $ do
    it "wraps command in quotes" $ do
      asString (InvCmd "ls -l") `shouldBe` "    c.run(\"ls -l\")\n"

    it "double-escapes escaped quotes in commands" $ do
      asString (InvCmd "ls \"fname\"") `shouldBe` "    c.run(\"ls \\\"fname\\\"\")\n"

    it "ignores preceding at-char in commands" $ do
      asString (InvCmd "@touch $!") `shouldBe` "    c.run(\"touch $!\")\n"

  describe "Comments" $ do
    it "renders comments" $ do
      pending

  describe "Includes" $ do
    it "skips includes (convert those separately for now)" $ do
      entryToTaskM (Include "foo" Nothing) `shouldBe` Nothing
      entryToTaskM (Include "foo" (lineComment "bar")) `shouldBe` Nothing
