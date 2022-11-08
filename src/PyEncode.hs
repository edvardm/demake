{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module PyEncode where

import Data.Graph (Graph, Vertex)
import qualified Data.Graph as G
import Data.List (partition)
import Data.Makefile
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import System.Posix.Escape (escape)
import qualified Text.RE.PCRE as RE
import Text.RE.Replace as REPL

fromCode :: T.Text -> Builder
fromCode = fromText

type VarName = Text

type TaskName = Text

type InvFile = [PyEntry]

type AdjTuple = (Task, TaskName, [TaskName])

newtype InvFun = InvFun TaskName
    deriving (Show, Eq)

newtype InvCmd = InvCmd Text
    deriving (Show, Eq)

newtype InvDep = InvDep Text
    deriving (Show, Eq)

data InvParam = Bool VarName | Str VarName
    deriving (Show, Eq)

data PyEntry = PyConst Const | PyTask Task
    deriving (Show, Eq)

class Encodable a where
    encode :: a -> Builder

data Task = Task
    { name :: TaskName
    , dependencies :: [InvDep]
    , commands :: [InvCmd]
    , parameters :: [InvParam]
    }
    deriving (Show, Eq)

data Const = Const VarName Text
    deriving (Show, Eq)

defTask :: Task
defTask = Task{name = "mytask", dependencies = [], commands = [], parameters = []}

toPyEntries :: Makefile -> [PyEntry]
toPyEntries Makefile{entries = es} = assigns ++ catMaybes tasksM
  where
    assigns = map PyConst $ mapMaybe assignToPyM es
    tasksM = map (trimDepsM . PyTask . injectOpts) $ taskFlt topoSorted
    topoSorted = takeList (topoSort allTasks) allTasks
    allTasks = mapMaybe entryToTaskM es

    injectOpts t@Task{name} = t{parameters = params}
      where
        params = map (Str . snd) $ catMaybes [Map.lookup name taskOpts]
        taskOpts :: Map.Map Text (VarName, Text)
        taskOpts = Map.fromList $ mapMaybe f es
          where
            f (RuleRef (Target t') ConditionalAssign key val) = Just (t', (key, val))
            f _ = Nothing

    -- note that phony rules ARE included, this is just rule which declares
    -- target as phony
    taskFlt = filter $ \Task{name} -> name `notStartWithAnyOf` skipPrefixes
      where
        notStartWithAnyOf x = none (`T.isPrefixOf` x)
        skipPrefixes = [".PHONY", "ifeq", "ifneq"]

    trimDepsM (PyConst _) = Nothing
    trimDepsM (PyTask t@(Task{dependencies})) =
        Just $ PyTask t{dependencies = deps'}
      where
        deps' = map InvDep $ filter (`elem` taskNames) $ map depName dependencies
        taskNames = Data.Set.fromList $ mapMaybe nameOf allTasks
        nameOf Task{name} = Just name

assignToPyM :: Entry -> Maybe Const
assignToPyM = \case
    Assignment _ k v -> Just $ Const (pyTranslate k) v
    _ -> Nothing

-- TODO: do in single pass, cast to proper types here -> out value Maybe PyEntry
entryToTaskM :: Entry -> Maybe Task
entryToTaskM e = case e of
    Rule (Target t) ds cs ->
        Just $
            defTask{name = t, dependencies = depToInv ds, commands = cmdToInv cs}
    _ -> Nothing
  where
    cmdToInv = map (\(Command c) -> InvCmd c)
    depToInv = map (\(Dependency d) -> InvDep d)

-- encoders
instance Encodable InvFile where
    encode entries =
        imports
            <> foldMap ((<>) nl . encode) consts
            <> foldMap ((<>) funSep . encode) tasks
      where
        (consts, tasks) = partition isConst entries

        isConst :: PyEntry -> Bool
        isConst = \case
            (PyConst _) -> True
            _ -> False

instance Encodable PyEntry where
    encode (PyConst (Const k v)) = bld [k, " = ", quoteVar v]
    encode (PyTask t) = encode t

instance Encodable [InvDep] where
    encode cmds = fromText "@task" <> argDecl cmds
      where
        argDecl [] = mempty -- don't want pre=[]
        argDecl deps = bld ["(pre=[", T.intercalate ", " (map f deps), "])"]
          where
            f (InvDep d) = pyTranslate d

instance Encodable TaskName where
    encode name = bld ["def ", pyTranslate name, "(c):"]

instance Encodable InvCmd where
    encode (InvCmd cmd)
        | "ifeq" `T.isPrefixOf` cmd = mempty
        | "ifneq" `T.isPrefixOf` cmd = mempty
        | "#" `T.isPrefixOf` cmd = indent <> fromText cmd <> nl
        | otherwise =
            indent
                <> fromText "c.run("
                <> (fromText . formatCmd) cmd
                <> fromText ")"
                <> nl
      where
        formatCmd = fStringFmt . (escape . T.unpack) . rmNlsTabs
        rmNlsTabs = T.replace "\t" "" . T.replace "\\\n" " "
        fStringFmt src =
            let re = [RE.re|\$\($(.*?)\)|]
                subst =
                    if matched $ src RE.?=~ re
                        then 'f' : replaceAll "{$1}" (src RE.*=~ re)
                        else src
             in T.pack subst

instance Encodable Task where
    encode (Task n ds cs _) = mjoin "\n" [encode ds, encode n, mkBody cs]
      where
        mkBody [] = indent <> fromString "pass"
        mkBody xs = foldMap encode xs

-- for convenience, so that we can call `asString` for Makefile in
instance Encodable Makefile where
    encode = encode . toPyEntries

asString :: Encodable a => a -> String
asString = T.unpack . L.toStrict . toLazyText . encode

-- helpers
-- return first list if it is not empty, otherwise return second
takeList :: [a] -> [a] -> [a]
takeList as bs = if null as then bs else as

quoteVar :: (Semigroup a, IsString a) => a -> a
quoteVar v = "\"" <> v <> "\""

none :: Foldable t => (a -> Bool) -> t a -> Bool
none f = not . any f

indent :: Builder
indent = "    "

bld :: [T.Text] -> Builder
bld = foldr1 (<>) . map fromText

imports :: Builder
imports = fromText "from invoke import task" <> nl

pyTranslate :: Text -> Text
pyTranslate = foldr1 (.) $ map (`T.replace` "_") [".", "-"]

nl :: Builder
nl = singleton '\n'

funSep :: Builder
funSep = nl <> nl

mjoin :: Monoid a => a -> [a] -> a
mjoin s = foldr1 (\a acc -> a <> s <> acc)

depName :: InvDep -> Text
depName (InvDep c) = c

-- Topologically sort graph. Note: returns empty list in case of acyclic graph
topoSort :: [Task] -> [Task]
topoSort tasks = reverse topoSorted
  where
    (graph, nodeFromVertex, _) = mkGraph tasks
    topoSorted = map (fst3 . nodeFromVertex) $ G.topSort graph
    fst3 (x, _, _) = x

mkGraph :: [Task] -> (Graph, Vertex -> AdjTuple, TaskName -> Maybe Vertex)
mkGraph = G.graphFromEdges . map (\t -> (t, name t, map depName $ dependencies t))
