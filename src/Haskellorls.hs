{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Haskellorls
  ( run,
    argParser,
    buildFiles,
    Entry.toEntries,
  )
where

import qualified Data.List as L
import qualified Data.Monoid as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Haskellorls.Decorator as Decorator
import qualified Haskellorls.Entry as Entry
import qualified Haskellorls.Grid as Grid
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Size.Decorator as Size
import qualified Haskellorls.Sort.Method as Sort
import qualified Haskellorls.Tree as Tree
import qualified Haskellorls.Utils as Utils
import qualified Haskellorls.WrappedText as WT
import qualified Options.Applicative as OA
import qualified System.Exit as Exit
import qualified System.Posix.Files as Files

data Operation
  = Newline
  | PrintEntry
      { execEntry :: Entry.Entry,
        execOption :: Option.Option
      }

newtype EntryPrinter = EntryPrinter {getEntryPrinter :: Entry.Entry -> IO T.Text}

run :: [String] -> IO ()
run args = do
  options <- argParser args

  if Option.version options
    then showVersion
    else run' options

run' :: Option.Option -> IO ()
run' opt = do
  files@Entry.Files {..} <- buildFiles opt

  mapM_ Utils.outputNoExistPathErr noExistences

  printers <- Decorator.buildPrinters opt
  let entryPrinter = EntryPrinter $ fmap (TL.toStrict . TLB.toLazyText . M.mconcat . L.intersperse (TLB.fromText "\n")) . generateEntryLines opt printers

  exec entryPrinter $ L.intersperse Newline $ map (`PrintEntry` opt) (Entry.toEntries files)

  if null noExistences
    then Exit.exitSuccess
    else Exit.exitWith $ Exit.ExitFailure 2

-- | NOTE: Execute output operation with stack data structure. This way may
-- cause performance down about total execution time. But user can see output
-- to stdout immediately. It leads to good user experiences. This focuses large
-- output situation like --recursive/-R option.
--
-- Another way is concatenating many text using builder aa long as possible. It
-- reduces text building cost maximally. But can not output until get all text
-- and concatenate them. So it causes output delay. This is bad user
-- experiences.
exec :: EntryPrinter -> [Operation] -> IO ()
exec _ [] = pure ()
exec printer (op : stack) = do
  entries <- L.intersperse Newline <$> eval printer op
  let entries' = if null entries then entries else Newline : entries

  exec printer (entries' <> stack)

eval :: EntryPrinter -> Operation -> IO [Operation]
eval printer op = case op of
  Newline -> do
    T.putStrLn ""
    pure []
  PrintEntry {..} -> do
    T.putStrLn =<< getEntryPrinter printer execEntry
    map (`PrintEntry` opt) <$> Entry.entryToDirectoryEntries execOption execEntry
    where
      opt = execOption {Option.level = Tree.decreaseDepth $ Option.level execOption}

buildFiles :: Option.Option -> IO Entry.Files
buildFiles opt = do
  let targets = Option.targets opt
      targets' = if null targets then ["."] else targets

  Entry.buildFiles opt targets'

generateEntryLines :: Option.Option -> Decorator.Printers -> Entry.Entry -> IO [TLB.Builder]
generateEntryLines opt printers Entry.Entry {..} = do
  nodes <- fmap (Sort.sorter opt) . mapM (Node.nodeInfo entryPath) $ entryContents
  let nodes' = Decorator.buildLines nodes printers $ Decorator.buildPrinterTypes opt
      addHeader = case entryType of
        Entry.DIRECTORY -> \ss -> TLB.fromText (T.pack entryPath `T.snoc` ':') : ss
        _ -> id

      -- Add total block size header only about directries when long style layout.
      addTotalBlockSize = case entryType of
        Entry.FILES -> id
        _
          | Decorator.isLongStyle opt -> (builder :)
          | otherwise -> id
          where
            builder = TLB.fromText . T.concat . ("total " :) . map (T.concat . WT.toList) . Size.toTotalBlockSize opt $ map (Files.fileSize . Node.nodeInfoStatus) nodes

  colLen <- Grid.virtualColumnSize opt

  return . addHeader . addTotalBlockSize . Grid.renderGrid $ Grid.buildValidGrid colLen nodes'

argParser :: [String] -> IO Option.Option
argParser args = OA.handleParseResult presult
  where
    presult = OA.execParserPure pprefs pinfo args
    pprefs = OA.prefs OA.helpLongEquals
    pinfo = Option.opts

showVersion :: IO ()
showVersion = T.putStrLn version

version :: T.Text
version = "v0.1.0.0"
