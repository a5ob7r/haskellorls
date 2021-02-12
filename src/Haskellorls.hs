{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Haskellorls
  ( run,
    renderEntriesLines,
    argParser,
    buildFiles,
    Entry.toEntries,
  )
where

import qualified Data.Functor as F
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
import qualified Haskellorls.Sort as Sort
import qualified Haskellorls.Utils as Utils
import qualified Haskellorls.WrappedText as WT
import qualified Options.Applicative as OA
import qualified System.Exit as Exit
import qualified System.Posix.Files as Files

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

  renderEntriesLinesAsList opt (Entry.toEntries files) >>= mapM_ T.putStr >> putStrLn ""

  if null noExistences
    then Exit.exitSuccess
    else Exit.exitWith $ Exit.ExitFailure 2

buildFiles :: Option.Option -> IO Entry.Files
buildFiles opt = do
  let targets = Option.targets opt
      targets' = if null targets then ["."] else targets

  Entry.buildFiles opt targets'

renderEntriesLines :: Option.Option -> [Entry.Entry] -> IO T.Text
renderEntriesLines opt entries = renderEntriesLines' opt entries F.<&> TL.toStrict . TLB.toLazyText . M.mconcat

renderEntriesLinesAsList :: Option.Option -> [Entry.Entry] -> IO [T.Text]
renderEntriesLinesAsList opt entries = renderEntriesLines' opt entries F.<&> map (TL.toStrict . TLB.toLazyText)

renderEntriesLines' :: Option.Option -> [Entry.Entry] -> IO [TLB.Builder]
renderEntriesLines' opt entries = generateEntriesLines opt entries F.<&> L.intersperse (TLB.fromText "\n\n") . map (M.mconcat . L.intersperse (TLB.fromText "\n"))

generateEntriesLines :: Option.Option -> [Entry.Entry] -> IO [[TLB.Builder]]
generateEntriesLines opt entries = do
  printers <- Decorator.buildPrinters opt

  let generator = generateEntryLines opt printers

  mapM generator entries

generateEntryLines :: Option.Option -> Decorator.Printers -> Entry.Entry -> IO [TLB.Builder]
generateEntryLines opt printers (Entry.Entry eType path contents) = do
  nodes <- fmap (Sort.sorter opt) . mapM (Node.nodeInfo path) $ contents
  let nodes' = Decorator.buildLines nodes printers $ Decorator.buildPrinterTypes opt
      addHeader = case eType of
        Entry.DIRECTORY -> \ss -> TLB.fromText (T.pack path `T.snoc` ':') : ss
        _ -> id
      addTotalBlockSize = case eType of
        Entry.FILES -> id
        _ -> (builder :)
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
