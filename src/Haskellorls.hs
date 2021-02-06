{-# LANGUAGE OverloadedStrings #-}

module Haskellorls
  ( run,
    renderEntriesLines,
    argParser,
  )
where

import qualified Data.Functor as F
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Haskellorls.Decorator as Decorator
import qualified Haskellorls.Entry as Entry
import qualified Haskellorls.Grid as Grid
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Sort as Sort
import qualified Options.Applicative as OA

run :: [String] -> IO ()
run args = do
  options <- argParser args

  if Option.version options
    then showVersion
    else runWith options

runWith :: Option.Option -> IO ()
runWith opt = T.putStrLn =<< renderEntriesLines opt

renderEntriesLines :: Option.Option -> IO T.Text
renderEntriesLines opt = generateEntriesLines opt F.<&> T.intercalate "\n\n" . map (T.intercalate "\n")

generateEntriesLines :: Option.Option -> IO [[T.Text]]
generateEntriesLines opt = do
  let targets = Option.targets opt
      targets' = if null targets then ["."] else targets

  files <- Entry.buildFiles opt targets'
  printers <- Decorator.buildPrinters opt

  let generator = generateEntryLines opt printers

  mapM generator $ Entry.toEntries files

generateEntryLines :: Option.Option -> Decorator.Printers -> Entry.Entry -> IO [T.Text]
generateEntryLines opt printers (Entry.Entry eType path contents) = do
  nodes <- fmap (Sort.sorter opt) . mapM (Node.nodeInfo path) $ contents
  let nodes' = Decorator.buildLines nodes printers $ Decorator.buildPrinterTypes opt
      addHeader = case eType of
        Entry.FILES -> id
        _ -> \ss -> T.pack path `T.snoc` ':' : ss

  colLen <- Grid.virtualColumnSize opt

  return . addHeader . Grid.renderGrid $ Grid.buildValidGrid colLen nodes'

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
