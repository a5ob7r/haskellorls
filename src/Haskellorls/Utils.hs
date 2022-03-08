module Haskellorls.Utils
  ( getSymbolicLinkStatus,
    getFileStatus,
    readSymbolicLink,
    destFileStatusRecursive,
    linkDestPath,
    listContents,
    exclude,
    partitionExistOrNotPathes,
    validatePathExistence,
    textLengthForDisplay,
    replaceControlCharsToQuestion,
    escapeFormatter,
    escapeCharsForStdout,
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Char as C
import qualified Data.Either as E
import Data.Functor
import qualified Data.List as L
import qualified Data.Text as T
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Quote.Type as Quote
import qualified System.Directory as Directory
import qualified System.FilePath.Glob as Glob
import qualified System.FilePath.Posix as Posix
import qualified System.Posix.Files as Files

getSymbolicLinkStatus :: (MonadThrow m, MonadIO m) => FilePath -> m Files.FileStatus
getSymbolicLinkStatus = liftIO . Files.getSymbolicLinkStatus

getFileStatus :: (MonadThrow m, MonadIO m) => FilePath -> m Files.FileStatus
getFileStatus = liftIO . Files.getFileStatus

readSymbolicLink :: (MonadThrow m, MonadIO m) => FilePath -> m FilePath
readSymbolicLink = liftIO . Files.readSymbolicLink

destFileStatusRecursive :: (MonadThrow m, MonadIO m) => FilePath -> FilePath -> m Files.FileStatus
destFileStatusRecursive dirPath basePath = do
  s <- getFileStatus (linkDestPath dirPath basePath)

  if Files.isSymbolicLink s
    then do
      link <- readSymbolicLink (linkDestPath dirPath basePath)
      destFileStatusRecursive dirPath link
    else pure s

linkDestPath :: FilePath -> FilePath -> FilePath
linkDestPath parPath linkPath
  | isAbsPath linkPath = linkPath
  | otherwise = Posix.takeDirectory parPath Posix.</> linkPath

isAbsPath :: FilePath -> Bool
isAbsPath path = "/" `L.isPrefixOf` path

listContents :: (MonadThrow m, MonadIO m) => Option.Option -> FilePath -> m [FilePath]
listContents opt path = list path <&> ignoreExcluder . hideExcluder . ignoreFilter
  where
    list
      | Option.all opt || Option.noneSortExtra opt = listAllEntries
      | Option.almostAll opt = listSemiAllEntries
      | otherwise = listEntries
    ignoreFilter =
      if Option.ignoreBackups opt
        then ignoreBackupsFilter
        else id
    ignoreExcluder = case Option.ignore opt of
      "" -> id
      ptn -> exclude ptn
    hideExcluder = case Option.hide opt of
      "" -> id
      _ | isShowHiddenEntries -> id
      ptn -> exclude ptn
    isShowHiddenEntries = Option.all opt || Option.almostAll opt

listAllEntries :: (MonadThrow m, MonadIO m) => FilePath -> m [FilePath]
listAllEntries = liftIO . Directory.getDirectoryContents

listSemiAllEntries :: (MonadThrow m, MonadIO m) => FilePath -> m [FilePath]
listSemiAllEntries = liftIO . Directory.listDirectory

listEntries :: (MonadThrow m, MonadIO m) => FilePath -> m [FilePath]
listEntries path = listSemiAllEntries path <&> filter (not . isHiddenEntries)

isHiddenEntries :: FilePath -> Bool
isHiddenEntries [] = False
isHiddenEntries ('.' : _) = True
isHiddenEntries _ = False

ignoreBackupsFilter :: [FilePath] -> [FilePath]
ignoreBackupsFilter = filter (\path -> not $ "~" `L.isSuffixOf` path)

exclude :: String -> [FilePath] -> [FilePath]
exclude ptn = filter (not . Glob.match ptn')
  where
    ptn' = Glob.compile ptn

partitionExistOrNotPathes :: [FilePath] -> IO ([FilePath], [FilePath])
partitionExistOrNotPathes pathes = E.partitionEithers <$> mapM validatePathExistence pathes

validatePathExistence :: FilePath -> IO (Either FilePath FilePath)
validatePathExistence path = E.either (const $ Left path) (const $ Right path) <$> tryIO (liftIO $ getSymbolicLinkStatus path)

-- | Assumes not Latin1 charactor has double width of Latin1 charactor for display.
textLengthForDisplay :: T.Text -> Int
textLengthForDisplay = sum . map (\c -> if C.isLatin1 c then 1 else 2) . T.unpack

replaceControlCharsToQuestion :: T.Text -> T.Text
replaceControlCharsToQuestion = T.map (\c -> if C.isPrint c then c else '?')

escapeCharsForStdout :: T.Text -> T.Text
escapeCharsForStdout = T.concatMap $ \case
  '\r' -> "'$'\\r''"
  '\t' -> "'$'\\t''"
  c -> T.singleton c

-- | NOTE: There may be some missing targets.
escapeCharsForStdoutByCStyle :: T.Text -> T.Text
escapeCharsForStdoutByCStyle = T.concatMap $ \case
  '\t' -> "\\t"
  '\r' -> "\\r"
  '\n' -> "\\n"
  ' ' -> "\\ "
  '|' -> "\\|"
  '\\' -> "\\\\"
  c -> T.singleton c

escapeFormatter :: Option.Option -> T.Text -> T.Text
escapeFormatter opt = case Option.quotingStyle opt of
  _
    | Option.literal opt -> replaceControlCharsToQuestion
    | Option.escape opt -> escapeCharsForStdoutByCStyle
    | Option.hideControlChars opt && not (Option.showControlChars opt) -> replaceControlCharsToQuestion
  Quote.Literal -> replaceControlCharsToQuestion
  Quote.Shell -> replaceControlCharsToQuestion
  Quote.ShellAlways -> replaceControlCharsToQuestion
  Quote.ShellEscape -> escapeCharsForStdout
  Quote.ShellEscapeAlways -> escapeCharsForStdout
  Quote.C -> escapeCharsForStdoutByCStyle
  Quote.Escape -> escapeCharsForStdoutByCStyle
  _ | Option.toStdout opt -> escapeCharsForStdout
  _ -> id
