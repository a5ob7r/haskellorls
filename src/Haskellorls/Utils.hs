module Haskellorls.Utils
  ( getSymbolicLinkStatus,
    getFileStatus,
    readSymbolicLink,
    destFileStatusRecursive,
    linkDestPath,
    listContents,
    exclude,
    textLengthForDisplay,
    escapeFormatter,
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.Char
import Data.Functor
import qualified Data.Text as T
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Quote.Type as Quote
import RawFilePath.Directory
import System.FilePath.Glob
import System.FilePath.Posix.ByteString
import qualified System.Posix.Files.ByteString as Files

getSymbolicLinkStatus :: (MonadThrow m, MonadIO m) => RawFilePath -> m Files.FileStatus
getSymbolicLinkStatus = liftIO . Files.getSymbolicLinkStatus

getFileStatus :: (MonadThrow m, MonadIO m) => RawFilePath -> m Files.FileStatus
getFileStatus = liftIO . Files.getFileStatus

readSymbolicLink :: (MonadThrow m, MonadIO m) => RawFilePath -> m RawFilePath
readSymbolicLink = liftIO . Files.readSymbolicLink

destFileStatusRecursive :: (MonadThrow m, MonadIO m) => RawFilePath -> RawFilePath -> m Files.FileStatus
destFileStatusRecursive dirPath basePath = do
  s <- getFileStatus (linkDestPath dirPath basePath)

  if Files.isSymbolicLink s
    then do
      link <- readSymbolicLink (linkDestPath dirPath basePath)
      destFileStatusRecursive dirPath link
    else pure s

linkDestPath :: RawFilePath -> RawFilePath -> RawFilePath
linkDestPath parPath linkPath
  | isAbsPath linkPath = linkPath
  | otherwise = takeDirectory parPath </> linkPath

isAbsPath :: RawFilePath -> Bool
isAbsPath path = "/" `B.isPrefixOf` path

listContents :: (MonadThrow m, MonadIO m) => Option.Option -> RawFilePath -> m [RawFilePath]
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

listAllEntries :: (MonadThrow m, MonadIO m) => RawFilePath -> m [RawFilePath]
listAllEntries = liftIO . getDirectoryFiles

listSemiAllEntries :: (MonadThrow m, MonadIO m) => RawFilePath -> m [RawFilePath]
listSemiAllEntries = liftIO . listDirectory

listEntries :: (MonadThrow m, MonadIO m) => RawFilePath -> m [RawFilePath]
listEntries path = listSemiAllEntries path <&> filter (not . isHiddenEntries)

isHiddenEntries :: RawFilePath -> Bool
isHiddenEntries s
  | B.null s = False
  | "." `B.isPrefixOf` s = True
isHiddenEntries _ = False

ignoreBackupsFilter :: [RawFilePath] -> [RawFilePath]
ignoreBackupsFilter = filter (\path -> not $ "~" `B.isSuffixOf` path)

-- | TODO: Use 'ByteString' native globbing instead of 'String' one.
exclude :: String -> [RawFilePath] -> [RawFilePath]
exclude ptn = map encodeFilePath . filter (not . match ptn') . map decodeFilePath
  where
    ptn' = compile ptn

-- | Assumes not Latin1 charactor has double width of Latin1 charactor for display.
textLengthForDisplay :: T.Text -> Int
textLengthForDisplay = T.foldr (\c acc -> acc + if isLatin1 c then 1 else 2) 0

replaceControlCharsToQuestion :: T.Text -> T.Text
replaceControlCharsToQuestion = T.map (\c -> if isPrint c then c else '?')

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
