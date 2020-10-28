module Main where

import Data.List(transpose)
import Data.List.Split
import System.Environment(getArgs, getEnv)
import System.FilePath.Glob
import System.FilePath.Posix
import System.Posix.Files
import System.Posix.Types
import System.Posix.User

import System.Directory.Extra
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601

data Node = Node { name :: String
                 , mode :: String
                 , owner :: String
                 , group :: String
                 , size :: String
                 , mtime :: String
                 } deriving (Show)

main :: IO ()
main = do
  path <- pure head <*> getArgs
  contents <- listContents $ path
  nodes <- mapM node contents
  let additionals = decorator nodes fs
      names = map name nodes
  namesWithColor <- mapM (\name -> pure colorize <*> (escapeSecuenceFromLSCOLOR name) <*> pure name) names
  mapM_ putStrLn . map (\(a, b) -> a ++ " " ++ b) $ zip additionals namesWithColor
    where fs = [mode, owner, group, size, mtime]

decorator :: [Node] -> [Node -> String] -> [String]
decorator nodes = map join . transpose . map (\f -> decorator' nodes f)
  where join = foldr1 (\l r -> l ++ " " ++ r)

decorator' :: [Node] -> (Node -> String) -> [String]
decorator' nodes f = map (leftPadding ' ' maxLength) nodes'
  where nodes' = map f nodes
        maxLength = maximum . map length $ nodes'

leftPadding :: Char -> Int -> String -> String
leftPadding c n s | n > len = pad ++ s
  where len = length s
        padSize = n - len
        pad = take padSize . repeat $ c
leftPadding _ _ s = s

{-| Colorize String with ansii escape sequence.
-}
colorize :: String -> String -> String
colorize esc str = "\^[[" ++ esc ++ "m" ++ str ++ "\^[[m"

escapeSecuenceFromLSCOLOR :: String -> IO String
escapeSecuenceFromLSCOLOR name = do
  indicators <- colorIndicators
  return . snd . headWithDefault (compile "", "") . filter (\(ptn, _) -> match ptn name) $ indicators

headWithDefault :: a -> [a] -> a
headWithDefault d [] = d
headWithDefault _ xs = head xs

colorIndicators :: IO [(Pattern, String)]
colorIndicators = do
  envLSCOLORS <- getLSCOLORS
  return . map makePatternEscapePair . filter (\s -> '*' == (head s)) . endBy ":" $ envLSCOLORS

makePatternEscapePair :: String -> (Pattern, String)
makePatternEscapePair s = (ptn, esc)
  where pairs = splitOn "=" s
        ptn = compile (head pairs)
        esc = last pairs

getLSCOLORS :: IO String
getLSCOLORS = getEnv "LS_COLORS"

node :: FilePath -> IO Node
node path = do
  let name = takeFileName path
  status <- getFileStatus path
  mode <- fileModeOf status
  owner <- userNameOf status
  group <- groupNameOf status
  size <- fileSizeOf status
  mtime <- modificationTimeOf status
  return $ Node { name = name
                , mode = mode
                , owner = owner
                , group = group
                , size = size
                , mtime = mtime
                }

userNameOf :: FileStatus -> IO String
userNameOf = fmap userName . getUserEntryForID . fileOwner

groupNameOf :: FileStatus -> IO String
groupNameOf = fmap groupName . getGroupEntryForID . fileGroup

fileModeOf :: FileStatus -> IO String
fileModeOf = return . allFileMode

fileSizeOf :: FileStatus -> IO String
fileSizeOf = return . fmap show fileSize

modificationTimeOf :: FileStatus -> IO String
modificationTimeOf = return . modifiedTime . modificationTime

modifiedTime :: EpochTime -> String
modifiedTime = iso8601Show . posixSecondsToUTCTime . realToFrac

allFileMode :: FileStatus -> String
allFileMode status = foldl (\l r -> l ++ r mode) "" [ownerFileMode, groupFileMode, otherFileMode]
  where
    mode = fileMode status

readableMode :: String
readableMode = "r"

writableMode :: String
writableMode = "w"

executableMode :: String
executableMode = "x"

nullMode :: String
nullMode = "-"

ownerFileMode :: FileMode -> String
ownerFileMode = classFileMode [isOwnerReadMode, isOwnerWriteMode, isOwnerExecuteMode]

groupFileMode :: FileMode -> String
groupFileMode = classFileMode [isGroupReadMode, isGroupWriteMode, isGroupExecuteMode]

otherFileMode :: FileMode -> String
otherFileMode = classFileMode [isOtherReadMode, isOtherWriteMode, isOtherExecuteMode]

classFileMode :: [(FileMode -> Bool)] -> FileMode -> String
classFileMode fs mode = foldl (\l r -> l ++ r) "" . map (\(f, perm) -> perm f mode) $ zip fs perms
  where
    perms = [readablePerm, writablePerm, executablePerm]

readablePerm :: (FileMode -> Bool) -> FileMode -> String
readablePerm f mode = if f mode
                         then readableMode
                         else nullMode

writablePerm :: (FileMode -> Bool) -> FileMode -> String
writablePerm f mode = if f mode
                         then writableMode
                         else nullMode

executablePerm :: (FileMode -> Bool) -> FileMode -> String
executablePerm f mode = if f mode
                           then executableMode
                           else nullMode

hasFileMode :: FileMode -> FileMode -> Bool
hasFileMode x y = x == intersectFileModes x y

isOwnerReadMode :: FileMode -> Bool
isOwnerReadMode = hasFileMode ownerReadMode

isOwnerWriteMode :: FileMode -> Bool
isOwnerWriteMode = hasFileMode ownerWriteMode

isOwnerExecuteMode :: FileMode -> Bool
isOwnerExecuteMode = hasFileMode ownerExecuteMode

isGroupReadMode :: FileMode -> Bool
isGroupReadMode = hasFileMode groupReadMode

isGroupWriteMode :: FileMode -> Bool
isGroupWriteMode = hasFileMode groupWriteMode

isGroupExecuteMode :: FileMode -> Bool
isGroupExecuteMode = hasFileMode groupExecuteMode

isOtherReadMode :: FileMode -> Bool
isOtherReadMode = hasFileMode otherReadMode

isOtherWriteMode :: FileMode -> Bool
isOtherWriteMode = hasFileMode otherWriteMode

isOtherExecuteMode :: FileMode -> Bool
isOtherExecuteMode = hasFileMode otherExecuteMode
