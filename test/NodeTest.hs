module NodeTest where

import Data.Functor ((<&>))
import Foreign.C.Types (CTime(CTime))

import System.IO.Temp (emptySystemTempFile)
import System.Directory (removeFile)
import System.FilePath.Posix (takeFileName)
import System.Posix.Files (setFileMode, unionFileModes, nullFileMode, ownerModes, groupModes, otherModes, ownerReadMode, ownerWriteMode, setFileTimes)
import System.Posix.User (getRealUserID, getUserEntryForID, userName, getRealGroupID, getGroupEntryForID, groupName)
import Test.Tasty.HUnit (assertEqual)

import Haskellorls.Node

unit_nodeName :: IO ()
unit_nodeName = do
  filepath <- emptySystemTempFile ""
  nd <- node filepath
  assertEqual "Return node's filename" (takeFileName filepath) $ nodeName nd
  removeFile filepath

unit_nodeMode :: IO ()
unit_nodeMode = do
  filepath <- emptySystemTempFile ""

  setFileMode filepath $ foldl unionFileModes nullFileMode [ownerModes, groupModes, otherModes]
  nd1 <- node filepath
  assertEqual "Return all permissions" "rwxrwxrwx" $ nodeMode nd1

  setFileMode filepath $ foldl unionFileModes nullFileMode [ownerModes]
  nd2 <- node filepath
  assertEqual "Return all owner permissions" "rwx------" $ nodeMode nd2

  setFileMode filepath $ foldl unionFileModes nullFileMode [ownerReadMode, ownerWriteMode]
  nd3 <- node filepath
  assertEqual "Return owner read and write permissions" "rw-------" $ nodeMode nd3

  removeFile filepath

unit_nodeOwner :: IO ()
unit_nodeOwner = do
  filepath <- emptySystemTempFile ""

  uname <- getRealUserID >>= getUserEntryForID <&> userName
  nd <- node filepath
  assertEqual "Return file owner name" uname $ nodeOwner nd

  removeFile filepath

unit_nodeGroup :: IO ()
unit_nodeGroup = do
  filepath <- emptySystemTempFile ""

  gname <- getRealGroupID >>= getGroupEntryForID <&> groupName
  nd <- node filepath
  assertEqual "Return file group name" gname $ nodeGroup nd

  removeFile filepath

unit_nodeSize :: IO ()
unit_nodeSize = do
  filepath <- emptySystemTempFile ""

  nd <- node filepath
  assertEqual "Return file size" "0" $ nodeSize nd

  removeFile filepath

unit_nodeMtime :: IO ()
unit_nodeMtime = do
  filepath <- emptySystemTempFile ""
  let ctime = CTime 1605414190
  setFileTimes filepath ctime ctime

  nd <- node filepath
  assertEqual "Return modified time" "2020-11-15T04:23:10Z" $ nodeMtime nd

  removeFile filepath
