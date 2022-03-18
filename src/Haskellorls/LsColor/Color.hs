-- | This module implements 'LS_COLORS'.
module Haskellorls.LsColor.Color
  ( LsColors,
    Sequence (..),
    lsColors,
    module Haskellorls.LsColor.Config,
  )
where

import Control.Applicative
import Data.Bifunctor
import Data.Default
import Data.Functor
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Haskellorls.Class
import Haskellorls.Context.Type
import Haskellorls.Filemode.Entry.Type
import Haskellorls.Filemode.Permission.Type
import Haskellorls.Inode.Type
import Haskellorls.Link.Type
import Haskellorls.LsColor.Config
import Haskellorls.NodeInfo
import Haskellorls.Ownership.Type
import Haskellorls.Size.Type
import Haskellorls.Time.Type
import Haskellorls.Tree.Type
import qualified System.Environment as Env
import Prelude hiding (lookup)

lsColors :: IO LsColors
lsColors = getLSCOLORS <> getEXACOLORS <&> deserialize

getLSCOLORS :: IO T.Text
getLSCOLORS = maybe "" T.pack <$> Env.lookupEnv "LS_COLORS"

getEXACOLORS :: IO T.Text
getEXACOLORS = maybe "" T.pack <$> Env.lookupEnv "EXA_COLORS"

-- | LS_COLORS
type LsColors = Options Sequence Extensions

instance Default LsColors where
  def =
    Options
      { left = Just "\^[[",
        right = Just "m",
        end = Nothing,
        reset = Just "0",
        normal = Nothing,
        file = Nothing,
        directory = Just "01;34",
        symlink = Just "01;36",
        pipe = Just "33",
        socket = Just "01;35",
        block = Just "01;33",
        char = Just "01;33",
        missing = Nothing,
        orphan = Nothing,
        executable = Just "01;32",
        door = Just "01;35",
        setuid = Just "37;41",
        setgid = Just "30;43",
        sticky = Just "37;44",
        otherWritable = Just "34;42",
        stickyOtherWritable = Just "30;42",
        capability = Just "30;41",
        multiHardlink = Nothing,
        clearLine = Just "\^[[K",
        extension = def
      }

instance Dictionary NodeInfo Sequence LsColors where
  lookup n l@(Options {..}) = case getNodeLinkInfo n of
    Just (Left _) -> orphan
    Just (Right _) | symlink /= Just "target" -> symlink
    _ -> case pfsNodeType $ getNodeStatus n' of
      Directory -> directory
      NamedPipe -> pipe
      Socket -> socket
      BlockDevise -> block
      CharDevise -> char
      DoorsDevise -> door
      Setuid -> setuid
      Setgid -> setgid
      Sticky -> sticky
      StickyOtherWritable -> stickyOtherWritable
      OtherWritable -> otherWritable
      Executable -> executable
      File -> q `query` l
        where
          q = Query . T.pack $ getNodePath n'
      _ -> orphan
    where
      n' = toFileInfo n

instance Dictionary EntryType Sequence LsColors where
  lookup e (Options {..}) = case e of
    REGULAR -> file
    BLOCK -> block
    CHAR -> char
    DIR -> directory
    SYMLINK ->
      if symlink == Just "target"
        then Nothing
        else symlink
    FIFO -> pipe
    SOCK -> socket
    OTHER -> Nothing

instance Dictionary Permission Sequence LsColors where
  lookup p (Options {..}) = do
    ExtraOptions {..} <- extraLsColors <$> extension

    case p of
      UserPerm READ -> userReadPermBit
      UserPerm WRITE -> userWritePermBit
      UserPerm EXEC -> userExecPermBitFile
      UserPerm SETUID -> setuid
      UserPerm E_SETUID -> setuid
      GroupPerm READ -> groupReadPermBit
      GroupPerm WRITE -> groupWritePermBit
      GroupPerm EXEC -> groupExecPermBit
      GroupPerm SETGID -> setgid
      GroupPerm E_SETGID -> setgid
      OtherPerm READ -> otherReadPermBit
      OtherPerm WRITE -> otherWritePermBit
      OtherPerm EXEC -> otherExecPermBit
      OtherPerm STICKY -> sticky
      OtherPerm E_STICKY -> sticky
      _ -> Nothing

instance Dictionary Inode Sequence LsColors where
  lookup i (Options {..}) = extension >>= lookup i

instance Dictionary LinkCount Sequence LsColors where
  lookup l (Options {..}) = extension >>= lookup l

instance Dictionary UserID Sequence LsColors where
  lookup u (Options {..}) = extension >>= lookup u

instance Dictionary GroupID Sequence LsColors where
  lookup g (Options {..}) = extension >>= lookup g

instance Dictionary FileContext Sequence LsColors where
  lookup c (Options {..}) = extension >>= lookup c

instance Dictionary SizeNumberScale Sequence LsColors where
  lookup s (Options {..}) = extension >>= lookup s

instance Dictionary SizeUnitScale Sequence LsColors where
  lookup s (Options {..}) = extension >>= lookup s

instance Dictionary Datetime Sequence LsColors where
  lookup d (Options {..}) = extension >>= lookup d

instance Dictionary TreeNodePosition Sequence LsColors where
  lookup t (Options {..}) = extension >>= lookup t

instance Dictionary Query Sequence LsColors where
  lookup (Query t) (Options {..}) = extension >>= \e -> Query (T.toUpper t) `lookup` e

instance Queryable Sequence LsColors

-- | An entension to implement LS_COLORS and extra colors.
data Extensions = Extensions {sequences :: Sequences, extraLsColors :: ExtraLsColors}
  deriving (Show)

instance Default Extensions where
  def = Extensions def def

instance From Sources Extensions where
  from s = Extensions (from s) (from s)

instance Dictionary Inode Sequence Extensions where
  lookup i (Extensions {..}) = i `lookup` extraLsColors

instance Dictionary LinkCount Sequence Extensions where
  lookup l (Extensions {..}) = l `lookup` extraLsColors

instance Dictionary UserID Sequence Extensions where
  lookup u (Extensions {..}) = u `lookup` extraLsColors

instance Dictionary GroupID Sequence Extensions where
  lookup g (Extensions {..}) = g `lookup` extraLsColors

instance Dictionary FileContext Sequence Extensions where
  lookup c (Extensions {..}) = c `lookup` extraLsColors

instance Dictionary SizeNumberScale Sequence Extensions where
  lookup s (Extensions {..}) = s `lookup` extraLsColors

instance Dictionary SizeUnitScale Sequence Extensions where
  lookup s (Extensions {..}) = s `lookup` extraLsColors

instance Dictionary Datetime Sequence Extensions where
  lookup d (Extensions {..}) = d `lookup` extraLsColors

instance Dictionary TreeNodePosition Sequence Extensions where
  lookup t (Extensions {..}) = t `lookup` extraLsColors

instance Dictionary Query Sequence Extensions where
  lookup k (Extensions {..}) = k `lookup` sequences

instance Queryable Sequence Extensions

-- | Filename extension patterns or prefix them and paired sequences.
newtype Sequences = Sequences (M.Map Query Sequence)
  deriving (Show, Default)

instance From Sources Sequences where
  from = Sequences . M.fromList . map (bimap Query Sequence) . mapMaybe (uncurry f) . M.toList . unSources
    where
      f k v = case T.uncons k of
        Just (c, k') | c == '*' -> Just (T.toUpper k', v)
        _ -> Nothing

instance From T.Text Sequences where
  from = from . (deserialize :: T.Text -> Sources)

instance Deserialize Sequences

instance Dictionary Query Sequence Sequences where
  lookup k (Sequences m) = k `M.lookup` m

instance Queryable Sequence Sequences

-- | Extra colors for coloring not filename such as permissions, owner/group
-- and so on. This is not implemented on GNU ls.
data ExtraOptions a = ExtraOptions
  { userReadPermBit :: Maybe a,
    userWritePermBit :: Maybe a,
    userExecPermBitFile :: Maybe a,
    userExecPermBitOther :: Maybe a,
    groupReadPermBit :: Maybe a,
    groupWritePermBit :: Maybe a,
    groupExecPermBit :: Maybe a,
    otherReadPermBit :: Maybe a,
    otherWritePermBit :: Maybe a,
    otherExecPermBit :: Maybe a,
    sPermBitFile :: Maybe a,
    sPermBitOther :: Maybe a,
    ownerYourself :: Maybe a,
    ownerNotYourself :: Maybe a,
    groupYouBelongsTo :: Maybe a,
    groupYouNotBelongsTo :: Maybe a,
    fileSizeNumber :: Maybe a,
    fileSizeNumberByte :: Maybe a,
    fileSizeNumberKilo :: Maybe a,
    fileSizeNumberMega :: Maybe a,
    fileSizeNumberGiga :: Maybe a,
    fileSizeNumberTera :: Maybe a,
    fileSizeNumberPeta :: Maybe a,
    fileSizeNumberExa :: Maybe a,
    fileSizeNumberZetta :: Maybe a,
    fileSizeNumberYotta :: Maybe a,
    fileSizeUnitByte :: Maybe a,
    fileSizeUnitKilo :: Maybe a,
    fileSizeUnitMega :: Maybe a,
    fileSizeUnitGiga :: Maybe a,
    fileSizeUnitTera :: Maybe a,
    fileSizeUnitPeta :: Maybe a,
    fileSizeUnitExa :: Maybe a,
    fileSizeUnitZetta :: Maybe a,
    fileSizeUnitYotta :: Maybe a,
    date :: Maybe a,
    fileLink :: Maybe a,
    fileInode :: Maybe a,
    treeBranch :: Maybe a,
    fileContext :: Maybe a
  }
  deriving (Show)

instance Dictionary Inode a (ExtraOptions a) where
  lookup _ (ExtraOptions {..}) = fileInode

instance Dictionary LinkCount a (ExtraOptions a) where
  lookup _ (ExtraOptions {..}) = fileLink

instance Dictionary UserID a (ExtraOptions a) where
  lookup u (ExtraOptions {..}) = case u of
    Myself _ -> ownerYourself
    NotMyself _ -> ownerNotYourself

instance Dictionary GroupID a (ExtraOptions a) where
  lookup u (ExtraOptions {..}) = case u of
    Belongs _ -> groupYouBelongsTo
    NotBelongs _ -> groupYouNotBelongsTo

instance Dictionary FileContext a (ExtraOptions a) where
  lookup _ (ExtraOptions {..}) = fileContext

instance Dictionary SizeNumberScale a (ExtraOptions a) where
  lookup (SizeNumberScale s) (ExtraOptions {..}) = case s of
    BYTE -> fileSizeNumber
    KILO -> fileSizeNumberKilo
    MEGA -> fileSizeNumberMega
    GIGA -> fileSizeNumberGiga
    TERA -> fileSizeNumberTera
    PETA -> fileSizeNumberPeta
    EXA -> fileSizeNumberExa
    ZETTA -> fileSizeNumberZetta
    YOTTA -> fileSizeNumberYotta

instance Dictionary SizeUnitScale a (ExtraOptions a) where
  lookup (SizeUnitScale s) (ExtraOptions {..}) = case s of
    BYTE -> fileSizeUnitByte
    KILO -> fileSizeUnitKilo
    MEGA -> fileSizeUnitMega
    GIGA -> fileSizeUnitGiga
    TERA -> fileSizeUnitTera
    PETA -> fileSizeUnitPeta
    EXA -> fileSizeUnitExa
    ZETTA -> fileSizeUnitZetta
    YOTTA -> fileSizeUnitYotta

instance Dictionary Datetime a (ExtraOptions a) where
  lookup _ (ExtraOptions {..}) = date

instance Dictionary TreeNodePosition a (ExtraOptions a) where
  lookup _ (ExtraOptions {..}) = treeBranch

-- | Extra color sequences.
type ExtraLsColors = ExtraOptions Sequence

instance Default ExtraLsColors where
  def =
    ExtraOptions
      { userReadPermBit = Just "1;32",
        userWritePermBit = Just "1;31",
        userExecPermBitFile = Just "1;33",
        userExecPermBitOther = Just "1;93",
        groupReadPermBit = Just "32",
        groupWritePermBit = Just "31",
        groupExecPermBit = Just "33",
        otherReadPermBit = Just "32",
        otherWritePermBit = Just "31",
        otherExecPermBit = Just "33",
        sPermBitFile = Just "96",
        sPermBitOther = Just "96",
        ownerYourself = Just "35",
        ownerNotYourself = Nothing,
        groupYouBelongsTo = Just "35",
        groupYouNotBelongsTo = Nothing,
        fileSizeNumber = Just "1;32",
        fileSizeNumberByte = Just "1;32",
        fileSizeNumberKilo = Just "1;32",
        fileSizeNumberMega = Just "1;32",
        fileSizeNumberGiga = Just "1;32",
        fileSizeNumberTera = Just "1;32",
        fileSizeNumberPeta = Just "1;32",
        fileSizeNumberExa = Just "1;32",
        fileSizeNumberZetta = Just "1;32",
        fileSizeNumberYotta = Just "1;32",
        fileSizeUnitByte = Just "32",
        fileSizeUnitKilo = Just "32",
        fileSizeUnitMega = Just "32",
        fileSizeUnitGiga = Just "32",
        fileSizeUnitTera = Just "32",
        fileSizeUnitPeta = Just "32",
        fileSizeUnitExa = Just "32",
        fileSizeUnitZetta = Just "32",
        fileSizeUnitYotta = Just "32",
        date = Just "34",
        fileLink = Just "36",
        fileInode = Just "36",
        treeBranch = Just "90",
        fileContext = Just "36"
      }

instance From Sources ExtraLsColors where
  from s =
    ExtraOptions
      { userReadPermBit = Sequence <$> "ur" `lookup'` s <|> userReadPermBit,
        userWritePermBit = Sequence <$> "uw" `lookup'` s <|> userWritePermBit,
        userExecPermBitFile = Sequence <$> "ux" `lookup'` s <|> userExecPermBitFile,
        userExecPermBitOther = Sequence <$> "ue" `lookup'` s <|> userExecPermBitOther,
        groupReadPermBit = Sequence <$> "gr" `lookup'` s <|> groupReadPermBit,
        groupWritePermBit = Sequence <$> "gw" `lookup'` s <|> groupWritePermBit,
        groupExecPermBit = Sequence <$> "gx" `lookup'` s <|> groupExecPermBit,
        otherReadPermBit = Sequence <$> "tr" `lookup'` s <|> otherReadPermBit,
        -- TODO: `tw` is conflict between GNU LS and EXA
        otherWritePermBit = Sequence <$> "tw'" `lookup'` s <|> otherWritePermBit,
        otherExecPermBit = Sequence <$> "tx" `lookup'` s <|> otherExecPermBit,
        sPermBitFile = Sequence <$> "su" `lookup'` s <|> sPermBitFile,
        sPermBitOther = Sequence <$> "sf" `lookup'` s <|> sPermBitOther,
        ownerYourself = Sequence <$> "uu" `lookup'` s <|> ownerYourself,
        ownerNotYourself = Sequence <$> "un" `lookup'` s <|> ownerNotYourself,
        groupYouBelongsTo = Sequence <$> "gu" `lookup'` s <|> groupYouBelongsTo,
        groupYouNotBelongsTo = Sequence <$> "gn" `lookup'` s <|> groupYouNotBelongsTo,
        fileSizeNumber = Sequence <$> "sn" `lookup'` s <|> fileSizeNumber,
        fileSizeNumberByte = Sequence <$> "nb" `lookup'` s <|> fileSizeNumberByte,
        fileSizeNumberKilo = Sequence <$> "nk" `lookup'` s <|> fileSizeNumberKilo,
        fileSizeNumberMega = Sequence <$> "nm" `lookup'` s <|> fileSizeNumberMega,
        fileSizeNumberGiga = Sequence <$> "ng" `lookup'` s <|> fileSizeNumberGiga,
        fileSizeNumberTera = Sequence <$> "nt" `lookup'` s <|> fileSizeNumberTera,
        fileSizeNumberPeta = Sequence <$> "np" `lookup'` s <|> fileSizeNumberPeta,
        fileSizeNumberExa = Sequence <$> "ne" `lookup'` s <|> fileSizeNumberExa,
        fileSizeNumberZetta = Sequence <$> "nz" `lookup'` s <|> fileSizeNumberZetta,
        fileSizeNumberYotta = Sequence <$> "ny" `lookup'` s <|> fileSizeNumberYotta,
        fileSizeUnitByte = Sequence <$> "ub" `lookup'` s <|> fileSizeUnitByte,
        fileSizeUnitKilo = Sequence <$> "uk" `lookup'` s <|> fileSizeUnitKilo,
        fileSizeUnitMega = Sequence <$> "um" `lookup'` s <|> fileSizeUnitMega,
        fileSizeUnitGiga = Sequence <$> "ug" `lookup'` s <|> fileSizeUnitGiga,
        fileSizeUnitTera = Sequence <$> "ut" `lookup'` s <|> fileSizeUnitTera,
        fileSizeUnitPeta = Sequence <$> "up" `lookup'` s <|> fileSizeUnitPeta,
        fileSizeUnitExa = Sequence <$> "ue" `lookup'` s <|> fileSizeUnitExa,
        fileSizeUnitZetta = Sequence <$> "uz" `lookup'` s <|> fileSizeUnitZetta,
        fileSizeUnitYotta = Sequence <$> "uy" `lookup'` s <|> fileSizeUnitYotta,
        date = Sequence <$> "da" `lookup'` s <|> date,
        fileLink = Sequence <$> "lc'" `lookup'` s <|> fileLink,
        fileInode = Sequence <$> "in" `lookup'` s <|> fileInode,
        treeBranch = treeBranch,
        fileContext = fileContext
      }
    where
      ExtraOptions {..} = def
      lookup' :: T.Text -> Sources -> Maybe T.Text
      lookup' = lookup

-- | An ANSI escape sequence parameter such as '0', '01;34', '38;5;81' and so
-- on.
--
-- NOTE: This doesn't confirm whether or not the parameter is valid. Users must
-- guarantee that the value is valid as ANSI escape sequence parameter.
newtype Sequence = Sequence {unSequence :: T.Text}
  deriving (Eq, Show, IsString, Semigroup, Monoid)

instance From Sequence T.Text where
  from = unSequence

instance Serialize Sequence

instance From T.Text Sequence where
  from = Sequence

instance Deserialize Sequence