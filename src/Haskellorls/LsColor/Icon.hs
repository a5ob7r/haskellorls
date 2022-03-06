-- | This module implements 'LS_ICONS.
module Haskellorls.LsColor.Icon
  ( LsIcons,
    Icon (..),
    lsIcons,
    module Haskellorls.Class,
  )
where

import Control.Applicative
import Data.Bifunctor
import Data.Default
import Data.Functor
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import Data.String
import qualified Data.Text as T
import Haskellorls.Class
import Haskellorls.LsColor.Config
import Haskellorls.NodeInfo
import qualified System.Environment as Env
import qualified System.FilePath.Posix as Posix
import Prelude hiding (lookup)

lsIcons :: IO LsIcons
lsIcons = getLSICONS <&> deserialize

getLSICONS :: IO T.Text
getLSICONS = Maybe.maybe "" T.pack <$> Env.lookupEnv "LS_ICONS"

-- | LS_ICONS
type LsIcons = Options Icon Icons

instance Default LsIcons where
  def =
    Options
      { left = Nothing,
        right = Nothing,
        end = Nothing,
        reset = Nothing,
        normal = Just "\xf016",
        file = Just "\xf016",
        directory = Just "\xf115",
        symlink = Just "\xf481",
        pipe = Just "\xfce3",
        socket = Just "\xf6a7",
        block = Just "\xfc29",
        char = Just "\xe601",
        missing = Nothing,
        orphan = Just "\xf127",
        executable = Nothing,
        door = Just "\xfd18",
        setuid = Nothing,
        setgid = Nothing,
        sticky = Nothing,
        otherWritable = Nothing,
        stickyOtherWritable = Nothing,
        capability = Nothing,
        multiHardlink = Nothing,
        clearLine = Nothing,
        extension = def
      }

instance Dictionary NodeInfo Icon LsIcons where
  lookup n l@(Options {..}) = case getNodeLinkInfo n of
    Just (Left _) -> orphan
    Just (Right _) -> symlink
    _ -> case pfsNodeType $ getNodeStatus n of
      NamedPipe -> pipe
      Socket -> socket
      BlockDevise -> block
      CharDevise -> char
      Orphan -> orphan
      Directory -> directory
      _ -> Query filename `query` l <|> file
      where
        filename = T.pack . Posix.takeFileName $ getNodePath n

instance Dictionary Query Icon LsIcons where
  lookup (Query t) (Options {..}) = extension >>= \e -> Query (T.toUpper t) `lookup` e

instance Queryable Icon LsIcons

-- | Filename patterns and corresponsing icons.
newtype Icons = Icons {unIcons :: M.Map Query Icon}
  deriving (Show, Default)

instance From Sources Icons where
  from = Icons . M.fromList . map (bimap Query Icon) . Maybe.mapMaybe (uncurry f) . M.toList . unSources
    where
      f k v = case T.uncons k of
        Just (c, k') | c == '*' -> Just (T.toUpper k', v)
        _ -> Nothing

instance From T.Text Icons where
  from = from . (from :: T.Text -> Sources)

instance Deserialize Icons

instance Dictionary Query Icon Icons where
  lookup q (Icons i) = q `M.lookup` i

instance Queryable Icon Icons

-- | An icon character.
--
-- NOTE: This assumes a single character, but no validation. So users must
-- guarantee it.
newtype Icon = Icon {unIcon :: T.Text}
  deriving (Eq, Show, IsString, Semigroup, Monoid)

instance From Icon T.Text where
  from = unIcon

instance Serialize Icon

instance From T.Text Icon where
  from = Icon

instance Deserialize Icon
