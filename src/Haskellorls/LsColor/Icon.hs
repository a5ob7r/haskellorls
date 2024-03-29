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
import Data.Default.Class
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.String
import Data.Text qualified as T
import Haskellorls.Class (Dictionary (..))
import Haskellorls.LsColor.Config
import Haskellorls.NodeInfo
import Haskellorls.System.OsPath.Posix.Extra (decode, takeFileName)
import System.Environment
import Witch (From (..), via)
import Prelude hiding (lookup)

lsIcons :: IO LsIcons
lsIcons = from <$> getLSICONS

getLSICONS :: IO T.Text
getLSICONS = maybe "" T.pack <$> lookupEnv "LS_ICONS"

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
    _ -> case nodeType n of
      Nothing -> orphan
      Just NamedPipe -> pipe
      Just Socket -> socket
      Just BlockDevise -> block
      Just CharDevise -> char
      Just Orphan -> orphan
      Just Directory -> directory
      _ ->
        let filename = from . decode . takeFileName $ getNodePath n
         in Query filename `query` l <|> file

instance Dictionary Query Icon LsIcons where
  lookup q (Options {..}) = extension >>= \e -> q `lookup` e

instance Queryable Icon LsIcons

-- | Filename patterns and corresponsing icons.
newtype Icons = Icons (M.Map Query Icon)
  deriving (Semigroup, Monoid)

instance From Sources Icons where
  from = Icons . M.fromList . map (bimap Query Icon) . mapMaybe (uncurry f) . M.toList . unSources
    where
      f k v = case T.uncons k of
        Just (c, k') | c == '*' -> Just (T.toUpper k', v)
        _ -> Nothing

instance From T.Text Icons where
  from = via @Sources

instance Dictionary Query Icon Icons where
  lookup q (Icons i) = q `M.lookup` i

instance Queryable Icon Icons

-- | An icon character.
--
-- NOTE: This assumes a single character, but no validation. So users must
-- guarantee it.
newtype Icon = Icon {unIcon :: T.Text}
  deriving (Eq, Show, IsString, Semigroup, Monoid)

instance From Icon T.Text

instance From T.Text Icon
