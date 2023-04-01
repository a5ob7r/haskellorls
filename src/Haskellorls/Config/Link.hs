module Haskellorls.Config.Link (LinkCount (..)) where

import System.Posix.Types qualified as Types

newtype LinkCount = LinkCount {unLinkCount :: Types.LinkCount}
