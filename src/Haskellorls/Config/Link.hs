module Haskellorls.Config.Link (LinkCount (..)) where

import qualified System.Posix.Types as Types

newtype LinkCount = LinkCount {unLinkCount :: Types.LinkCount}
