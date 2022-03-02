module Haskellorls.Link.Type
  ( LinkCount (..),
  )
where

import qualified System.Posix.Types as Types

newtype LinkCount = LinkCount {unLinkCount :: Types.LinkCount}
