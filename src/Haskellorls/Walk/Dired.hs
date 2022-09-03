module Haskellorls.Walk.Dired
  ( NameIndeces (dired, subdired),
    empty,
    update,
  )
where

import qualified Data.ByteString as B
import Haskellorls.Formatter.Attribute

-- | Filename byte indeces for @-D / --dired@ option. These indeces are
-- 0-index.
data NameIndeces = NameIndeces
  { total :: Int,
    dired :: [(Int, Int)],
    subdired :: [(Int, Int)]
  }

empty :: NameIndeces
empty = NameIndeces 0 [] []

update :: Attribute B.ByteString -> NameIndeces -> NameIndeces
update (Name s) NameIndeces {..} = let l = B.length s in NameIndeces (total + l) ((total, total + l) : dired) subdired
update (Dir s) NameIndeces {..} = let l = B.length s in NameIndeces (total + l) dired ((total, total + l) : subdired)
update (Missing s) NameIndeces {..} = let l = B.length s in NameIndeces (total + l) dired subdired
update (Other s) NameIndeces {..} = let l = B.length s in NameIndeces (total + l) dired subdired
