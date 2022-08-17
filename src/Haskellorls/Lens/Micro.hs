module Haskellorls.Lens.Micro
  ( makeLenses',
    module Lens.Micro.Mtl,
  )
where

import Data.Char (toLower)
import Language.Haskell.TH (DecsQ, Name, mkName, nameBase)
import Lens.Micro ((&), (.~))
import Lens.Micro.Mtl
import Lens.Micro.TH (DefName (TopName), lensField, lensRules, makeLensesWith)

-- | A custom 'makeLenses' to append a suffix @L@ to each field.
makeLenses' :: Name -> DecsQ
makeLenses' =
  makeLensesWith $
    lensRules
      & lensField .~ \_ _ n ->
        case nameBase n of
          '_' : x : xs -> [TopName (mkName $ (toLower x : xs) <> "L")]
          _ -> []
