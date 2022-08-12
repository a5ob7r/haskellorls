module Haskellorls.Formatter.Attribute
  ( Attribute (..),
    unwrap,
  )
where

-- | A modifier to indicate what the value represents as metadata for
-- @-D / --dired@ option.
data Attribute a
  = -- | A filename.
    Name a
  | -- | A directory path on a header.
    Dir a
  | -- | Other value.
    Other a
  deriving (Functor)

-- | Return @a@ in @'Attribute' a@.
unwrap :: Attribute a -> a
unwrap (Name x) = x
unwrap (Dir x) = x
unwrap (Other x) = x
