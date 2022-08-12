module Haskellorls.Data.Infinitable (Infinitable (..)) where

-- | A value of @'Infinitable' a@ type either represents just a value or a
-- positive infinity value. The infinity value is greater than all of just a
-- value.
--
-- This is very similar to 'Maybe', but @'Nothing'@ is greater than @'Just' a@
-- in this case.
--
-- prop> Only x < Infinity
--
-- prop> Only x `compare` Only y == x `compare` y
--
-- prop> Infinity == Infinity
data Infinitable a = Only a | Infinity
  deriving (Show, Eq, Ord, Functor)
