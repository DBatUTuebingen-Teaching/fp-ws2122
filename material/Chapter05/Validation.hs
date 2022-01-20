module Validation (Validation, valid, invalid) where

import Control.Applicative
import Data.Monoid

-- Validation e a represents
--
--   1. a value (of type a) that underwent a             [value]
--      series of validations/sanity checks,
--   2. a history of messages (history has type e)       [structure/context]
--      issued by these validations.

data Validation e a = Validation e a

instance (Show a, Show e, Monoid e, Eq e) => Show (Validation e a) where
  show (Validation e a) | e == mempty = "OK: "   ++ show a
                        | otherwise   = "FAIL: " ++ show e


-- A value x that passed validation without message
valid :: Monoid e => a -> Validation e a
valid x = Validation mempty x

-- A problematic value x whose validation generated message e
invalid :: Monoid e => a -> e -> Validation e a
invalid x e = Validation e x

-- Applies f to the validated value
-- (message history is context that remains unchanged)
instance Functor (Validation e) where
  fmap f (Validation e a) = Validation e (f a)

-- Appends messages when two validations are combined
instance Monoid e => Applicative (Validation e) where
  pure = valid
  (Validation e1 f) <*> (Validation e2 x) = Validation (e1 <> e2) (f x)
