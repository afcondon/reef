-- | Odonus's two inputs and its locks, on their own so that a patch can carry
-- | its locks (Reef.Odonus.Patch) without importing Reef.Route, which imports
-- | Reef.Move, which imports the patch. Reef.Route re-exports all of this; the
-- | rig keeps calling `reef_route@ps`, whose `parseLocks` and `printLocks` are
-- | these.
module Reef.Route.Locks
  ( Input(..)
  , Locks
  , parseLocks
  , printLocks
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.String (Pattern(..), joinWith, split, trim)
import Data.Traversable (traverse)

data Input = OdonusGrid | OdonusOut

derive instance Eq Input

-- | **Odonus's locks** (docs/kb/plans/routing-in-tidal.md, AC 2026-10-27: "a
-- | concept of locking the in-app quantisation or yielding to takeover"): the
-- | inputs that keep Odonus's own scale whatever is routed to them. A locked
-- | input's route stays in the table (it is still sent, and drawn held off)
-- | but is not followed. Odonus's own state, kept on the stage as
-- | `odonus/locks`, and in a patch (`locks: grid`): the inputs' short names,
-- | `grid out`, `grid`, or nothing.
type Locks = Array Input

parseLocks :: String -> Either String Locks
parseLocks t = map (Array.nubEq <<< inOrder) (traverse one (Array.filter (_ /= "") (split (Pattern " ") (trim t))))
  where
  one = case _ of
    "grid" -> Right OdonusGrid
    "out" -> Right OdonusOut
    "output" -> Right OdonusOut
    other -> Left ("no input '" <> other <> "' to lock (grid, out)")

printLocks :: Locks -> String
printLocks ls = joinWith " " (map short (inOrder (Array.nubEq ls)))
  where
  short = case _ of
    OdonusGrid -> "grid"
    OdonusOut -> "out"

-- | Grid first, as the pickers stand.
inOrder :: Locks -> Locks
inOrder = Array.sortWith (\i -> if i == OdonusGrid then 0 else 1)
