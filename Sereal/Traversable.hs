module Sereal.Traversable (
    Traversable,
    lookup,
    keys
) where

import Sereal.Types
import Prelude hiding (lookup)

class Traversable a where
    lookup :: SerealHashKey -> a -> Maybe SerealBody
    keys :: a -> [SerealHashKey]
    
