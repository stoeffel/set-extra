module Set.Extra exposing (concatMap)

{-| Convenience functions for working with Set.

@docs concatMap
-}

import Set exposing (Set)
import Tuple


{-| Map a given function onto a set and flatten the resulting set.
    neighbors :: (Int, Int) -> Set (Int, Int)
    neighbors (x, y) =
        Set.fromList
          [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
          , (x - 1, y),                 (x + 1, y)
          , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
          ]
    Set.Extra.concatMap neighbors setOfPoints
-}
concatMap : (comparable -> Set comparable2) -> Set comparable -> Set comparable2
concatMap f s =
    Set.foldl (Set.union << f) Set.empty s
