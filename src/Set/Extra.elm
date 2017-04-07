module Set.Extra exposing (concatMap, subset)

{-| Convenience functions for working with Set.

@docs concatMap, subset
-}

import Set exposing (Set)
import Tuple


{-| Map a given function onto a set and union the resulting set.
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


{-| Check if a Set is a subset of another Set.
    Set.Extra.subset (Set.fromList [1,2,3]) (Set.fromList [1,2,3,4,5]) -- True
-}
subset : Set comparable -> Set comparable -> Bool
subset s1 s2 =
    Set.size s1
        <= Set.size s2
        && Set.foldl (\x acc -> acc && Set.member x s2) True s1
