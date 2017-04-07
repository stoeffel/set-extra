module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (int, list)
import String
import Set exposing (Set)
import Set.Extra


all : Test
all =
    describe "Set.Extra"
        [ describe "#concatMap"
            [ fuzz (list int) "Same as concatMap and from/toList" <|
                \xs ->
                    Set.fromList xs
                        |> Set.Extra.concatMap doubleSet
                        |> Expect.equal (Set.fromList <| List.concatMap doubleList xs)
            , fuzz int "left identity" <|
                \x ->
                    Set.singleton x
                        |> Set.Extra.concatMap doubleSet
                        |> Expect.equal (doubleSet x)
            , fuzz (list int) "right identity" <|
                \xs ->
                    Set.fromList xs
                        |> Set.Extra.concatMap Set.singleton
                        |> Expect.equal (Set.fromList xs)
            , fuzz (list int) "associativity" <|
                \xs ->
                    Set.fromList xs
                        |> Set.Extra.concatMap doubleSet
                        |> Set.Extra.concatMap tripleSet
                        |> Expect.equal
                            (Set.fromList xs
                                |> Set.Extra.concatMap
                                    (\x ->
                                        doubleSet x
                                            |> Set.Extra.concatMap tripleSet
                                    )
                            )
            ]
        ]


doubleList : Int -> List Int
doubleList a =
    [ a, 2 * a ]


doubleSet : Int -> Set Int
doubleSet a =
    Set.fromList [ a, 2 * a ]


tripleSet : Int -> Set Int
tripleSet a =
    Set.fromList [ a, 3 * a ]
