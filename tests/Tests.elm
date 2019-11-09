module Tests exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (float, floatRange, map)
import Main exposing (parseFloat, truncateFloat, updateTaxExcludedPrice, updateTaxIncludedPrice, updateTaxRate, updateTruncation)
import Test exposing (..)


rounded =
    floatRange 0 1.0e9
        |> map truncateFloat


suite : Test
suite =
    describe "The Main module"
        [ describe "float truncation"
            [ test "parse 1.23" <|
                \_ ->
                    "1.23"
                        |> parseFloat
                        |> Expect.within (Absolute 0.0001) 1.23
            , test "truncate 1.23 to 1.0" <|
                \_ ->
                    1.23
                        |> truncateFloat
                        |> Expect.within (Absolute 0.0001) 1.0
            ]
        , describe "truncated price"
            [ describe "update tax-excluded price"
                [ test "tax-included price" <|
                    \_ ->
                        { taxRate = 0.08
                        , taxExcludedPrice = 0
                        , taxIncludedPrice = 0
                        , tax = 0
                        , truncated = True
                        }
                            |> updateTaxExcludedPrice 1234
                            |> .taxIncludedPrice
                            |> Expect.equal 1332
                , test "tax" <|
                    \_ ->
                        { taxRate = 0.08
                        , taxExcludedPrice = 0
                        , taxIncludedPrice = 0
                        , tax = 0
                        , truncated = True
                        }
                            |> updateTaxExcludedPrice 1234
                            |> .tax
                            |> Expect.within (Absolute 0.01) 98.72
                ]
            , describe "update tax-included price"
                [ test "tax-excluded price" <|
                    \_ ->
                        { taxRate = 0.08
                        , taxExcludedPrice = 0
                        , taxIncludedPrice = 0
                        , tax = 0
                        , truncated = True
                        }
                            |> updateTaxIncludedPrice 1234
                            |> .taxExcludedPrice
                            |> Expect.equal 1142
                , test "tax" <|
                    \_ ->
                        { taxRate = 0.08
                        , taxExcludedPrice = 0
                        , taxIncludedPrice = 0
                        , tax = 0
                        , truncated = True
                        }
                            |> updateTaxIncludedPrice 1234
                            |> .tax
                            |> Expect.within (Absolute 0.01) 91.41
                ]
            , describe "update tax rate"
                [ test "tax-excluded price is same" <|
                    \_ ->
                        { taxRate = 0
                        , taxExcludedPrice = 1234
                        , taxIncludedPrice = 1234
                        , tax = 0
                        , truncated = True
                        }
                            |> updateTaxRate 0.1
                            |> .taxExcludedPrice
                            |> Expect.within (Absolute 0.1) 1234
                , test "tax-included price" <|
                    \_ ->
                        { taxRate = 0
                        , taxExcludedPrice = 1234
                        , taxIncludedPrice = 1234
                        , tax = 0
                        , truncated = True
                        }
                            |> updateTaxRate 0.1
                            |> .taxIncludedPrice
                            |> Expect.equal 1357
                , test "tax" <|
                    \_ ->
                        { taxRate = 0
                        , taxExcludedPrice = 1234
                        , taxIncludedPrice = 1234
                        , tax = 0
                        , truncated = True
                        }
                            |> updateTaxRate 0.1
                            |> .tax
                            |> Expect.within (Absolute 0.01) 123.4
                ]
            , describe "un-truncate"
                [ test "tax-excluded price is same" <|
                    \_ ->
                        { taxRate = 0
                        , taxExcludedPrice = 1234
                        , taxIncludedPrice = 1234
                        , tax = 0
                        , truncated = True
                        }
                            |> updateTruncation False
                            |> .taxExcludedPrice
                            |> Expect.within (Absolute 0.01) 1234
                , test "tax-included price is same" <|
                    \_ ->
                        { taxRate = 0
                        , taxExcludedPrice = 1234
                        , taxIncludedPrice = 1234
                        , tax = 0
                        , truncated = True
                        }
                            |> updateTruncation False
                            |> .taxIncludedPrice
                            |> Expect.within (Absolute 0.01) 1234
                ]
            , fuzz rounded "update unit price -> total price -> unit price is diff in 1" <|
                \price ->
                    let
                        model =
                            { taxRate = 0.08
                            , taxExcludedPrice = 0
                            , taxIncludedPrice = 0
                            , tax = 0
                            , truncated = True
                            }
                                |> updateTaxExcludedPrice price

                        unitPrice =
                            model.taxExcludedPrice
                    in
                    model
                        |> updateTaxIncludedPrice model.taxIncludedPrice
                        |> .taxExcludedPrice
                        |> Expect.within (Absolute 1) unitPrice
            , fuzz float "truncate twice is idempotent" <|
                \n ->
                    let
                        f =
                            truncateFloat n

                        model =
                            { taxRate = 0
                            , taxExcludedPrice = f
                            , taxIncludedPrice = f
                            , tax = 0
                            , truncated = True
                            }
                    in
                    model
                        |> updateTruncation False
                        |> updateTruncation True
                        |> Expect.equal model
            ]
        , describe "not-truncated"
            [ describe "update tax-excluded price"
                [ test "tax-included price" <|
                    \_ ->
                        { taxRate = 0.08
                        , taxExcludedPrice = 0
                        , taxIncludedPrice = 0
                        , tax = 0
                        , truncated = False
                        }
                            |> updateTaxExcludedPrice 1234
                            |> .taxIncludedPrice
                            |> Expect.within (Absolute 0.01) 1332.72
                , test "tax" <|
                    \_ ->
                        { taxRate = 0.08
                        , taxExcludedPrice = 0
                        , taxIncludedPrice = 0
                        , tax = 0
                        , truncated = False
                        }
                            |> updateTaxExcludedPrice 1234
                            |> .tax
                            |> Expect.within (Absolute 0.01) 98.72
                ]
            , describe "update tax-included price"
                [ test "tax-excluded price" <|
                    \_ ->
                        { taxRate = 0.08
                        , taxExcludedPrice = 0
                        , taxIncludedPrice = 0
                        , tax = 0
                        , truncated = False
                        }
                            |> updateTaxIncludedPrice 1234
                            |> .taxExcludedPrice
                            |> Expect.within (Absolute 0.01) 1142.6
                , test "tax" <|
                    \_ ->
                        { taxRate = 0.08
                        , taxExcludedPrice = 0
                        , taxIncludedPrice = 0
                        , tax = 0
                        , truncated = False
                        }
                            |> updateTaxIncludedPrice 1234
                            |> .tax
                            |> Expect.within (Absolute 0.01) 91.41
                ]
            , describe "update tax rate"
                [ test "tax-excluded price is same" <|
                    \_ ->
                        { taxRate = 0
                        , taxExcludedPrice = 1234
                        , taxIncludedPrice = 1234
                        , tax = 0
                        , truncated = False
                        }
                            |> updateTaxRate 0.1
                            |> .taxExcludedPrice
                            |> Expect.within (Absolute 0.1) 1234
                , test "tax-included price" <|
                    \_ ->
                        { taxRate = 0
                        , taxExcludedPrice = 1234
                        , taxIncludedPrice = 1234
                        , tax = 0
                        , truncated = False
                        }
                            |> updateTaxRate 0.1
                            |> .taxIncludedPrice
                            |> Expect.within (Absolute 0.01) 1357.4
                , test "tax" <|
                    \_ ->
                        { taxRate = 0
                        , taxExcludedPrice = 1234
                        , taxIncludedPrice = 1234
                        , tax = 0
                        , truncated = False
                        }
                            |> updateTaxRate 0.1
                            |> .tax
                            |> Expect.within (Absolute 0.01) 123.4
                ]
            , describe "un-truncate"
                [ test "tax-excluded price is same" <|
                    \_ ->
                        { taxRate = 0
                        , taxExcludedPrice = 1234
                        , taxIncludedPrice = 1234
                        , tax = 0
                        , truncated = False
                        }
                            |> updateTruncation False
                            |> .taxExcludedPrice
                            |> Expect.within (Absolute 0.01) 1234
                , test "tax-included price is same" <|
                    \_ ->
                        { taxRate = 0
                        , taxExcludedPrice = 1234
                        , taxIncludedPrice = 1234
                        , tax = 0
                        , truncated = False
                        }
                            |> updateTruncation False
                            |> .taxIncludedPrice
                            |> Expect.within (Absolute 0.01) 1234
                ]
            , fuzz rounded "update unit price -> total price -> unit price is diff in 1" <|
                \price ->
                    let
                        model =
                            { taxRate = 0.08
                            , taxExcludedPrice = 0
                            , taxIncludedPrice = 0
                            , tax = 0
                            , truncated = False
                            }
                                |> updateTaxExcludedPrice price

                        unitPrice =
                            model.taxExcludedPrice
                    in
                    model
                        |> updateTaxIncludedPrice model.taxIncludedPrice
                        |> .taxExcludedPrice
                        |> Expect.within (Absolute 1) unitPrice
            ]
        ]
