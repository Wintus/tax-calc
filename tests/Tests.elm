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
            [ test "tax-included price of updated tax-excluded price" <|
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
            , test "tax of updated tax-excluded price" <|
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
            , test "tax-excluded price of updated tax-included price" <|
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
            , test "tax of updated tax-included price" <|
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
            , test "tax rate -> 10%" <|
                \_ ->
                    { taxRate = 0
                    , taxExcludedPrice = 1234
                    , taxIncludedPrice = 1234
                    , tax = 0
                    , truncated = True
                    }
                        |> updateTaxRate 0.1
                        |> .taxRate
                        |> Expect.within (Absolute 0.0001) 0.1
            , test "tax-included price of updated tax rate" <|
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
            , test "tax of updated tax rate" <|
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
            , test "tax-excluded price of non-truncated is same" <|
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
            , test "tax-included price of non-truncated is same" <|
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
            [ test "tax-included price of updated tax-excluded price" <|
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
            , test "tax of updated tax-excluded price" <|
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
            , test "tax-excluded price of updated tax-included price" <|
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
            , test "tax of updated tax-included price" <|
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
            , test "tax rate -> 10%" <|
                \_ ->
                    { taxRate = 0
                    , taxExcludedPrice = 1234
                    , taxIncludedPrice = 1234
                    , tax = 0
                    , truncated = False
                    }
                        |> updateTaxRate 0.1
                        |> .taxRate
                        |> Expect.within (Absolute 0.0001) 0.1
            , test "tax-included price of updated tax rate" <|
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
            , test "tax of updated tax rate" <|
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
            , test "tax-excluded price of truncated" <|
                \_ ->
                    { taxRate = 0
                    , taxExcludedPrice = 1234.56
                    , taxIncludedPrice = 1234.56
                    , tax = 0
                    , truncated = False
                    }
                        |> updateTruncation True
                        |> .taxExcludedPrice
                        |> Expect.within (Absolute 0.01) 1234
            , test "tax-included price of truncated" <|
                \_ ->
                    { taxRate = 0
                    , taxExcludedPrice = 1234.56
                    , taxIncludedPrice = 1234.56
                    , tax = 0
                    , truncated = False
                    }
                        |> updateTruncation True
                        |> .taxIncludedPrice
                        |> Expect.within (Absolute 0.01) 1234
            ]
        ]
