module Tests exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Main exposing (parseFloat, truncateFloat, updateTaxExcludedPrice, updateTaxIncludedPrice, updateTaxRate, updateTruncation)
import Test exposing (..)


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
            [ test "1234 + 8% tax = 1332" <|
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
            , test "8% tax of 1234 ~= 98.72" <|
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
            , test "1234 - 8% tax = 1142" <|
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
            , test "8% tax of total 1234 ~= 91.41" <|
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
            , test "total price : 1234 -> 10% tax" <|
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
            , test "tax : 1234 -> 10% tax" <|
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
            , test "disable truncate tax-excluded" <|
                \_ ->
                    { taxRate = 0
                    , taxExcludedPrice = 1234
                    , taxIncludedPrice = 1234
                    , tax = 0
                    , truncated = True
                    }
                        |> updateTruncation True
                        |> .taxExcludedPrice
                        |> Expect.within (Absolute 0.01) 1234
            , test "disable truncate tax-included" <|
                \_ ->
                    { taxRate = 0
                    , taxExcludedPrice = 1234
                    , taxIncludedPrice = 1234
                    , tax = 0
                    , truncated = True
                    }
                        |> updateTruncation True
                        |> .taxIncludedPrice
                        |> Expect.within (Absolute 0.01) 1234
            , todo "fuzz unit price"
            ]
        , describe "not-truncated"
            [ todo "tax-excluded"
            , todo "tax-included"
            , todo "tax rate"
            , test "truncate tax-excluded" <|
                \_ ->
                    { taxRate = 0
                    , taxExcludedPrice = 1234.56
                    , taxIncludedPrice = 1234.56
                    , tax = 0
                    , truncated = True
                    }
                        |> updateTruncation True
                        |> .taxExcludedPrice
                        |> Expect.within (Absolute 0.01) 1234
            , test "truncate tax-included" <|
                \_ ->
                    { taxRate = 0
                    , taxExcludedPrice = 1234.56
                    , taxIncludedPrice = 1234.56
                    , tax = 0
                    , truncated = True
                    }
                        |> updateTruncation True
                        |> .taxIncludedPrice
                        |> Expect.within (Absolute 0.01) 1234
            ]
        ]
