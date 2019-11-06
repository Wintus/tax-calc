module Tests exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Main exposing (parseFloat, truncateFloat, updateTaxExcludedPrice, updateTaxIncludedPrice, updateTaxRate)
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
                    , priceBeforeTax = 0
                    , priceWithTax = 0
                    , tax = 0
                    , truncated = True
                    }
                        |> updateTaxIncludedPrice 1234
                        |> .priceWithTax
                        |> Expect.equal 1332
            , test "8% tax of 1234 ~= 98.72" <|
                \_ ->
                    { taxRate = 0.08
                    , priceBeforeTax = 0
                    , priceWithTax = 0
                    , tax = 0
                    , truncated = True
                    }
                        |> updateTaxIncludedPrice 1234
                        |> .tax
                        |> Expect.within (Absolute 0.01) 98.72
            , test "1234 - 8% tax = 1142" <|
                \_ ->
                    { taxRate = 0.08
                    , priceBeforeTax = 0
                    , priceWithTax = 0
                    , tax = 0
                    , truncated = True
                    }
                        |> updateTaxExcludedPrice 1234
                        |> .priceBeforeTax
                        |> Expect.equal 1142
            , test "8% tax of total 1234 ~= 91.41" <|
                \_ ->
                    { taxRate = 0.08
                    , priceBeforeTax = 0
                    , priceWithTax = 0
                    , tax = 0
                    , truncated = True
                    }
                        |> updateTaxExcludedPrice 1234
                        |> .tax
                        |> Expect.within (Absolute 0.01) 91.41
            , test "total price : 1234 -> 10% tax" <|
                \_ ->
                    { taxRate = 0
                    , priceBeforeTax = 1234
                    , priceWithTax = 1234
                    , tax = 0
                    , truncated = True
                    }
                        |> updateTaxRate 0.1
                        |> .priceWithTax
                        |> Expect.equal 1357
            , test "tax : 1234 -> 10% tax" <|
                \_ ->
                    { taxRate = 0
                    , priceBeforeTax = 1234
                    , priceWithTax = 1234
                    , tax = 0
                    , truncated = True
                    }
                        |> updateTaxRate 0.1
                        |> .tax
                        |> Expect.within (Absolute 0.01) 123.4
            , todo "fuzz unit price"
            ]
        ]
