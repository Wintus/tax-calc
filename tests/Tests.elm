module Tests exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Main exposing (parseFloat, truncateFloat, updateTaxExcludedPrice, updateTaxIncludedPrice)
import Test exposing (..)


suite : Test
suite =
    describe "The Main module"
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
        , test "1234 + 8% tax = 1332 (truncated)" <|
            \_ ->
                { taxRate = 0.08
                , priceBeforeTax = 1234
                , priceWithTax = 0
                , tax = 0
                , truncated = True
                }
                    |> updateTaxIncludedPrice
                    |> .priceWithTax
                    |> Expect.equal 1332
        , test "8% tax of 1234 ~= 98.72" <|
            \_ ->
                { taxRate = 0.08
                , priceBeforeTax = 1234
                , priceWithTax = 0
                , tax = 0
                , truncated = True
                }
                    |> updateTaxIncludedPrice
                    |> .tax
                    |> Expect.within (Absolute 0.01) 98.72
        , test "1234 - 8% tax = 1142 (truncated)" <|
            \_ ->
                { taxRate = 0.08
                , priceBeforeTax = 0
                , priceWithTax = 1234
                , tax = 0
                , truncated = True
                }
                    |> updateTaxExcludedPrice
                    |> .priceBeforeTax
                    |> Expect.equal 1142
        , test "8% tax of total 1234 ~= 91.41" <|
            \_ ->
                { taxRate = 0.08
                , priceBeforeTax = 0
                , priceWithTax = 1234
                , tax = 0
                , truncated = True
                }
                    |> updateTaxExcludedPrice
                    |> .tax
                    |> Expect.within (Absolute 0.01) 91.41
        , todo "fuzz unit price"
        ]
