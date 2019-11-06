module Tests exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Main exposing (parseFloat, truncateFloat)
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
        , todo "1234 + 8% tax = 1332 (truncated)"
        , todo "total price - tax"
        , todo "fuzz unit price"
        ]
