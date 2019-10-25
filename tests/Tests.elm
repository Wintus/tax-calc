module Tests exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (string)
import Main exposing (parseFloat, truncateFloat)
import Test exposing (..)


suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse"
            -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    Expect.equal palindrome (String.reverse palindrome)

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            , test "parse 1.23" <|
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
        ]
