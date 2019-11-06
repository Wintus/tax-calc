module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.sandbox
        { view = view
        , init = init
        , update = update
        }



---- MODEL ----


type alias Model =
    { taxRate : Float
    , priceBeforeTax : Float
    , priceWithTax : Float
    , tax : Float
    , truncated : Bool
    }


init : Model
init =
    { taxRate = 0.08
    , priceBeforeTax = 0
    , priceWithTax = 0
    , tax = 0
    , truncated = True
    }



---- UPDATE ----


type Msg
    = ChangePriceBeforeTax String
    | ChangePriceWithTax String
    | ChangeTruncated Bool


update : Msg -> Model -> Model
update msg model =
    let
        float =
            if model.truncated then
                truncateFloat

            else
                identity
    in
    case msg of
        ChangePriceBeforeTax s ->
            let
                price =
                    s |> parseFloat |> float

                tax =
                    price * model.taxRate
            in
            { model
                | priceBeforeTax = price
                , priceWithTax = price + tax |> float
                , tax = tax
            }

        ChangePriceWithTax s ->
            let
                price =
                    s |> parseFloat |> float

                rate =
                    model.taxRate

                tax =
                    price * rate / (1 + rate)
            in
            { model
                | priceWithTax = price
                , priceBeforeTax = price - tax |> float
                , tax = tax
            }

        ChangeTruncated flag ->
            { model
                | truncated = flag
                , priceWithTax = model.priceWithTax |> float
                , priceBeforeTax = model.priceBeforeTax |> float
            }


parseFloat s =
    s |> String.toFloat |> Maybe.withDefault 0


truncateFloat =
    truncate >> toFloat


updateTaxIncludedPrice : Model -> Model
updateTaxIncludedPrice model =
    let
        float =
            if model.truncated then
                truncateFloat

            else
                identity

        price =
            model.priceBeforeTax

        tax =
            price * model.taxRate
    in
    { model
        | priceWithTax = price + tax |> float
        , tax = tax
    }



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        labeledNumberInput =
            labeledInput numberInput

        labeledCheckbox =
            labeledInput checkbox
    in
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Tax Calculator" ]
        , div [ class "flex-center" ]
            [ div [ class "grid" ] <|
                List.concat
                    [ labeledNumberInput
                        "tax-rate"
                        "税率"
                        [ value <| String.fromFloat model.taxRate
                        , disabled True
                        ]
                    , labeledNumberInput
                        "price-before-tax"
                        "税抜価格"
                        [ value <| String.fromFloat model.priceBeforeTax
                        , onInput ChangePriceBeforeTax
                        ]
                    , labeledNumberInput
                        "tax"
                        "税額"
                        [ value <| String.fromFloat model.tax
                        , disabled True
                        ]
                    , labeledNumberInput
                        "price-with-tax"
                        "税込価格"
                        [ value <| String.fromFloat model.priceWithTax
                        , onInput ChangePriceWithTax
                        ]
                    , labeledCheckbox
                        "truncate"
                        "切り捨て"
                        [ checked model.truncated
                        , onCheck ChangeTruncated
                        ]
                    ]
            ]
        ]


type alias Input msg =
    List (Attribute msg) -> Html msg


labeledInput :
    Input msg
    -> String
    -> String
    -> List (Attribute msg)
    -> List (Html msg)
labeledInput input_ id_ labelText inputAttrs =
    [ label [ for id_ ] [ text labelText ]
    , input_ <| id id_ :: inputAttrs
    ]


typedInput : String -> Input msg
typedInput typeName attrs =
    input (type_ typeName :: attrs) []


numberInput : Input msg
numberInput =
    typedInput "number"


checkbox : Input msg
checkbox =
    typedInput "checkbox"
