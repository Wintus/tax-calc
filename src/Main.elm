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
    , taxExcludedPrice : Float
    , taxIncludedPrice : Float
    , tax : Float
    , truncated : Bool
    }


init : Model
init =
    { taxRate = 0.08
    , taxExcludedPrice = 0
    , taxIncludedPrice = 0
    , tax = 0
    , truncated = True
    }



---- UPDATE ----


type Msg
    = ChangeTaxRate String
    | ChangeTaxExcludedPrice String
    | ChangeTaxIncludedPrice String
    | ChangeTruncated Bool


update : Msg -> Model -> Model
update msg model =
    let
        float =
            truncation model
    in
    case msg of
        ChangeTaxRate s ->
            let
                -- no truncate
                rate =
                    s |> parseFloat
            in
            updateTaxRate rate model

        ChangeTaxExcludedPrice s ->
            let
                price =
                    s |> parseFloat |> float
            in
            updateTaxExcludedPrice price model

        ChangeTaxIncludedPrice s ->
            let
                price =
                    s |> parseFloat |> float
            in
            updateTaxIncludedPrice price model

        ChangeTruncated truncated ->
            updateTruncation truncated model


parseFloat s =
    s |> String.toFloat |> Maybe.withDefault 0


truncateFloat =
    truncate >> toFloat


truncation { truncated } =
    if truncated then
        truncateFloat

    else
        identity


updateTaxExcludedPrice : Float -> Model -> Model
updateTaxExcludedPrice price model =
    let
        float =
            truncation model

        tax =
            price * model.taxRate
    in
    { model
        | taxExcludedPrice = price
        , taxIncludedPrice = price + tax |> float
        , tax = tax
    }


updateTaxIncludedPrice : Float -> Model -> Model
updateTaxIncludedPrice price model =
    let
        float =
            truncation model

        rate =
            model.taxRate

        tax =
            price * rate / (1 + rate)
    in
    { model
        | taxIncludedPrice = price
        , taxExcludedPrice = price - tax |> float
        , tax = tax
    }


updateTaxRate : Float -> Model -> Model
updateTaxRate taxRate model =
    let
        float =
            truncation model

        price =
            model.taxExcludedPrice

        tax =
            price * taxRate
    in
    { model
        | taxRate = taxRate
        , tax = tax
        , taxIncludedPrice = price + tax |> float
    }


updateTruncation : Bool -> Model -> Model
updateTruncation truncated model =
    let
        float =
            if truncated then
                truncateFloat

            else
                identity
    in
    { model
        | truncated = truncated
        , taxIncludedPrice = model.taxIncludedPrice |> float
        , taxExcludedPrice = model.taxExcludedPrice |> float
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
                        , step (String.fromFloat 0.01)
                        , onInput ChangeTaxRate
                        ]
                    , labeledNumberInput
                        "price-before-tax"
                        "税抜価格"
                        [ value <| String.fromFloat model.taxExcludedPrice
                        , onInput ChangeTaxExcludedPrice
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
                        [ value <| String.fromFloat model.taxIncludedPrice
                        , onInput ChangeTaxIncludedPrice
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
