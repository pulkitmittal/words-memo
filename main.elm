module Main exposing (..)

import Html exposing (Html, div, a, text)
import Html.Attributes exposing (id, class)


main : Html msg
main =
    div [ class "header" ]
        [ div [ class "logo" ] []
        , div [ class "account-settings" ] []
        ]
