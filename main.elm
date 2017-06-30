module Main exposing (..)

import Html exposing (Html, div, a, text, button)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    List Word


type alias Word =
    { id : Int
    , word : String
    , type_ : String
    }


init : ( Model, Cmd msg )
init =
    ( [], Cmd.none )



-- UPDATE


type Msg
    = LoadedAll (Result Http.Error Model)
    | LoadAll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedAll (Ok list) ->
            ( list, Cmd.none )

        LoadedAll (Err _) ->
            ( model, Cmd.none )

        LoadAll ->
            ( model, loadTranslations )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ div [ class "logo" ] []
            , div [ class "account-settings" ] []
            ]
        , button [ onClick LoadAll ] [ text "Load All" ]
        , div [ class "main" ] <|
            List.map
                (\x -> text x.word)
                model
        ]



-- DATA


decodeEnWord : Decode.Decoder EnWord
decodeEnWord =
    Decode.map3 EnWord
        (Decode.field "id" Decode.int)
        (Decode.field "word" Decode.string)
        (Decode.field "type" Decode.string)


decodeEnWords : Decode.Decoder Model
decodeEnWords =
    Decode.list decodeEnWord


loadTranslations : Cmd Msg
loadTranslations =
    Http.get "http://localhost:3000/en_words" decodeEnWords
        |> Http.send LoadedAll
