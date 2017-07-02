module Main exposing (..)

import Dict exposing (Dict)
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
    List Single


type alias Single =
    { en : String
    , ja : String
    , type_ : String
    }


type alias Data =
    { enWords : List Word
    , jaWords : List Word
    , translations : List Translation
    , types : List Type
    }


type alias Word =
    { id : Int
    , word : String
    , type_ : Int
    }


type alias Translation =
    { enId : Int
    , jaId : Int
    }


type alias Type =
    { id : Int
    , type_ : String
    }


init : ( Model, Cmd msg )
init =
    ( [], Cmd.none )



-- UPDATE


type Msg
    = LoadedAll (Result Http.Error Data)
    | LoadAll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedAll (Ok data) ->
            ( convert data, Cmd.none )

        LoadedAll (Err _) ->
            ( model, Cmd.none )

        LoadAll ->
            ( model, loadTranslations )


convert : Data -> Model
convert data =
    let
        enWords : Dict Int Word
        enWords =
            listToDict .id data.enWords

        jaWords : Dict Int Word
        jaWords =
            listToDict .id data.jaWords

        types : Dict Int Type
        types =
            listToDict .id data.types
    in
        List.map
            (\translation -> convertSingle translation enWords jaWords types)
            data.translations


convertSingle : Translation -> Dict Int Word -> Dict Int Word -> Dict Int Type -> Single
convertSingle tr en ja ty =
    let
        en_ : Word
        en_ =
            Maybe.withDefault { id = 0, word = "", type_ = 0 } <| Dict.get tr.enId en

        ja_ : Word
        ja_ =
            Maybe.withDefault { id = 0, word = "", type_ = 0 } <| Dict.get tr.jaId ja

        ty_ : Type
        ty_ =
            Maybe.withDefault { id = 0, type_ = "" } <| Dict.get en_.type_ ty
    in
        { en = en_.word, ja = ja_.word, type_ = ty_.type_ }



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
                (\x -> row x)
                model
        ]


row : Single -> Html Msg
row single =
    div [ class "row" ]
        [ div [ class "col-4" ] [ text single.en ]
        , div [ class "col-4" ] [ text single.ja ]
        , div [ class "col-4" ] [ text single.type_ ]
        ]



-- DATA


listToDict : (a -> comparable) -> List a -> Dict.Dict comparable a
listToDict getKey values =
    Dict.fromList (List.map (\v -> ( getKey v, v )) values)


decodeWords : Decode.Decoder (List Word)
decodeWords =
    Decode.map3 Word
        (Decode.field "id" Decode.int)
        (Decode.field "word" Decode.string)
        (Decode.field "type" Decode.int)
        |> Decode.list


decodeTranslations : Decode.Decoder (List Translation)
decodeTranslations =
    Decode.map2 Translation
        (Decode.field "en_word" Decode.int)
        (Decode.field "ja_word" Decode.int)
        |> Decode.list


decodeTypes : Decode.Decoder (List Type)
decodeTypes =
    Decode.map2 Type
        (Decode.field "id" Decode.int)
        (Decode.field "type" Decode.string)
        |> Decode.list


decodeAll : Decode.Decoder Data
decodeAll =
    Decode.map4 Data
        (Decode.field "en_words" decodeWords)
        (Decode.field "ja_words" decodeWords)
        (Decode.field "translations" decodeTranslations)
        (Decode.field "types" decodeTypes)


loadTranslations : Cmd Msg
loadTranslations =
    Http.get "http://localhost:3000/db" decodeAll
        |> Http.send LoadedAll
