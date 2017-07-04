module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, text, button, input, select, option)
import Html.Attributes exposing (id, class, name, value, selected)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { translations : List Single
    , types : List Type
    , addWord : AddWord
    }


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


type alias AddWord =
    { en : String
    , ja : String
    , type_ : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { translations = [], types = [], addWord = { en = "", ja = "", type_ = 0 } }, loadTranslations )



-- UPDATE


type Msg
    = LoadAll
    | LoadedAll (Result Http.Error Data)
    | Add
    | Added (Result Http.Error Word)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadAll ->
            ( model, loadTranslations )

        LoadedAll (Ok data) ->
            ( convert data model, Cmd.none )

        LoadedAll (Err _) ->
            ( model, Cmd.none )

        Add ->
            ( model, addTranslation model )

        Added (Ok res) ->
            ( model, Cmd.none )

        Added (Err _) ->
            ( model, Cmd.none )


convert : Data -> Model -> Model
convert data model =
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
        { translations =
            List.map
                (\translation -> convertSingle translation enWords jaWords types)
                data.translations
        , types = data.types
        , addWord = model.addWord
        }


listToDict : (a -> comparable) -> List a -> Dict.Dict comparable a
listToDict getKey values =
    Dict.fromList (List.map (\v -> ( getKey v, v )) values)


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
        , div [ class "main" ] <|
            List.map
                (\x -> row x)
                model.translations
        , div [ class "input-row" ]
            [ input [ name "en-word", value model.addWord.en ]
                []
            , input
                [ name "ja-word", value model.addWord.ja ]
                []
            , select [] <|
                List.map
                    (\x -> option [ id (toString x.id), selected (x.id == model.addWord.type_) ] [ text x.type_ ])
                    model.types
            , button [ onClick Add ] [ text "Add" ]
            ]
        ]


row : Single -> Html Msg
row single =
    div [ class "row" ]
        [ div [ class "col-4" ] [ text single.en ]
        , div [ class "col-4" ] [ text single.ja ]
        , div [ class "col-4" ] [ text single.type_ ]
        ]



-- DATA


loadTranslations : Cmd Msg
loadTranslations =
    Http.send LoadedAll <|
        Http.get "http://localhost:3000/db" decodeAll


decodeAll : Decode.Decoder Data
decodeAll =
    let
        decodeTypes =
            Decode.map2 Type
                (Decode.field "id" Decode.int)
                (Decode.field "type" Decode.string)
                |> Decode.list

        decodeTranslations =
            Decode.map2 Translation
                (Decode.field "en_word" Decode.int)
                (Decode.field "ja_word" Decode.int)
                |> Decode.list
    in
        Decode.map4 Data
            (Decode.field "en_words" decodeWords)
            (Decode.field "ja_words" decodeWords)
            (Decode.field "translations" decodeTranslations)
            (Decode.field "types" decodeTypes)


decodeWords : Decode.Decoder (List Word)
decodeWords =
    Decode.list decodeWord


decodeWord : Decode.Decoder Word
decodeWord =
    Decode.map3 Word
        (Decode.field "id" Decode.int)
        (Decode.field "word" Decode.string)
        (Decode.field "type" Decode.int)


addTranslation : Model -> Cmd Msg
addTranslation model =
    let
        body word =
            Http.jsonBody <|
                Encode.object
                    [ ( "word", Encode.string word )
                    , ( "type", Encode.int model.addWord.type_ )
                    ]

        enBody =
            body model.addWord.en

        jaBody =
            body model.addWord.ja

        req1 =
            Http.post "http://localhost:3000/en_words" enBody decodeWord

        req2 =
            Http.post "http://localhost:3000/ja_words" jaBody decodeWord

        encodeAddTranslation : AddWord -> Encode.Value
        encodeAddTranslation addWord =
            Encode.object
                [ ( "en_word", Encode.string addWord.en )
                , ( "ja_word", Encode.string addWord.ja )
                ]

        -- req3 =
        --     Http.post "http://localhost:3000/translations"
        --         (Http.jsonBody <| encodeAddWord model.addWord)
        --         decodeWord
        seq =
            [ Http.toTask req1, Http.toTask req2 ]

        chain =
            Task.sequence seq
                |> Task.andThen
    in
        Task.attempt Added chain
