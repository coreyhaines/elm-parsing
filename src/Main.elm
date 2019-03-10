module Main exposing (Model, Msg(..))

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Parser as P exposing ((|.), (|=), Parser)



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { textToParse : String
    , parsedValue : ParsedValue
    }


type alias Point =
    { x : Float
    , y : Float
    }



-- ---------------------------
-- PARSING
-- ---------------------------


type ParsedType
    = ParsedFloat Float
    | ParsedPoint Point


type ParsedValue
    = None
    | ParsedResult (Result (List P.DeadEnd) ParsedType)


parse : Parser ParsedType -> String -> ParsedValue
parse parser textToParse =
    textToParse
        |> P.run parser
        |> ParsedResult


chooseParser : String -> Parser ParsedType
chooseParser parserType =
    case parserType of
        "float" ->
            floatParser

        "point" ->
            pointParser

        _ ->
            P.problem "Unsupported identifier"


parseAny : Parser ParsedType
parseAny =
    P.getChompedString (P.chompUntil " ")
        |> P.andThen chooseParser


floatParser : Parser ParsedType
floatParser =
    P.map ParsedFloat <|
        (P.succeed identity
            |. P.spaces
            |= P.float
        )


pointParser : P.Parser ParsedType
pointParser =
    P.map ParsedPoint <|
        (P.succeed Point
            |. P.spaces
            |. P.symbol "("
            |. P.spaces
            |= P.float
            |. P.spaces
            |. P.symbol ","
            |. P.spaces
            |= P.float
            |. P.spaces
            |. P.symbol ")"
        )


init : Model
init =
    { textToParse = "", parsedValue = None }



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = UpdateTextToParse String
    | ParseText


update : Msg -> Model -> Model
update message model =
    case message of
        UpdateTextToParse text ->
            { model | textToParse = text }

        ParseText ->
            { model | parsedValue = parse parseAny model.textToParse }



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ header []
            [ -- img [ src "/images/logo.png" ] []
              span [ class "logo" ] []
            , h1 [] [ text "How do Parsers work?" ]
            ]
        , div []
            [ p [] [ text "Supported Identifiers: float point" ]
            , textarea [ cols 50, rows 10, onInput UpdateTextToParse ] []
            , br [] []
            , br [] []
            , button [ onClick ParseText ] [ text "Parse" ]
            ]
        , div []
            [ p [] [ text "Model" ]
            , div [] [ text <| Debug.toString model ]
            ]
        ]



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
