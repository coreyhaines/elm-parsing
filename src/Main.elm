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


type ParsedType
    = ParsedFloat Float
    | ParsedPoint Point


type ParsedValue
    = None
    | ParsedResult (Result (List P.DeadEnd) ParsedType)


init : Model
init =
    { textToParse = "", parsedValue = None }



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = UpdateTextToParse String
    | ParseFloat
    | ParsePoint
    | ParseAny


update : Msg -> Model -> Model
update message model =
    case message of
        UpdateTextToParse text ->
            { model | textToParse = text }

        ParseFloat ->
            { model | parsedValue = parse floatParser model.textToParse }

        ParsePoint ->
            { model | parsedValue = parse pointParser model.textToParse }

        ParseAny ->
            { model | parsedValue = parse parseAny model.textToParse }


parse : Parser ParsedType -> String -> ParsedValue
parse parser textToParse =
    textToParse
        |> P.run parser
        |> ParsedResult


parseAny : Parser ParsedType
parseAny =
    P.getChompedString (P.chompUntil " ")
        |> P.andThen (\parseType -> floatParser)


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
            [ p [] [ text "Parsing Text" ]
            , textarea [ cols 50, rows 10, onInput UpdateTextToParse ] []
            , br [] []
            , div []
                [ button [ onClick ParseFloat ] [ text "Parse Float" ]
                , button [ onClick ParsePoint ] [ text "Parse Point" ]
                ]
            , br [] []
            , button [ onClick ParseAny ] [ text "Figure It Out (float 1.0 or Point (1, 5)" ]
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
