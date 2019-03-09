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


type alias ParseResult a =
    Result (List P.DeadEnd) a


type ParsedValue
    = None
    | ParsedFloat (ParseResult Float)
    | ParsedPoint (ParseResult Point)


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


update : Msg -> Model -> Model
update message model =
    case message of
        UpdateTextToParse text ->
            { model | textToParse = text }

        ParseFloat ->
            { model | parsedValue = parseFloat model.textToParse }

        ParsePoint ->
            { model | parsedValue = parsePoint model.textToParse }


parseFloat : String -> ParsedValue
parseFloat textToParse =
    textToParse
        |> P.run P.float
        |> ParsedFloat


parsePoint : String -> ParsedValue
parsePoint textToParse =
    textToParse
        |> P.run pointParser
        |> ParsedPoint


pointParser : P.Parser Point
pointParser =
    P.succeed Point
        |. P.symbol "("
        |. P.spaces
        |= P.float
        |. P.spaces
        |. P.symbol ","
        |. P.spaces
        |= P.float
        |. P.spaces
        |. P.symbol ")"



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
            , button [ onClick ParseFloat ] [ text "Parse Float" ]
            , button [ onClick ParsePoint ] [ text "Parse Point" ]
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
