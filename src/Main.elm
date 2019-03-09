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
            model

        --{ model | parsedValue = parsePoint model.textToParse }
        ParseAny ->
            model



--{ model | parsedValue = parseAny model.textToParse }
--parseAny : String -> ParsedValue
--parseAny textToParse =
--ParsedFloat (Ok 1.0)


parse : Parser ParsedType -> String -> ParsedValue
parse parser textToParse =
    textToParse
        |> P.run parser
        |> ParsedResult


floatParser : Parser ParsedType
floatParser =
    P.map ParsedFloat P.float



--parsePoint : String -> ParsedValue
--parsePoint textToParse =
--textToParse
--|> P.run (pointParser >> P.map ParsedPoint)
--pointParser : P.Parser Point
--pointParser =
--P.succeed Point
--|. P.symbol "("
--|. P.spaces
--|= P.float
--|. P.spaces
--|. P.symbol ","
--|. P.spaces
--|= P.float
--|. P.spaces
--|. P.symbol ")"
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
