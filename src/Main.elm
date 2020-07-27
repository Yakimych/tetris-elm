module Main exposing (main)

import Browser
import Dict exposing (insert)
import Html exposing (Html, button, div, h6, text)
import Html.Events exposing (onClick)
import Set
import Styling exposing (boardBackgroundColor, boardBorderColor, getPieceColor, getTileColor, lineColor)
import Svg exposing (line, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, viewBox, width, x, x1, x2, y, y1, y2)
import Time
import Types exposing (BoardMap, BoardTile(..), Orientation(..), PieceShape(..), getPieceSet)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias PieceState =
    { shape : PieceShape
    , orientation : Orientation
    , x : Int
    , y : Int
    }


type alias GameState =
    { board : BoardMap
    , currentPiece : PieceState
    , nextShape : PieceShape
    , millisecondsSinceLastTick : Int
    , linesCleared : Int
    }


initPiece : PieceShape -> PieceState
initPiece pieceShape =
    { shape = pieceShape, orientation = Up, x = 3, y = -2 }


initGameState : () -> GameState
initGameState () =
    let
        startingPieceShape =
            L
    in
    { board = Dict.empty |> addBoundaries
    , currentPiece = initPiece startingPieceShape
    , nextShape = T
    , millisecondsSinceLastTick = 0
    , linesCleared = 0
    }


type Model
    = NotStarted
    | Running GameState
    | Paused GameState
    | GameOver GameState


init : Model
init =
    NotStarted


pieceSizeOnBoard : Int
pieceSizeOnBoard =
    20


tickResolutionInMs : Int
tickResolutionInMs =
    10


boardWidth : Int
boardWidth =
    10


boardHeight : Int
boardHeight =
    20


pieceSize : Int
pieceSize =
    4


addSideBoundaries : BoardMap -> BoardMap
addSideBoundaries board =
    List.range -1 boardHeight
        |> List.foldl
            (\y tempBoard ->
                tempBoard
                    |> Dict.insert ( -1, y ) Boundary
                    |> Dict.insert ( boardWidth, y ) Boundary
            )
            board


addBottomBoundary : BoardMap -> BoardMap
addBottomBoundary board =
    List.range -1 boardWidth
        |> List.foldl (\x tempBoard -> tempBoard |> Dict.insert ( x, boardHeight ) Boundary) board


addBoundaries : BoardMap -> BoardMap
addBoundaries =
    addSideBoundaries >> addBottomBoundary



-- UPDATE


type Msg
    = Tick Time.Posix
    | UpPressed
    | MovePieceDown
    | DownPressed
    | RightPressed
    | LeftPressed
    | SpawnNextPiece PieceShape
    | PausePressed
    | ResumePressed
    | StartNewGamePressed
    | StartNewGame ( PieceShape, PieceShape )


update : Msg -> Model -> Model
update msg model =
    case ( model, msg ) of
        ( _, StartNewGamePressed ) ->
            Running (initGameState ())

        ( Running gameState, PausePressed ) ->
            Paused gameState

        ( Paused gameState, ResumePressed ) ->
            Running gameState

        _ ->
            model



-- VIEW


getStatus : Model -> String
getStatus model =
    case model of
        NotStarted ->
            "Not Started"

        Running _ ->
            "Running"

        Paused _ ->
            "Paused"

        GameOver _ ->
            "Game Over"


canvasWidth : String
canvasWidth =
    pieceSizeOnBoard * boardWidth |> String.fromInt


canvasHeight : String
canvasHeight =
    pieceSizeOnBoard * boardHeight |> String.fromInt


nextPieceCanvasSize : String
nextPieceCanvasSize =
    pieceSizeOnBoard * pieceSize |> String.fromInt


drawBackground : () -> List (Svg.Svg Msg)
drawBackground () =
    let
        boardRect =
            rect
                [ width canvasWidth
                , height canvasHeight
                , x "0"
                , y "0"
                , fill boardBackgroundColor
                , stroke boardBorderColor
                , strokeWidth "1"
                ]
                []

        verticalLines =
            List.range 1 (boardWidth - 1)
                |> List.map
                    (\col ->
                        line
                            [ col * pieceSizeOnBoard |> String.fromInt |> x1
                            , y1 "0"
                            , col * pieceSizeOnBoard |> String.fromInt |> x2
                            , y2 canvasHeight
                            , stroke lineColor
                            , strokeWidth "1"
                            ]
                            []
                    )

        horizontalLines =
            List.range 1 (boardHeight - 1)
                |> List.map
                    (\row ->
                        line
                            [ x1 "0"
                            , row * pieceSizeOnBoard |> String.fromInt |> y1
                            , x2 canvasWidth
                            , row * pieceSizeOnBoard |> String.fromInt |> y2
                            , stroke lineColor
                            , strokeWidth "1"
                            ]
                            []
                    )
    in
    boardRect :: verticalLines ++ horizontalLines


drawCell : Int -> Int -> String -> Svg.Svg Msg
drawCell atX atY color =
    rect
        [ width (pieceSizeOnBoard |> String.fromInt)
        , height (pieceSizeOnBoard |> String.fromInt)
        , x (atX * pieceSizeOnBoard |> String.fromInt)
        , y (atY * pieceSizeOnBoard |> String.fromInt)
        , fill color
        , stroke "Black"
        , strokeWidth "1"
        ]
        []


drawPiece : Int -> Int -> PieceShape -> Orientation -> List (Svg.Svg Msg)
drawPiece atX atY pieceShape pieceOrientation =
    getPieceSet pieceShape pieceOrientation
        |> Set.toList
        |> List.map (\( x, y ) -> drawCell (atX + x) (atY + y) (getPieceColor pieceShape))


drawPieceState : PieceState -> List (Svg.Svg Msg)
drawPieceState pieceState =
    drawPiece pieceState.x pieceState.y pieceState.shape pieceState.orientation


drawNextPieceState : PieceShape -> List (Svg.Svg Msg)
drawNextPieceState pieceShape =
    let
        nextPieceCanvasRect =
            rect
                [ width nextPieceCanvasSize
                , height nextPieceCanvasSize
                , x "0"
                , y "0"
                , fill boardBackgroundColor
                , stroke boardBorderColor
                , strokeWidth "1"
                ]
                []

        pieceTiles =
            drawPiece 0 0 pieceShape Up
    in
    nextPieceCanvasRect :: pieceTiles


drawBoard : BoardMap -> List (Svg.Svg Msg)
drawBoard board =
    board
        |> Dict.map (\( x, y ) boardTile -> drawCell x y (getTileColor boardTile))
        |> Dict.toList
        |> List.map (\( _, rect ) -> rect)


view : Model -> Html Msg
view model =
    let
        tilesOnMainCanvas =
            case model of
                NotStarted ->
                    []

                Paused gameState ->
                    drawBoard gameState.board ++ drawPieceState gameState.currentPiece

                GameOver gameState ->
                    drawBoard gameState.board ++ drawPieceState gameState.currentPiece

                Running gameState ->
                    drawBoard gameState.board ++ drawPieceState gameState.currentPiece

        tilesOnNextPieceCanvas =
            case model of
                NotStarted ->
                    []

                Paused gameState ->
                    drawNextPieceState gameState.nextShape

                GameOver gameState ->
                    drawNextPieceState gameState.nextShape

                Running gameState ->
                    drawNextPieceState gameState.nextShape
    in
    div []
        [ button [ onClick StartNewGamePressed ] [ text "Start New Game" ]
        , div [] [ text <| getStatus model ]
        , button [ onClick PausePressed ] [ text "Pause" ]
        , button [ onClick ResumePressed ] [ text "Resume" ]
        , h6 [] [ text <| getStatus model ]
        , svg
            [ width canvasWidth
            , height canvasHeight
            , viewBox (String.concat [ "0 0 ", canvasWidth, " ", canvasHeight ])
            ]
            (drawBackground () ++ tilesOnMainCanvas)
        , svg
            [ width nextPieceCanvasSize
            , height nextPieceCanvasSize
            , viewBox (String.concat [ "0 0 ", nextPieceCanvasSize, " ", nextPieceCanvasSize ])
            ]
            tilesOnNextPieceCanvas
        ]
