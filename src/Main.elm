module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Dict
import Html exposing (Html, button, div, h6, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List.Extra
import Random
import Set
import Styling exposing (boardBackgroundColor, boardBorderColor, getPieceColor, getTileColor, lineColor)
import Svg exposing (line, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, viewBox, width, x, x1, x2, y, y1, y2)
import Types exposing (BoardMap, BoardTile(..), Orientation(..), PieceShape(..), getNextOrientation, getPieceSet)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toMsg (Decode.field "key" Decode.string)


toMsg : String -> Msg
toMsg string =
    case string of
        "ArrowLeft" ->
            LeftPressed

        "ArrowRight" ->
            RightPressed

        "ArrowDown" ->
            MovePieceDown

        "ArrowUp" ->
            UpPressed

        _ ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Running _ ->
            let
                keySubscription =
                    Browser.Events.onKeyDown keyDecoder

                timerSubscription =
                    Browser.Events.onAnimationFrameDelta (\deltaSinceLastTick -> Tick deltaSinceLastTick)
            in
            Sub.batch [ keySubscription, timerSubscription ]

        _ ->
            Sub.none



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


initGameState : PieceShape -> PieceShape -> GameState
initGameState startingPieceShape nextPieceShape =
    { board = Dict.empty |> addBoundaries
    , currentPiece = initPiece startingPieceShape
    , nextShape = nextPieceShape
    , millisecondsSinceLastTick = 0
    , linesCleared = 0
    }


landPiece : GameState -> GameState
landPiece gameState =
    let
        newBoard =
            getPieceSet gameState.currentPiece.shape gameState.currentPiece.orientation
                |> Set.foldl
                    (\( x, y ) tempBoard ->
                        tempBoard |> Dict.insert ( x + gameState.currentPiece.x, y + gameState.currentPiece.y ) (OccupiedBy gameState.currentPiece.shape)
                    )
                    gameState.board
    in
    { gameState | board = newBoard }


hasCollisionWith : BoardMap -> PieceState -> Bool
hasCollisionWith board piece =
    let
        pieceSet =
            getPieceSet piece.shape piece.orientation
                |> Set.map (\( x, y ) -> ( x + piece.x, y + piece.y ))

        boardSet =
            board |> Dict.keys |> Set.fromList
    in
    boardSet |> Set.intersect pieceSet |> Set.isEmpty |> not


isOccupiedByPiece : BoardTile -> Bool
isOccupiedByPiece boardTile =
    case boardTile of
        OccupiedBy _ ->
            True

        Boundary ->
            False


isBoundary : BoardTile -> Bool
isBoundary boardTile =
    case boardTile of
        OccupiedBy _ ->
            False

        Boundary ->
            True


isFull : BoardMap -> Int -> Bool
isFull board line =
    let
        tilesOnLine =
            board |> Dict.filter (\( _, y ) boardTile -> isOccupiedByPiece boardTile && y == line) |> Dict.size
    in
    tilesOnLine == boardWidth


tryFindBottomMostFullLine : BoardMap -> Maybe Int
tryFindBottomMostFullLine board =
    List.range 0 (boardHeight - 1)
        |> List.Extra.findIndices (\line -> line |> isFull board)
        |> List.Extra.last


shiftLinesAbove : Int -> ( ( Int, Int ), BoardTile ) -> ( ( Int, Int ), BoardTile )
shiftLinesAbove fromLine ( ( x, y ), boardTile ) =
    if y < fromLine && isOccupiedByPiece boardTile then
        ( ( x, y + 1 ), boardTile )

    else
        ( ( x, y ), boardTile )


removeLine : BoardMap -> Int -> BoardMap
removeLine board line =
    board
        |> Dict.filter (\( _, y ) boardTile -> y /= line || isBoundary boardTile)
        |> Dict.toList
        |> List.map (shiftLinesAbove line)
        |> Dict.fromList


clearLines : GameState -> GameState
clearLines gameState =
    gameState.board
        |> tryFindBottomMostFullLine
        |> Maybe.map
            (\lineToRemove ->
                clearLines
                    { gameState
                        | board = removeLine gameState.board lineToRemove
                        , linesCleared = gameState.linesCleared + 1
                    }
            )
        |> Maybe.withDefault gameState


movedLeft : PieceState -> PieceState
movedLeft pieceState =
    { pieceState | x = pieceState.x - 1 }


movedRight : PieceState -> PieceState
movedRight pieceState =
    { pieceState | x = pieceState.x + 1 }


getRotatedPiece : PieceState -> PieceState
getRotatedPiece pieceState =
    { pieceState | orientation = pieceState.orientation |> getNextOrientation }


setPieceIfNoCollision : GameState -> PieceState -> ( Model, Cmd Msg )
setPieceIfNoCollision gameState newPiece =
    if newPiece |> hasCollisionWith gameState.board then
        ( Running gameState, Cmd.none )

    else
        ( Running { gameState | currentPiece = newPiece }, Cmd.none )


type Model
    = NotStarted
    | Running GameState
    | Paused GameState
    | GameOver GameState


init : () -> ( Model, Cmd Msg )
init _ =
    ( NotStarted, Cmd.none )


pieceSizeOnBoard : Int
pieceSizeOnBoard =
    20


boardWidth : Int
boardWidth =
    10


boardHeight : Int
boardHeight =
    20


pieceSize : Int
pieceSize =
    4


lastLevelInterval : Int
lastLevelInterval =
    50


timerIntervalsPerLevel : Array Int
timerIntervalsPerLevel =
    [ 500, 400, 300, 200, 100, 75, lastLevelInterval ] |> Array.fromList


getCurrentLevel : GameState -> Int
getCurrentLevel gameState =
    gameState.linesCleared // 10


getCurrentTimerInterval : GameState -> Int
getCurrentTimerInterval gameState =
    timerIntervalsPerLevel
        |> Array.get (getCurrentLevel gameState)
        |> Maybe.withDefault lastLevelInterval


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
    = Tick Float
    | NoOp
    | UpPressed
    | MovePieceDown
    | RightPressed
    | LeftPressed
    | SpawnNextPiece PieceShape
    | PausePressed
    | ResumePressed
    | StartNewGamePressed
    | StartNewGame ( PieceShape, PieceShape )


movePieceDown : GameState -> ( Model, Cmd Msg )
movePieceDown gameState =
    let
        currentPiece =
            gameState.currentPiece

        newPiece =
            { currentPiece | y = gameState.currentPiece.y + 1 }
    in
    if newPiece |> hasCollisionWith gameState.board then
        ( Running (gameState |> landPiece |> clearLines), Random.generate SpawnNextPiece randomPieceShape )

    else
        ( Running { gameState | currentPiece = newPiece }, Cmd.none )


randomPieceShape : Random.Generator PieceShape
randomPieceShape =
    Random.uniform T [ S, Z, I, O, L, J ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Running gameState, Tick deltaSinceLastTick ) ->
            let
                newMillisecondsSinceLastTick =
                    gameState.millisecondsSinceLastTick + (deltaSinceLastTick |> ceiling)

                timerIntervalForCurrentLevel =
                    getCurrentTimerInterval gameState
            in
            if newMillisecondsSinceLastTick >= timerIntervalForCurrentLevel then
                movePieceDown { gameState | millisecondsSinceLastTick = newMillisecondsSinceLastTick - timerIntervalForCurrentLevel }

            else
                ( Running { gameState | millisecondsSinceLastTick = newMillisecondsSinceLastTick }, Cmd.none )

        ( Running gameState, MovePieceDown ) ->
            movePieceDown gameState

        ( Running gameState, UpPressed ) ->
            setPieceIfNoCollision gameState (gameState.currentPiece |> getRotatedPiece)

        ( Running gameState, LeftPressed ) ->
            setPieceIfNoCollision gameState (gameState.currentPiece |> movedLeft)

        ( Running gameState, RightPressed ) ->
            setPieceIfNoCollision gameState (gameState.currentPiece |> movedRight)

        ( Running gameState, SpawnNextPiece nextPieceShape ) ->
            let
                newPiece =
                    initPiece gameState.nextShape
            in
            if newPiece |> hasCollisionWith gameState.board then
                ( GameOver gameState, Cmd.none )

            else
                ( Running { gameState | currentPiece = newPiece, nextShape = nextPieceShape }, Cmd.none )

        ( Running gameState, PausePressed ) ->
            ( Paused gameState, Cmd.none )

        ( Paused gameState, ResumePressed ) ->
            ( Running gameState, Cmd.none )

        ( _, StartNewGamePressed ) ->
            ( model, Random.generate StartNewGame (Random.pair randomPieceShape randomPieceShape) )

        ( _, StartNewGame ( startingPieceShape, nextPieceShape ) ) ->
            ( Running (initGameState startingPieceShape nextPieceShape), Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


getMetricsText : GameState -> String
getMetricsText gameState =
    "Current level: "
        ++ ((getCurrentLevel gameState + 1) |> String.fromInt)
        ++ ". Lines cleared: "
        ++ (gameState.linesCleared |> String.fromInt)


getStatus : Model -> String
getStatus model =
    case model of
        NotStarted ->
            "Not Started"

        Running gameState ->
            "Running. " ++ (gameState |> getMetricsText)

        Paused gameState ->
            "Paused: " ++ (gameState |> getMetricsText)

        GameOver gameState ->
            "Game Over! " ++ (gameState |> getMetricsText)


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
