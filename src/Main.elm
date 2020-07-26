module Main exposing (main)

import Browser
import Dict exposing (insert)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Time
import Types exposing (BoardMap, Orientation, PieceShape)



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
    { shape = pieceShape, orientation = Types.Up, x = 3, y = -1 }


initGameState : () -> GameState
initGameState () =
    let
        startingPieceShape =
            Types.L
    in
    { board = Dict.empty |> addBoundaries
    , currentPiece = initPiece startingPieceShape
    , nextShape = Types.T
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
                    |> Dict.insert ( -1, y ) Types.Boundary
                    |> Dict.insert ( boardWidth, y ) Types.Boundary
            )
            board


addBottomBoundary : BoardMap -> BoardMap
addBottomBoundary board =
    List.range -1 boardWidth
        |> List.foldl (\x tempBoard -> tempBoard |> Dict.insert ( x, boardHeight ) Types.Boundary) board


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
    case ( msg, model ) of
        ( StartNewGamePressed, _ ) ->
            Running (initGameState ())

        ( PausePressed, Running gameState ) ->
            Paused gameState

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


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick StartNewGamePressed ] [ text "Start New Game" ]
        , div [] [ text (String.fromInt 123) ]
        , div [] [ text <| getStatus model ]
        , button [ onClick PausePressed ] [ text "Pause" ]
        ]
