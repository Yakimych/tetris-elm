module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Time
import Types exposing (BoardMap, Orientation, PieceShape)



-- MAIN


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


type Model
    = NotStarted
    | Running GameState
    | Paused GameState
    | GameOver GameState


init : Model
init =
    NotStarted



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
    model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick StartNewGamePressed ] [ text "Start New Game" ]
        , div [] [ text (String.fromInt 123) ]
        , button [ onClick PausePressed ] [ text "Pause" ]
        ]
