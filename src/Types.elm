module Types exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


type BoardTile
    = OccupiedBy PieceShape
    | Boundary


type alias BoardMap =
    Dict ( Int, Int ) BoardTile


type alias PieceSet =
    Set ( Int, Int )


type PieceShape
    = T
    | S
    | Z
    | I
    | O
    | L
    | J


type Orientation
    = Up
    | Left
    | Down
    | Right



-- PieceI


getMatrixI : Orientation -> PieceSet
getMatrixI orientation =
    let
        upDownMatrix =
            Set.fromList [ ( 2, 0 ), ( 2, 1 ), ( 2, 2 ), ( 2, 3 ) ]

        leftRightMatrix =
            Set.fromList [ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ), ( 3, 2 ) ]
    in
    case orientation of
        Up ->
            upDownMatrix

        Down ->
            upDownMatrix

        Left ->
            leftRightMatrix

        Right ->
            leftRightMatrix


getMatrixO : () -> PieceSet
getMatrixO () =
    Set.fromList [ ( 1, 1 ), ( 2, 1 ), ( 1, 2 ), ( 2, 2 ) ]


getPieceSet : PieceShape -> Orientation -> PieceSet
getPieceSet pieceShape orientation =
    case pieceShape of
        I ->
            getMatrixI orientation

        O ->
            getMatrixO ()

        _ ->
            -- TODO: Implement the rest of the cases
            getMatrixO ()


getNextOrientation : Orientation -> Orientation
getNextOrientation currentOrientation =
    case currentOrientation of
        Up ->
            Left

        Left ->
            Down

        Down ->
            Right

        Right ->
            Up
