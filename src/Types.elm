module Types exposing (BoardMap, BoardTile(..), Orientation(..), PieceShape(..), getNextOrientation, getPieceSet)

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


getMatrixJ : Orientation -> PieceSet
getMatrixJ orientation =
    case orientation of
        Up ->
            Set.fromList [ ( 0, 1 ), ( 0, 2 ), ( 1, 2 ), ( 2, 2 ) ]

        Left ->
            Set.fromList [ ( 1, 1 ), ( 1, 2 ), ( 0, 3 ), ( 1, 3 ) ]

        Down ->
            Set.fromList [ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ), ( 2, 3 ) ]

        Right ->
            Set.fromList [ ( 1, 1 ), ( 2, 1 ), ( 1, 2 ), ( 1, 3 ) ]


getMatrixL : Orientation -> PieceSet
getMatrixL orientation =
    case orientation of
        Up ->
            Set.fromList [ ( 2, 1 ), ( 0, 2 ), ( 1, 2 ), ( 2, 2 ) ]

        Left ->
            Set.fromList [ ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 1, 3 ) ]

        Down ->
            Set.fromList [ ( 0, 2 ), ( 0, 3 ), ( 1, 2 ), ( 2, 2 ) ]

        Right ->
            Set.fromList [ ( 1, 1 ), ( 1, 2 ), ( 1, 3 ), ( 2, 3 ) ]


getMatrixS : Orientation -> PieceSet
getMatrixS orientation =
    case orientation of
        Up ->
            Set.fromList [ ( 1, 1 ), ( 2, 1 ), ( 0, 2 ), ( 1, 2 ) ]

        Left ->
            Set.fromList [ ( 0, 1 ), ( 0, 2 ), ( 1, 2 ), ( 1, 3 ) ]

        Down ->
            Set.fromList [ ( 1, 2 ), ( 2, 2 ), ( 0, 3 ), ( 1, 3 ) ]

        Right ->
            Set.fromList [ ( 1, 1 ), ( 1, 2 ), ( 2, 2 ), ( 2, 3 ) ]


getMatrixZ : Orientation -> PieceSet
getMatrixZ orientation =
    case orientation of
        Up ->
            Set.fromList [ ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 2 ) ]

        Left ->
            Set.fromList [ ( 1, 1 ), ( 0, 2 ), ( 1, 2 ), ( 0, 3 ) ]

        Down ->
            Set.fromList [ ( 0, 2 ), ( 1, 2 ), ( 1, 3 ), ( 2, 3 ) ]

        Right ->
            Set.fromList [ ( 2, 1 ), ( 1, 2 ), ( 2, 2 ), ( 1, 3 ) ]


getMatrixT : Orientation -> PieceSet
getMatrixT orientation =
    case orientation of
        Up ->
            Set.fromList [ ( 1, 1 ), ( 0, 2 ), ( 1, 2 ), ( 2, 2 ) ]

        Left ->
            Set.fromList [ ( 1, 1 ), ( 0, 2 ), ( 1, 2 ), ( 1, 3 ) ]

        Down ->
            Set.fromList [ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ), ( 1, 3 ) ]

        Right ->
            Set.fromList [ ( 1, 1 ), ( 1, 2 ), ( 2, 2 ), ( 1, 3 ) ]


getPieceSet : PieceShape -> Orientation -> PieceSet
getPieceSet pieceShape orientation =
    case pieceShape of
        I ->
            getMatrixI orientation

        O ->
            getMatrixO ()

        J ->
            getMatrixJ orientation

        L ->
            getMatrixL orientation

        S ->
            getMatrixS orientation

        Z ->
            getMatrixZ orientation

        T ->
            getMatrixT orientation


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
