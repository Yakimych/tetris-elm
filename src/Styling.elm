module Styling exposing (boardBackgroundColor, boardBorderColor, getLandedPieceColor, getPieceColor, getTileColor, lineColor)

import Types exposing (BoardTile(..), PieceShape(..))


lineColor : String
lineColor =
    "DarkGray"


boardBackgroundColor : String
boardBackgroundColor =
    "LightGray"


boardBorderColor : String
boardBorderColor =
    "Black"


getPieceColor : PieceShape -> String
getPieceColor pieceShape =
    case pieceShape of
        T ->
            "Green"

        S ->
            "Orange"

        Z ->
            "Purple"

        I ->
            "Brown"

        O ->
            "Yellow"

        L ->
            "Blue"

        J ->
            "Pink"


getLandedPieceColor : PieceShape -> String
getLandedPieceColor pieceShape =
    case pieceShape of
        T ->
            "DarkGreen"

        S ->
            "DarkOrange"

        Z ->
            "Indigo"

        I ->
            "Maroon"

        O ->
            "Khaki"

        L ->
            "Blue"

        J ->
            "Violet"


getTileColor : BoardTile -> String
getTileColor boardTile =
    case boardTile of
        OccupiedBy piece ->
            getLandedPieceColor piece

        Boundary ->
            "Black"
