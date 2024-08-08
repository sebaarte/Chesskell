module GrammarReader (parseChessFile) where
import System.IO

import ChessGameData


type ErrorMsg = String

parseChessFile:: FilePath -> Either ErrorMsg ChessGameState
parseChessFile f = error "Not Implemented"