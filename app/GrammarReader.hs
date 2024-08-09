module GrammarReader (parseChessFile) where
import System.IO

import ChessGameData

-- un-implemented class that will be used to parse .chess files in part 2 of the project

type ErrorMsg = String

parseChessFile:: FilePath -> Either ErrorMsg ChessGameState
parseChessFile f = error "Not Implemented"