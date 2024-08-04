module GrammarReader (parseChessFile) where
import System.IO

type ErrorMsg = String

parseChessFile:: FilePath -> Either ErrorMsg ChessGameState
parseChessFile f = Left (error "not implemented")