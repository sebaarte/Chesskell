import Control.Parallel.Strategies
import System.TimeIt

nfib 0 = 0
nfib 1 = 1
nfib n = nfib (n-1) + nfib (n-2)

l = [23..35]

main =  putStr (show (map (nfib) l))
        -- parMap rseq (nfib) l
        -- parMap rpar (nfib) l
        -- map (nfib) l