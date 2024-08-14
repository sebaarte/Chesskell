#!/usr/bin/env cabal
{- cabal:
build-depends: base
            , gloss
-}
import Graphics.Gloss








myWindow = InWindow "My Window" (200, 200) (10, 10)
main = display myWindow white (Circle 80)