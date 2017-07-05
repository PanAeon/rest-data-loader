module Main where

import Lib
-- https://haskell-lang.org/library/http-client
-- https://hackage.haskell.org/package/hs-duktape
-- https://hackage.haskell.org/package/jsaddle
-- curl -s whatthecommit.com |  grep -A2 '<div id="content">' |  tail -n +2 | head -n +1 | sed 's/<p>//'

main :: IO ()
main = someFunc
