module Main where

import Lib
-- https://haskell-lang.org/library/http-client
-- https://hackage.haskell.org/package/hs-duktape
-- https://hackage.haskell.org/package/jsaddle
-- curl -s whatthecommit.com |  grep -A2 '<div id="content">' |  tail -n +2 | head -n +1 | sed 's/<p>//'
-- xinput --set-prop "FTE1001:00 0B05:0101" 'Device Enabled' $(xinput --list-props 'FTE1001:00 0B05:0101' | grep -c 'Device Enabled (139):[[:space:]].*0')

main :: IO ()
main = someFunc
