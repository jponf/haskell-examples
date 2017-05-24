--
-- Some times it may be handy to access the raw command line arguments
--

import System.Environment

main = do
    args <- getArgs
    printArgs args

-- 'public' printArgs function
printArgs lst = printArgs' lst 1

-- 'private' printArgs that passes the index to the next call
printArgs' (x:[]) idx = do
    putStr (show idx)
    putStr " -> "
    putStrLn x
printArgs' (x:xs) idx = do
    putStr (show idx)
    putStr " -> "
    putStrLn x
    printArgs' xs (idx + 1)
