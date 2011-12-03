
module Main where

------------------------------------------------------------------------------

tokenize :: String -> [ String ]
tokenize s = [ "a", "b", "c", "d"]

------------------------------------------------------------------------------

printStrList :: [ String ] -> IO ()

printStrList (l:ls) = do
	putStrLn l
	printStrList ls

printStrList [] = do
	putStr ""

------------------------------------------------------------------------------

evalStr :: String -> IO ()
evalStr expr = do
	printStrList ( tokenize expr )
	mainLoop

------------------------------------------------------------------------------

mainLoop :: IO ()
mainLoop = do
	a <- getLine
	if a /= "q" then
		evalStr a
	else
		putStrLn "bye" --endLoop

------------------------------------------------------------------------------

main :: IO ()
main = do
	mainLoop

------------------------------------------------------------------------------

