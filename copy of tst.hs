
module Main where



numeral = 1
operator = 2
openingParenthesis = 3
closingParenthesis = 4
unknownClass = 666

classify :: Char -> Int
--classify c =
--	case c of
--		'(' -> openingParenthesis
--		')' -> closingParenthesis
--		'0'..'9' -> numeral
--		_ -> unknownClass

classify c
	| c == '(' = openingParenthesis
	| c == ')' = closingParenthesis
	| c >= '0' && c <= '9' || c == '.' = numeral
	| elem c "+-*/" = operator
	| otherwise = unknownClass


takeWhile2 _ [] = ([], [])
takeWhile2 p l@(x:xs)
	| p x =
		let (t, rest) = takeWhile2 p xs
		in (x:t, rest)
	| otherwise = ( [], x:l )

tokenize ::  String -> [ String ]
tokenize (s:xs) =
	let
		cls = classify s
		(tok, rest) = takeWhile2 (\c -> cls == classify c) xs
	in
		( s : tok ) : tokenize rest
tokenize [] = []

--tokenize ::  String -> String
--tokenize (s:xs) =
--	let
--		cls= classify s
--		(a, b) = takeWhile2 (\c -> cls == classify c) xs
--	in b
----	in s : takeWhile (\c -> cls == classify c) xs
----	[s] ++ takeWhile (\c -> ((classify s) == (classify c)) ) xs

--tokenize ::  String -> String
--tokenize (s:xs) =
--	let (cls, x) = ((classify s), 666)
--	in s : takeWhile (\c -> cls == classify c) xs
----	[s] ++ takeWhile (\c -> ((classify s) == (classify c)) ) xs


printStrList (l:ls) = do
	putStrLn l
	printStrList ls

printStrList [] = do
	putStr ""

main :: IO ()
main = do
--	print ( classify '/' )
--	print ((tokenize "12.3abcd") !! 1)
	printStrList (tokenize "12.3abcd+-*/")



