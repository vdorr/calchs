
import Debug.Trace

--module Main where

numeral = 1
operator = 2
openingParenthesis = 3
closingParenthesis = 4
unknownClass = 666

--classify :: Char -> Int
classify c
	| c == '(' = 3
	| c == ')' = 4
	| c >= '0' && c <= '9' || c == '.' = 1
	| elem c "+-*/^" = 2
	| otherwise = 666

--classify c
--	| c == '(' = openingParenthesis
--	| c == ')' = closingParenthesis
--	| c >= '0' && c <= '9' || c == '.' = numeral
--	| elem c "+-*/^" = operator
--	| otherwise = unknownClass


operPrio :: String -> Int
operPrio op =
	case op of
		"+" -> 1
		"-" -> 1
		"*" -> 2
		"/" -> 2
		"^" -> 3


processOper :: (Int, String) -> [ (Int, String) ] -> [ (Int, String) ] -> ( [ (Int, String) ], [ (Int, String) ] )

--processOper input_head queue stack@(stack_top:stack_rest) | trace ("processOper " ++ show input_head) False = undefined

processOper input_head queue stack@(stack_top:stack_rest) =
	let
		(_, stack_op) = stack_top
		(_, op) = input_head --traceShow input_head 
	in
--		([input_head]++queue, [input_head]++stack)
		if ( operPrio stack_op ) < ( operPrio op ) then
			processOper input_head (queue ++ [stack_top]) stack_rest
--		else processOper input_head (queue, [input_head]++stack)
		else (queue, [input_head]++stack) --traceShow input_head 

processOper input_head queue [] = (queue++[], [input_head])

--toRPN :: [ (Int, String) ] -> [ (Int, String) ] -> [ String ] -> ( [ (Int, String) ], [ String ] )

toRPN :: [ (Int, String) ] -> [ (Int, String) ] -> [ (Int, String) ] -> ( [ (Int, String) ], [ (Int, String) ] )

toRPN input@(s:xs) queue stack =
	let (tokType, token) = s
	in case tokType of
		1 -> toRPN xs (queue ++ [s]) stack
--		2 -> toRPN xs ([s] ++ queue) stack
		2 -> let (qu, st) = processOper s queue stack
			in  toRPN xs qu st

--toRPN [] queue stack = ( queue, stack )
toRPN [] queue (stack_top:stack_rest) = ( queue++[stack_top], stack_rest )

--toRPN [] _ _ = ( [], [] )

converToRPN :: [ (Int, String) ] -> [ (Int, String) ]
converToRPN input =
	let (queue, _) = toRPN input [] []
	in queue

operApply :: String -> Float -> Float -> Float
operApply op a b =
	case op of
		"+" -> a + b
		"-" -> a - b
		"*" -> a * b
		"/" -> a / b
		"^" -> a ** b

--	operApply op ( read a :: Float ) ( read b :: Float )

operApplyOnStack :: [ Float ] -> String -> [ Float ]
operApplyOnStack stack@(a:b:stack_rest) op =
	let x = operApply op a b
	in [ x ] ++ stack_rest
operApplyOnStack [] _ = []
operApplyOnStack (x:_) _ = [x]

evalRPN tokens@(s:xs) stack =
	let (tokType, tok) = s
	in case tokType of
		1 ->  evalRPN xs [ ( read tok :: Float ) ] ++ stack
		2 -> evalRPN xs ( operApplyOnStack stack tok )

evalRPN [] stack = stack

evaluateRPN tokens =
	let (result:stack) = evalRPN tokens []
	in result

takeWhile2 _ [] = ([], [])
takeWhile2 p l@(x:xs)
	| p x =
		let (t, rest) = takeWhile2 p xs
		in (x:t, rest)
	| otherwise = ( [], l )


tokenize ::  String -> [ (Int, String) ]
tokenize (s:xs) =
	let
		cls = classify s
		(tok, rest) = takeWhile2 (\c -> c == ' ' || cls == classify c) xs
	in
		(cls, (s:tok)) : tokenize rest
tokenize [] = []


printStrList (l:ls) = do
	putStrLn l
	printStrList ls


printStrList [] = do
	putStr ""


tokenizeTst s =
	[ (show cls) ++ ":" ++ tok | (cls, tok) <- converToRPN ( tokenize s ) ]


main :: IO ()
main = do
--	let s = "3+4*20"
--	in 
	putStrLn ( "result: " ++ show ( evaluateRPN ( converToRPN ( tokenize "3+4*20" ) ) ) )
--	printStrList (tokenizeTst "3+4*20")
--"30+4*20/(1-5)^2^3.14"
--"3.1415+4*20/2.71^2^3"



