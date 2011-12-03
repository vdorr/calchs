
import System( getArgs )
import Debug.Trace

numeral :: Int
numeral = 1
operator :: Int
operator = 2
openingParenthesis = 3
closingParenthesis = 4
unknownClass = 666

data CharClass =
	Numeric | Operator | OpeningParanthesis  | ClosingParanthesis | Alphabetic deriving (Enum)

classify :: Char -> Int
classify c
	| c == '(' = openingParenthesis
	| c == ')' = closingParenthesis
	| c >= '0' && c <= '9' || c == '.' = numeral
	| elem c "+-*/^" = operator
	| otherwise = unknownClass


operPrio :: String -> Int
operPrio op =
	case op of
		"+" -> 1
		"-" -> 1
		"*" -> 2
		"/" -> 2
		"^" -> 3


takeWhile2 _ [] = ([], [])
takeWhile2 p l@(x:xs)
	| p x =
		let (t, rest) = takeWhile2 p xs
		in (x:t, rest)
	| otherwise = ( [], l )


processOper :: (Int, String) -> [ (Int, String) ] -> [ (Int, String) ] -> ( [ (Int, String) ], [ (Int, String) ] )
processOper input_head queue stack@(stack_top:stack_rest) =
	let
		(tokType, stack_op) = stack_top
		(_, op) = input_head --traceShow input_head 
	in
		if tokType == 2 && ( operPrio stack_op ) > ( operPrio op ) then
			processOper input_head (queue ++ [stack_top]) stack_rest
		else (queue, [input_head]++stack) --traceShow input_head 
processOper input_head queue [] = (queue++[], [input_head])


processClosingParenthesis :: [ (Int, String) ] -> [ (Int, String) ] -> ( [ (Int, String) ], [ (Int, String) ] )
processClosingParenthesis queue stack = 
	let (to_queue, (_:stack_rest)) = takeWhile2 (\(_,t) -> t /= "(") stack
	in ((queue++to_queue), stack_rest)


toRPN :: [ (Int, String) ] -> [ (Int, String) ] -> [ (Int, String) ] -> ( [ (Int, String) ], [ (Int, String) ] )
toRPN input@(s:xs) queue stack =
	let (tokType, _) = s
	in case tokType of
		1 -> toRPN xs (queue ++ [s]) stack
		2 -> let (qu, st) = processOper s queue stack
			in  toRPN xs qu st
		3 -> toRPN xs queue (stack ++ [s])
		4 -> let (qu, st) = processClosingParenthesis queue stack
			in  toRPN xs qu st

toRPN [] queue stack = ( queue++stack, [] )


convertToRPN :: [ (Int, String) ] -> [ (Int, String) ]
convertToRPN input =
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


operApplyOnStack :: [ Float ] -> String -> [ Float ]
operApplyOnStack (b:a:stack_rest) op =
	[ operApply op a b ] ++ stack_rest
operApplyOnStack [] _ = []
operApplyOnStack (x:_) _ = [x]


evalRPN tokens@(s:xs) stack =
	let (tokType, tok) = s
	in case tokType of
		1 ->  evalRPN xs ( [ read tok :: Float ] ++ stack )
		2 -> evalRPN xs ( operApplyOnStack stack tok )
evalRPN [] stack = stack


evaluateRPN tokens =
	let (result:stack) = evalRPN tokens []-- traceShow tokens
	in result


tokenize ::  String -> [ (Int, String) ]
tokenize (s:xs) =
	let
		cls = classify s
		(tok, rest) = takeWhile2 (\c -> c == ' ' || cls == classify c) xs
	in
		(cls, (s:tok)) : tokenize rest
tokenize [] = []


eval expr =
	putStrLn $ expr ++ " = " ++ (show.evaluateRPN.convertToRPN.tokenize) expr


mainLoop :: IO ()
mainLoop = do
	a <- getLine
	if a /= "q" then do
		eval a
		mainLoop
	else
		putStrLn "bye"


main :: IO ()
main = do
	args <- getArgs
	if length args /= 0 then
		eval ( args !! 0 )
	else do
		putStrLn "type expression or 'q' to quit"
		mainLoop



