
import System( getArgs )
import Debug.Trace
import IO( isEOFError )
import Data.Maybe


data CharClass =
	Numeric | Operator | OpeningParanthesis  | ClosingParanthesis | Alphabetic | Unknown
	deriving (Enum, Eq)


classify :: Char -> CharClass
classify c
	| c == '(' = OpeningParanthesis
	| c == ')' = ClosingParanthesis
	| c >= '0' && c <= '9' || c == '.' = Numeric
	| elem c "+-*/^" = Operator
	| otherwise = Unknown


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


processOper :: (CharClass, String) -> [ (CharClass, String) ] -> [ (CharClass, String) ] ->
	( [ (CharClass, String) ], [ (CharClass, String) ] )
processOper input_head queue stack@(stack_top:stack_rest) =
	let
		(tokType, stack_op) = stack_top
		(_, op) = input_head --traceShow input_head 
	in
		if tokType == Operator && ( operPrio stack_op ) > ( operPrio op ) then
			processOper input_head (queue ++ [stack_top]) stack_rest
		else (queue, [input_head]++stack) --traceShow input_head 
processOper input_head queue [] = (queue++[], [input_head])


processClosingParenthesis :: [ (CharClass, String) ] -> [ (CharClass, String) ] ->
	( [ (CharClass, String) ], [ (CharClass, String) ] )
processClosingParenthesis queue stack = 
	let (to_queue, (_:stack_rest)) = takeWhile2 (\(_,t) -> t /= "(") stack
	in ((queue++to_queue), stack_rest)


toRPN :: [ (CharClass, String) ] -> [ (CharClass, String) ] -> [ (CharClass, String) ] ->
	( [ (CharClass, String) ], [ (CharClass, String) ] )
toRPN input@(s:xs) queue stack =
	let (tokType, _) = s
	in case tokType of
		Numeric -> toRPN xs (queue ++ [s]) stack
		Operator -> let (qu, st) = processOper s queue stack
			in  toRPN xs qu st
		OpeningParanthesis -> toRPN xs queue (stack ++ [s])
		ClosingParanthesis -> let (qu, st) = processClosingParenthesis queue stack
			in  toRPN xs qu st
toRPN [] queue stack = ( queue++stack, [] )


convertToRPN :: [ (CharClass, String) ] -> [ (CharClass, String) ]
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
		Numeric -> evalRPN xs ( [ read tok :: Float ] ++ stack )
		Operator -> evalRPN xs ( operApplyOnStack stack tok )
evalRPN [] stack = stack


evaluateRPN tokens =
	let (result:stack) = evalRPN tokens []-- traceShow tokens
	in result


tokenize ::  String -> [ (CharClass, String) ]
tokenize (s:xs) =
	let
		cls = classify s
		(tok, rest) = takeWhile2 (\c -> c == ' ' || cls == (classify c)) xs
	in
		(cls, (s:tok)) : tokenize rest
tokenize [] = []


eval expr =
	putStrLn $ expr ++ " = " ++ (show.evaluateRPN.convertToRPN.tokenize) expr


readLine :: IO (Maybe String)
readLine = do
	a <- (catch getLine (\e -> return ""))
	return $ if a == "" then Nothing else Just a


mainLoop :: IO ()
mainLoop = do
	a <- readLine
	if (isNothing a) || (fromJust a) == "q" then
		putStrLn "bye"
	else do
		eval (fromJust a)
		mainLoop


main :: IO ()
main = do
	args <- getArgs
	if length args /= 0 then
		eval ( args !! 0 )
	else do
		putStrLn "type expression or 'q' to quit"
		mainLoop



