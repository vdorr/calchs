
import System( getArgs )
import Debug.Trace

numeral = 1
operator = 2
openingParenthesis = 3
closingParenthesis = 4
unknownClass = 666

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
		(_, stack_op) = stack_top
		(_, op) = input_head --traceShow input_head 
	in
		if ( operPrio stack_op ) < ( operPrio op ) then
			processOper input_head (queue ++ [stack_top]) stack_rest
		else (queue, [input_head]++stack) --traceShow input_head 
processOper input_head queue [] = (queue++[], [input_head])


toRPN :: [ (Int, String) ] -> [ (Int, String) ] -> [ (Int, String) ] -> ( [ (Int, String) ], [ (Int, String) ] )
toRPN input@(s:xs) queue stack =
	let (tokType, _) = s
	in case tokType of
		1 -> toRPN xs (queue ++ [s]) stack
		2 -> let (qu, st) = processOper s queue stack
			in  toRPN xs qu st

toRPN [] queue (stack_top:stack_rest) = ( queue++[stack_top], stack_rest )


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


operApplyOnStack :: [ Float ] -> String -> [ Float ]
operApplyOnStack stack@(a:b:stack_rest) op =
	let x = operApply op a b
	in [ x ] ++ stack_rest
operApplyOnStack [] _ = []
operApplyOnStack (x:_) _ = [x]


evalRPN tokens@(s:xs) stack =
	let (tokType, tok) = s
	in case tokType of
		1 ->  evalRPN xs ( [ read tok :: Float ] ++ stack )
		2 -> evalRPN xs ( operApplyOnStack stack tok )
evalRPN [] stack = stack


evaluateRPN tokens =
	let (result:stack) = traceShow tokens evalRPN tokens []
	in result


tokenize ::  String -> [ (Int, String) ]
tokenize (s:xs) =
	let
		cls = classify s
		(tok, rest) = takeWhile2 (\c -> c == ' ' || cls == classify c) xs
	in
		(cls, (s:tok)) : tokenize rest
tokenize [] = []


main :: IO ()
main = do
	args <- getArgs
	putStrLn ( ( args !! 0 ) ++ "= "++ show ( evaluateRPN ( converToRPN ( tokenize ( args !! 0 ) ) ) ) )
--	putStrLn ( "result: " ++ show ( evaluateRPN ( converToRPN ( tokenize s ) ) ) )

--"3+4*20"
--	printStrList (tokenizeTst "3+4*20")
--"30+4*20/(1-5)^2^3.14"
--"3.1415+4*20/2.71^2^3"



