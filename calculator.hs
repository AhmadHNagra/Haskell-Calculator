type Operator = Double -> Double -> Double
type Entry = (String, Operator)
type Register = [Entry]

operatorRegister :: Register
operatorRegister = [
                ("-", (-)),
                ("+", (+)),
                ("/", (/)),
                ("*", (*))
            ]
            
main = do
		putStrLn "Enter your desired operation (each number and operation must be followed by space)"
		x <- getLine
		print $ calculate x
            
calculate :: String -> Maybe Double
calculate = eval operatorRegister . words
            
eval :: Register -> [String] -> Maybe Double
eval [] _ = Nothing
eval _ [] = Nothing
eval _ [number] = Just $ read number
eval ((operator, function):rest) unparsed =
    case span (/=operator) unparsed of
        (_, []) -> eval rest unparsed
        (beforeOperator, afterOperator) -> 
            function
                <$> (eval operatorRegister beforeOperator)
                <*> (eval operatorRegister $ drop 1 afterOperator)