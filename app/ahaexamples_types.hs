
type Numerator = Int
type Denominator = Int
type Result = Int

wrongargsdivision :: Numerator->Denominator->Result
wrongargsdivision a b = a `div` b

newtype NumeratorT' = NumeratorV' Float deriving Show
newtype Denominator' = Denominator' Float deriving Show
newtype Result' = Result' Float deriving Show

wrongargsdivision' :: NumeratorT' -> Denominator' -> Result'
wrongargsdivision' (NumeratorV' a) (Denominator' b) =  
	Result' (a / b)

printdivargs :: Numerator -> Denominator -> String
printdivargs a b =
		 "Numerator is " ++
		 show a ++
		 " Denominator is  " ++
		 show b

printdivargs' :: NumeratorT' -> Denominator' -> String
printdivargs' (NumeratorV' a) (Denominator' b) = 
	"Numerator is " ++
	 show a ++
	" Denominator is  " ++
	 show b	

data DivArgs = MakeArgs {numerator :: Float,denominator :: Float} deriving Show
wrongdiv'' :: DivArgs -> Float
wrongdiv'' (MakeArgs numerator denominator) =
	numerator / denominator 

printdiv'' :: DivArgs -> String
printdiv'' (MakeArgs a b) = 
	"Numerator is " ++
	 show a ++
	" Denominator is  " ++
	 show b		

main :: IO ()
main = do
	putStrLn $ show $ wrongargsdivision 1 2 -- oops mistake   		
	putStrLn $ show $ wrongargsdivision' (NumeratorV' 2) (Denominator' 1)   		
	putStrLn $ show $ wrongdiv'' (MakeArgs {numerator = 2,denominator = 1})   		
	putStrLn $ show $ printdivargs 1 2 -- oops mistake
	putStrLn $ show $ printdivargs' (NumeratorV' 2) (Denominator' 1)
	putStrLn $ show $ printdiv'' (MakeArgs {numerator = 2,denominator = 1})