import Control.Applicative
import Control.Monad.Writer
--practice_examples.hs
biggestInt,smallestInt :: Int
biggestInt = maxBound
smallestInt= minBound
n :: Integer
n = 2 ^ 2
i :: Int
i = 2 
-- (/) works only with Floats.use (/)
--badArith = i / i

-- (+) works only with 2 values of the same type and haskell doesnot do implicit conversion
--badArith = i + n

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)

hailstone :: Integer -> Integer
hailstone n 
	| n `mod` 2 == 0 = n `div` 2
	| otherwise = 3*n + 1

-- Use of maybe , pairs , operators
getroots :: Float -> Float -> Float -> Maybe (Float,Float)
getroots a b c = 
	if bsquare < 0 
	then Nothing else Just (root1,root2)
	where
		bsquare = (b * b) - (4 * a * c)
		bsquareroot = sqrt bsquare
		root1 = (- b + bsquareroot) / (2*a)
		root2 = (negate b - bsquareroot) / (2*a)

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
wrongdiv'' (MakeArgs {numerator=numerator,denominator=denominator}) =
	numerator / denominator 



printdiv'' :: DivArgs -> String
printdiv'' (MakeArgs {numerator=a,denominator=b}) = 
	"Numerator is " ++
	 show a ++
	" Denominator is  " ++
	 show b		

half :: Int -> Maybe Int
half x = if even x then Just (x `div` 2) else Nothing

myhalf :: Int -> Writer String Int
myhalf x = do 
		tell ("I halved "++ (show x) ++ "!")
		return (x `div` 2)


main :: IO ()
main = do
	-- putStrLn "Hello World" 
	-- putStrLn $ show $ biggestInt
	-- putStrLn $ show $ smallestInt
	putStrLn $ show $ sumtorial 3
	putStrLn $ show $ hailstone 3
	putStrLn $ show $ getroots 4.0 5.0 1.0
	putStrLn $ show $ wrongargsdivision 1 2 -- oops mistake   		
	putStrLn $ show $ wrongargsdivision' (NumeratorV' 2) (Denominator' 1)   		
	putStrLn $ show $ wrongdiv'' (MakeArgs {numerator = 2,denominator = 1})   		
	putStrLn $ show $ printdivargs 1 2 -- oops mistake
	putStrLn $ show $ printdivargs' (NumeratorV' 2) (Denominator' 1)
	putStrLn $ show $ printdiv'' (MakeArgs {numerator = 2,denominator = 1})
	putStrLn $ show $ (+1) <$> (+2) $ 10
	putStrLn $ show $ Just (+2) <*> Just 3
	putStrLn $ show $ [(*2),(+3)] <*> [1,2,3,4]
	putStrLn $ show $ (+1) <$> Just 2 
--putStrLn $ show $ Just (+1) <$> Just 2
	putStrLn $ show $ Just (+1) <*> Just 2
	putStrLn $ show $ liftA2 (*) (Just 5) (Just 3)
--(*) <$> Just 5 <*> Just 3
	putStrLn $ show $ Just 10 >>= half
	putStrLn $ show $ runWriter $ myhalf 8 >>= myhalf >>= myhalf