--ahaexamples_monad.hs
--functor.hs
import Control.Applicative
import Control.Monad.Writer
half :: Int -> Maybe Int
half x = if even x then Just (x `div` 2) else Nothing

myhalf :: Int -> Writer String Int
myhalf x = do 
		tell ("I halved "++ (show x) ++ "!")
		return (x `div` 2)
main :: IO ()
main = do
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