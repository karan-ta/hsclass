--practice_examples.hs
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

main :: IO ()
main = do
	putStrLn $ show $ getroots 4.0 5.0 1.0
	