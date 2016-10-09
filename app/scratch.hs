fn  = sumDigits . filter odd 
	where sumDigits [] = 0
	      sumDigits (x:xs) = x+sumDigits xs
		 

	


main = putStrLn $ show $ fn [1,2,3,4]