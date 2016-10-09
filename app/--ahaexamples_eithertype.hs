--ahaexamples_eithertype.hs
data DivArgs = MakeArgs {
    numerator :: Float,
    denominator ::Float
}

data Myresult a b = Lefty a | Righty b deriving Show
division :: DivArgs -> Myresult String Float
division (MakeArgs numerator denominator)
    | denominator == 0 = Lefty ("division by 0 not allowed")
    | otherwise = Righty (numerator / denominator)

main :: IO ()
main = do
	putStrLn $ show $ division (MakeArgs {numerator=2,denominator=1})
	putStrLn $ show $ (Lefty "karan" :: Myresult String Int)