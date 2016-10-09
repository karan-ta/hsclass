-- record syntax
data User = User
    {
	    name::String,   --name : User -> String
	    age::Int 		-- age::User -> Int				
    }deriving Show

fooGetName::User->String
fooGetName (User name age) = name
fooGetAge::User->Int
fooGetAge (User name age) = age
main::IO ()
main = do
putStrLn $ show $ name $ User {name="karan",age=32}
putStrLn $ show $ fooGetName $ User {name="karan",age=32}
putStrLn $ show $ age $ User {name="karan",age=32}
putStrLn $ show $ fooGetAge $ User {name="karan",age=32}