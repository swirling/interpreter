module Main where
import System.Environment
--main is of type IO ():unit type () :allows only one value
-- type declaration is optional
main :: IO ()
main = do
--    num1 <- getLine
--    num2 <- getLine
--    putStrLn $ show ( read num2 + read num1)
    putStrLn $ "give a name"
    name <- getLine
    putStrLn $ "name is " ++ name
