module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.Ratio
import Data.Complex
import Data.Array
import Numeric
-- one of the symbols allowed `in scheme identifier
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

--readExpr :: String -> String
--readExpr input = case parse (spaces >> symbol) "lisp" input of
--    Left err-> "Not match: "++ show err
--    Right val -> "matched "
data LispVal = Nil
    |Atom String
    |List [LispVal]
    |DottedList [LispVal] LispVal
    |Number Integer
    |String String
    |Bool Bool
    |Character Char
    |Float Double
    |Ratio Rational
    |Complex (Complex Double)
    |Vector (Array Int LispVal)
instance Show LispVal where show = showVal

escapeChars :: Parser Char
escapeChars =do char '\\'
                x <- oneOf "\\\"nrt"
                return $ case x of
                    '\\' -> x
                    '"'  -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escapeChars <|> noneOf "\"\\")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (string "newline" <|> string "space")
        <|> do { x<-anyChar; notFollowedBy alphaNum; return [x]}
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        otherwise -> (value !! 0)

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal1 :: Parser LispVal
--parseNumber = do
--        digStr <- many1 digit
--        return $ (Number . read) digStr
parseDecimal1 = (many1 digit) >>= return . (Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do try $ string "#d"; x <- many1 digit; return $ (Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"; return . Number . hex2dig =<< many1 hexDigit

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b";
                x <- many1 (oneOf "10");
                return $ Number (bin2dig x)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst.head $ readFloat (x++"."++y))

parseRatio :: Parser LispVal
parseRatio = do x<- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))
toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n)= fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do
            x <- (try parseFloat <|> parseNumber)
            char '+'
            y <- (try parseFloat <|> parseNumber)
            char 'i'
            return $ Complex (toDouble x :+ toDouble y)

--parseList :: Parser LispVal
--parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseList :: Parser LispVal
parseList = between beg end parseList1
           where beg = (char '(' >> skipMany space)
                 end = (skipMany space >> char ')')
parseList1 :: Parser LispVal
parseList1 = do list <- sepEndBy parseExpr spaces
                datum <- option Nil (char '.' >> spaces >> parseExpr)
                return $ case datum of
                    Nil -> List list
                    val  -> DottedList list val

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseBackQuote :: Parser LispVal
parseBackQuote = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do arrayValues <- sepBy parseExpr spaces;return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

parseExpr :: Parser LispVal
parseExpr =  parseAtom
            <|> parseString
            <|> try parseBool
            <|> try parseComplex
            <|> try parseFloat
            <|> try parseRatio
            <|> try parseNumber
            <|> try parseCharacter
            <|> parseQuoted
            <|> parseBackQuote
            <|> parseUnQuote
            <|> try parseList
            <|> do{string "#("; x<-try parseVector; char ')'; return x}

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err->String $ "Not match: "++ show err
    Right val -> val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Character contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "("++unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal]->String
unwordsList = unwords . map showVal

eval:: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Bool _) = val
eval val@(Character _) = val
eval (List [Atom "quote", val])= val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal]-> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal]-> LispVal)]
primitives = [  ("+", numericBinop (+)),
                ("-", numericBinop (-)),
                ("*", numericBinop (*)),
                ("/", numericBinop div),
                ("mod", numericBinop mod),
                ("quotient", numericBinop quot),
                ("remainder", numericBinop rem),
                ("symbol?" , unaryOp symbolp) ,
                ("string?" , unaryOp stringp) ,
                ("number?" , unaryOp numberp) ,
                ("bool?", unaryOp boolp) ,
                ("list?" , unaryOp listp),
                ("string2symbol", unaryOp string2symbol),
                ("symbol2string", unaryOp symbol2string)]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

string2symbol, symbol2string :: LispVal -> LispVal
string2symbol (String a) = (Atom a)
string2symbol _ = Atom ""
symbol2string (Atom s)   = String s
symbol2string _          = String ""

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False

numericBinop :: (Integer -> Integer-> Integer) -> [LispVal]->LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0



main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn $ show $ eval $ readExpr expr
