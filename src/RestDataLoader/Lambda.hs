module RestDataLoader.Lambda() where


import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse, try)
import Text.Parsec.Char (oneOf, char, digit, letter, satisfy)
import Text.Parsec.Combinator (many1, chainl1, between, eof, optionMaybe)
import Control.Applicative ((<$>), (<**>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void, ap)
import Data.Char (isLetter, isDigit)
import Data.Vector as V
import Data.Char(digitToInt)

--import Control.Applicative

data Variable = Variable Char

data Expr = Var Char  | App Expr Expr | Lambda Char Expr -- | const
            deriving (Show)



regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""


expr' :: Parser Expr
expr' = ((parens expr) <|> expr) <* eof


expr :: Parser Expr
expr =
        lambda
        <|>  (
           ((,) <$> (variable <* ws) <*> optionMaybe expr) >>= ( \foo ->
              case foo of
                 (e1, Just e2) -> return $ App e1 e2
                 (e1, _) -> return $ e1
           )
         )
        -- left factoring !! rubbish ))

{-
expr :: Parser Expr
expr =
        lambda
        <|>  variable
        <|> parens app -- left factoring !! doesn't work both have parens!
-}

app :: Parser Expr
app =  App <$> (expr <* ws') <*> (expr)

lambda :: Parser Expr
lambda =  Lambda <$> (  lambdaLit *> ws *> letter <* ws <* char '.' <* ws) <*> (expr <* ws )

lambdaLit :: Parser Char
lambdaLit = oneOf ['\\', 'λ', '+']

variable = fmap Var letter

--constant :: Parser Expr
--constant = fmap (Const . digitToInt) digit




parens :: Parser a -> Parser a
parens p = char '(' *> ws *>  p <* (char ')')


ws :: Parser ()
ws = void $ many $ oneOf " \n\t"

ws' :: Parser ()
ws' = void $ many1 $ oneOf " \n\t"


------------------- reduction -------------------------
