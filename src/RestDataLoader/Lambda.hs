module RestDataLoader.Lambda() where


import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse, try)
import Text.Parsec.Char (oneOf, char, digit, letter, satisfy)
import Text.Parsec.Combinator (many1, chainl1, between, eof, optionMaybe)
import Control.Applicative ((<$>), (<**>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void, ap)
import Data.Char (isLetter, isDigit)
import qualified Data.Vector as V
import Data.Char(digitToInt)
import Data.List(delete, union, find)

--import Control.Applicative

data Variable = Variable Char

data Expr = Var Char  | App Expr Expr | Lambda Char Expr -- | const
            deriving (Show)



parseOrError :: String -> Expr
parseOrError s =  either (\x -> error (show x)) id (regularParse expr' s)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""


expr' :: Parser Expr
expr' =  apply

pexpr' :: Parser Expr
pexpr' = parens expr'



apply :: Parser Expr
apply =
   ((,) <$> (expr <* ws) <*> optionMaybe expr) >>= ( \foo ->
      case foo of
         (e1, Just e2) -> return $ App e1 e2
         (e1, _) -> return $ e1
   )


expr :: Parser Expr
expr =
        lambda
        <|> variable
        <|> pexpr'
        -- left factoring !! rubbish ))

{-
expr :: Parser Expr
expr =
        lambda
        <|>  variable
        <|> parens app -- left factoring !! doesn't work both have parens!
-}

app :: Parser Expr
app =  App <$> (expr' <* ws') <*> (expr')

lambda :: Parser Expr
lambda =  Lambda <$> (  lambdaLit *> ws *> letter <* ws <* char '.' <* ws) <*> (expr' <* ws )

lambdaLit :: Parser Char
lambdaLit = oneOf ['\\', 'λ', '+']

variable :: Parser Expr
variable = fmap Var letter

--constant :: Parser Expr
--constant = fmap (Const . digitToInt) digit




parens :: Parser a -> Parser a
parens p = char '(' *> ws *>  p <* ws <* (char ')')


ws :: Parser ()
ws = void $ many $ oneOf " \n\t"

ws' :: Parser ()
ws' = void $ many1 $ oneOf " \n\t"


------------------- reduction -------------------------

-- FIXME: set
freeV :: Expr -> [Char]
freeV (Var x) = [x]
freeV (Lambda x t) = delete x $ freeV t
freeV (App e1 e2) = union (freeV e1) (freeV e2)


alpha:: Char -> Char -> Expr -> Expr
alpha x z (Var y) | x == y    = Var z
                  | otherwise =  Var y
alpha x z (App a b) = App (alpha x z a) (alpha x z b)
alpha x z (Lambda y f) | x == y = Lambda y f -- ???
                       | otherwise = if elem x (freeV f)
                                     then
                                       error "x in FV (f)"
                                     else
                                      Lambda y (alpha x z f)

subst :: Char -> Expr -> Expr -> Expr
subst x e' v@(Var y) | x == y    = e'
                     | otherwise = v
subst x e' (App a b)  = App (subst x e' a) (subst x e' b)
subst x e' l@(Lambda y f) | x == y = l
                          | otherwise = if elem y (freeV e')
                                        then
                                          error "y in FV (f)" -- FIXME: replace
                                        else
                                          Lambda y (subst x e' f)


needsAlpha :: Expr -> Expr -> Bool
needsAlpha e' (Lambda y _) = elem y (freeV e')
needsAlpha _ _             = False

fixFreeVars :: Char -> Expr -> (Char, Expr)
fixFreeVars x e = (s, alpha x s e)
   where
     symbols = ['a'..'z']
     fv      = freeV e
     s       = maybe (error "not enough vars!") id $ find (\z -> not $ elem z fv) symbols


beta :: Expr -> Expr
beta (App l@(Lambda v e) e') = if needsAlpha e' l then
                                  let (v', efixed) = fixFreeVars v e
                                  in subst v' e' efixed
                               else
                                 subst v e' e -- FIXME: do alpha if needed
beta x = x

-------------------- write interpreter --------------------------------
