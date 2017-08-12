module RestDataLoader.Lambda(interpreter) where


import Text.Parsec (ParseError)
import Text.Parsec.Token(lexeme)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse, try, (<?>))
import Text.Parsec.Char (oneOf, char, digit, letter, satisfy)
import Text.Parsec.Combinator (many1, chainl1, between, eof, optionMaybe,sepBy, notFollowedBy, anyToken)
import Control.Applicative ((<$>), (<**>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void, ap)
import Data.Char (isLetter, isDigit)
import qualified Data.Vector as V
import Data.Char(digitToInt)
import Data.List(delete, union, find)
import Debug.Trace(trace, traceShow, traceShowId)
--import Control.Applicative

data Variable = Variable Char

data Expr = Var Char  | App Expr Expr | Lambda Char Expr -- | const
            deriving (Show, Eq)



parseOrError :: String -> Expr
parseOrError s =  either (\x -> error (show x)) id (regularParse expr' s)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""



expr'' :: Parser Expr
expr'' = expr' <* ws <* eof

expr' :: Parser Expr
expr' =  apply

pexpr' :: Parser Expr
pexpr' = parens expr'



explodeApp :: [Expr] -> Expr
explodeApp = foldl1 (\a -> \r -> App a r)

apply :: Parser Expr
apply =
   ((,) <$> (expr <* ws) <*> (expr `sepBy` ws) ) >>= ( \foo ->
      case foo of
         (e1, []) -> return $ e1
         (e1, xs) -> return $ (explodeApp $ e1:xs)
   )


expr :: Parser Expr
expr =
        lambda
        <|> variable
        <|> (parens expr')
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
parens p =  char '(' *> ws *>  p <* ws <*  char ')'


--parens' = between (char '(') (char ')')
-- parens' :: Parser a -> Parser a
-- parens' p = do
--     void $ char '('
--     e <- p
--     void $ char ')'
--     return e




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
                       | otherwise = if elem z (freeV f)
                                     then
                                       error "x in FV (f)"
                                     else
                                      Lambda y (alpha x z f)

subst :: Char -> Expr -> Expr -> Expr
subst c e2 v@(Var y) | c == y    = e2
                     | otherwise = v
subst c e2 (App a b)  = App (subst c e2 a) (subst c e2 b)
subst c e2 l@(Lambda y f) | c == y = l
                          | otherwise = if elem c (freeV e2)
                                        then
                                          error $ "error in subst: '" ++ [c] ++ "' in FW " ++ (show e2)
                                        else
                                          Lambda y (subst c e2 f)


needsAlpha :: Expr -> Expr -> Bool
needsAlpha e2 (Lambda y _) = elem (y) (freeV e2)
needsAlpha _ _             = False

fixFreeVars :: Expr -> Expr -> (Char, Expr)
fixFreeVars e2 (Lambda x e1) = (s, alpha x s e1)
   where
     symbols = ['a'..'z']
     fv      = freeV e2
     s       = maybe (error "not enough vars!") id $ find (\z -> not $ elem z fv) symbols


isLambda :: Expr -> Bool
isLambda (Lambda _ _) = True
isLambda _ = False

-- http://www.cs.cornell.edu/courses/cs6110/2014sp/Handouts/Sestoft.pdf

-- same as subst, but will invoke alpha if needed

subst' :: Expr -> Expr -> Expr
subst' l@(Lambda v e1) e2 = if traceShowId $ needsAlpha e2 l then
                              let (v', e1') = fixFreeVars e2 l
                              in subst v' e2 e1'
                            else
                               subst v e2 e1

-- call-by-name, computes weak head normal form
cbn :: Expr -> Expr
cbn v@(Var _) = v
cbn l@(Lambda _ _) = l
cbn (App e1 e2) = case cbn e1 of
                    l@(Lambda x e) -> cbn $ subst' l e2
                    e1'          -> App e1' e2


-- normal order reduction
beta :: Expr -> Expr
beta (App e1 e2) = case cbn e1 of
                    l@(Lambda x e) -> beta $ subst' l e2
                    e1'            -> let e1'' = beta e1'
                                      in App e1'' (beta e2)

beta v@(Var _) = v
beta (Lambda v e) = Lambda v $ beta e


------------ FIXME: beta, consecutive apply ----------------------------


pprint :: Expr -> String
pprint (Var a) = [a]
pprint (App a b) =   pprint a ++ " " ++ "(" ++ pprint b ++")"
pprint (Lambda a b) = "\\" ++ [a] ++ "." ++ pprint b

test0 = "\\f.\\x.f ((\\n.\\f.\\x.f (n f x)) (\\f.\\x.x) f x)"
test0Expr = Lambda 'f' (Lambda 'x' (App (Var 'f') (App (App (App (Lambda 'n' (Lambda 'f' (Lambda 'x' (App (Var 'f') (App (App (Var 'n') (Var 'f')) (Var 'x')))))) (Lambda 'f' (Lambda 'x' (Var 'x')))) (Var 'f')) (Var 'x'))))

test1 = "(\\a.\\b.a (b (\\n.\\f.\\x.f (n f x))) (\\f.\\x.x)) (\\f.\\x.f (f (f x))) (\\f.\\x.f (f x))"
test1E = App (App (Lambda 'a' (Lambda 'b' (App (App (Var 'a') (App (Var 'b') (Lambda 'n' (Lambda 'f' (Lambda 'x' (App (Var 'f') (App (App (Var 'n') (Var 'f')) (Var 'x')))))))) (Lambda 'f' (Lambda 'x' (Var 'x')))))) (Lambda 'f' (Lambda 'x' (App (Var 'f') (App (Var 'f') (App (Var 'f') (Var 'x'))))))) (Lambda 'f' (Lambda 'x' (App (Var 'f') (App (Var 'f') (Var 'x')))))
-------------------- write interpreter --------------------------------
-- "(((\\x y.(y x))(((((\\x y. (y x))(((\\x.x) 12)))) (\\x.x))))(\\x.x))"
-- "((\\f.\\x.f (f (f x))) ((\\f.\\x.f (f x)) (\\n.\\f.\\x.f ((n f) x)))) (\\f.\\x.x)"
performReduction :: Expr -> [Expr]
performReduction e = if e' == e
                     then [e]
                     else e': performReduction e'
     where
       e' = beta e

interpreter :: IO ()
interpreter = do
               input <- getLine
               let res = case regularParse expr' input of
                          (Left err) ->   "fail. " ++ (show err)
                          (Right expr) -> "ok"
               putStrLn res
