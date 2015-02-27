import Parser

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)
              
espacesP :: Parser ()
espacesP = (zeroOuPlus (car ' ')) >>= \_ -> return ()

nomP :: Parser Nom
nomP = unOuPlus (carCond (flip elem ['a'..'z'])) >>= \s -> espacesP >>= \_ -> return s

varP :: Parser Expression
varP = nomP >>= \n -> return (Var n)

applique :: [Expression] -> Expression
applique = foldl1 (\x xs -> App x xs)

exprP :: Parser Expression
exprP = exprParentheseeP ||| lambdaP ||| nombreP ||| booleenP ||| varP

exprsP :: Parser Expression
exprsP = unOuPlus exprP >>= \s -> return (applique s)

lambdaP :: Parser Expression
lambdaP = car '\\' >>= \_ ->
          espacesP >>= \_ ->
          nomP >>= \v ->
          espacesP >>= \_ ->
          car '-' >>= \_ ->
          car '>' >>= \_ ->
          espacesP >>= \_ ->
          exprsP >>= \e ->
          return (Lam v e)
          
exprParentheseeP :: Parser Expression
exprParentheseeP = car '(' >>= \_ ->
                   exprsP >>= \e ->
                   car ')' >>= \_ ->
                   return e
                   
nombreP :: Parser Expression
nombreP = unOuPlus (carCond (flip elem ['0' .. '9'])) >>= \c ->
          espacesP >>= \_ -> 
          return (Lit (Entier (read c)))
          
booleenP :: Parser Expression
booleenP = unOuPlus (carCond (' '/=))  >>= \b -> 
           espacesP >>= \_ ->
           if (b == "True")
           then return (Lit (Bool True))
           else if (b == "False")
                then return (Lit (Bool False))
                else echoue 
                         
expressionP :: Parser Expression
expressionP = espacesP >>= \_ ->
              exprsP >>= \e ->
              return e
