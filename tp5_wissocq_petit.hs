import Parser
import Data.Maybe

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)

--Parser              
espacesP :: Parser ()
espacesP = (zeroOuPlus (car ' ')) >>= \_ ->
           return ()

espacesP' :: Parser ()
espacesP' = do zeroOuPlus (car ' ')
               return ()

nomP :: Parser Nom
nomP = unOuPlus (carCond (flip elem ['a'..'z'])) >>= \s ->
       espacesP >>= \_ -> return s

nomP' :: Parser Nom
nomP' = do s <- unOuPlus (carCond (flip elem ['a'..'z']))
           espacesP' 
           return s

varP :: Parser Expression
varP = nomP >>= \n -> 
       return (Var n)

varP' :: Parser Expression
varP' = do n <- nomP'  
           return (Var n)

applique :: [Expression] -> Expression
applique = foldl1 (\x xs -> App x xs)

exprP :: Parser Expression
exprP = exprParentheseeP ||| lambdaP ||| nombreP ||| booleenP ||| varP

exprP' :: Parser Expression
exprP' = exprParentheseeP' ||| lambdaP' ||| nombreP' ||| booleenP' ||| varP'

exprsP :: Parser Expression
exprsP = unOuPlus exprP >>= \s -> 
         return (applique s)   

exprsP' :: Parser Expression
exprsP' = do s <- unOuPlus exprP'
             return (applique s)   

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

lambdaP' :: Parser Expression
lambdaP' = do car '\\'
              espacesP' 
              v <- nomP'
              espacesP'
              car '-'
              car '>'
              espacesP' 
              e <- exprsP'
              return (Lam v e)
          
exprParentheseeP :: Parser Expression
exprParentheseeP = car '(' >>= \_ ->
                   espacesP >>= \_ ->
                   exprsP >>= \e ->
                   car ')' >>= \_ ->
	  	   espacesP >>= \_ ->
                   return e

exprParentheseeP' :: Parser Expression
exprParentheseeP' = do car '('
                       espacesP'
                       e <- exprsP'
                       car ')'
		       espacesP'
                       return e    

nombreP :: Parser Expression
nombreP = unOuPlus (carCond (flip elem ['0' .. '9'])) >>= \c ->
          espacesP >>= \_ -> 
          return (Lit (Entier (read c)))

nombreP' :: Parser Expression
nombreP' = do c <- unOuPlus (carCond (flip elem ['0' .. '9']))
              espacesP'
              return (Lit (Entier (read c)))
          
booleenP :: Parser Expression
booleenP = unOuPlus (carCond (' '/=))  >>= \b -> 
           espacesP >>= \_ ->
           if (b == "True")
           then return (Lit (Bool True))
           else if (b == "False")
                then return (Lit (Bool False))
                else echoue 

booleenP' :: Parser Expression
booleenP' = do b  <- unOuPlus (carCond (' '/=))
               espacesP'
               if (b == "True")
               then return (Lit (Bool True))
               else if (b == "False")
                    then return (Lit (Bool False))
                    else echoue 

expressionP :: Parser Expression
expressionP = espacesP >>= \_ ->
              exprsP >>= \e ->
              return e

expressionP' :: Parser Expression
expressionP' = do espacesP'
                  e <- exprsP'
                  return e

ras :: String -> Expression
ras s = if ((isNothing res) || (snd (fromJust res) /= ""))
	then error "Erreur d'analyse syntaxique"
	else fst (fromJust res)
	where res = parse expressionP s 

--Interprète

data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA)

instance Show ValeurA where
    show (VFonctionA _) = "λ"
                       -- ^ ou "VFonctionA _", ou "<fun>" ou toute
                       --   autre représentation des fonctions
    show (VLitteralA (Entier i)) = show i
    show (VLitteralA (Bool b)) = show b

type Environnement a = [(Nom, a)]


interpreteA :: Environnement ValeurA -> Expression -> ValeurA
interpreteA _ (Lit x)          = VLitteralA x
interpreteA env (Var x)        = fromJust (lookup x env) 
interpreteA env (Lam nom expr) = VFonctionA (\v -> interpreteA ((nom,v):env) expr)
interpreteA env (App e1 e2)    = f v2
                                where VFonctionA f = interpreteA env e1
                                      v2 = interpreteA env e2
                                      
negA :: ValeurA
negA = VFonctionA (\(VLitteralA (Entier x)) -> (VLitteralA (Entier (-x))))

addA :: ValeurA
addA = VFonctionA (\(VLitteralA (Entier x)) -> (VFonctionA (\(VLitteralA (Entier y)) -> (VLitteralA (Entier (x+y))))))
 interpreteA envA (ras "if True 1")
releveBinOpEntierA :: (Integer -> Integer -> Integer) -> ValeurA
releveBinOpEntierA = \f -> (VFonctionA (\(VLitteralA (Entier x)) -> (VFonctionA (\(VLitteralA (Entier y)) -> (VLitteralA (Entier (x `f` y)))))))

envA :: Environnement ValeurA
envA = [ ("neg",   negA)
       , ("add",   releveBinOpEntierA (+))
       , ("soust", releveBinOpEntierA (-))
       , ("mult",  releveBinOpEntierA (*))
       , ("quot",  releveBinOpEntierA quot)
       , ("if" , ifthenelseA) ]
       
ifthenelseA :: ValeurA
ifthenelseA = VFonctionA (\(VLitteralA (Bool b)) -> VFonctionA (\(VLitteralA x )-> VFonctionA (\(VLitteralA y) -> VLitteralA (if b
                                                                                                                              then x
                                                                                                                              else y))))
                                                                                                                              
-- voir fonction forever                                                                                                                           
