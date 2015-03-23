{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Parser
import Data.Maybe
import System.IO

--WISSOCQ Sarah & PETIT Antoine

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


releveBinOpEntierA :: (Integer -> Integer -> Integer) -> ValeurA
releveBinOpEntierA = \f -> (VFonctionA (\(VLitteralA (Entier x)) -> (VFonctionA (\(VLitteralA (Entier y)) -> (VLitteralA (Entier (x `f` y)))))))

envA :: Environnement ValeurA
envA = [ ("neg",   negA)
       , ("add",   releveBinOpEntierA (+))
       , ("soust", releveBinOpEntierA (-))
       , ("mult",  releveBinOpEntierA (*))
       , ("quot",  releveBinOpEntierA quot)
       , ("if" ,   ifthenelseA) ]
       
ifthenelseA :: ValeurA
ifthenelseA = VFonctionA (\(VLitteralA (Bool b)) -> VFonctionA (\(VLitteralA x )-> VFonctionA (\(VLitteralA y) -> VLitteralA (if b
                                                                                                                              then x
                                                                                                                              else y))))
                                                                                                                              
{-main = do putStr "minilang>"
          l <- getLine
          print (interpreteA envA (ras l))
          main-}

--Interprète avec erreurs
data ValeurB = VLitteralB Litteral
             | VFonctionB (ValeurB -> ErrValB)

type MsgErreur = String
type ErrValB   = Either MsgErreur ValeurB

instance Show ValeurB where
    show (VFonctionB _) = "λ"
                       -- ^ ou "VFonctionA _", ou "<fun>" ou toute
                       --   autre représentation des fonctions
    show (VLitteralB (Entier i)) = show i
    show (VLitteralB (Bool b)) = show b

interpreteB :: Environnement ValeurB -> Expression -> ErrValB
interpreteB _ (Lit x)          = Right (VLitteralB x)
interpreteB env (Var x)        = case lookup x env of
                                 Nothing -> Left ("la variable " ++ x ++ " n'est pas definie")
                                 Just v  -> Right v
interpreteB env (Lam nom expr) = Right (VFonctionB (\v -> interpreteB ((nom,v):env) expr))
interpreteB env (App e1 e2)    = case interpreteB env e1 of 
                                 Right (VFonctionB f)          -> case interpreteB env e2 of Right r -> f r
                                                                                             Left l -> Left l
                                 Right (VLitteralB (Entier n)) -> Left (show n ++ " n'est pas une fonction, application impossible")
                                 Right (VLitteralB (Bool b))   -> Left (show b ++ " n'est pas une fonction, application impossible")
                                 Left l                        -> Left l

addB :: ValeurB
addB = VFonctionB f
       where f (VLitteralB (Entier n)) = Right (VFonctionB h)
                                         where h (VLitteralB (Entier n2)) = Right (VLitteralB (Entier (n+n2)))
                                               h (VLitteralB (Bool b))    = Left (show b ++ " n'est pas un entier")
                                               h (VFonctionB _)           = Left ("λ n'est pas un entier")
             f (VLitteralB (Bool b))   = Left (show b ++ " n'est pas un entier")
             f (VFonctionB _)          = Left ("λ n'est pas un entier")

quotB :: ValeurB
quotB = VFonctionB f
        where f (VLitteralB (Entier n)) = Right (VFonctionB h)
                                          where h (VLitteralB (Entier 0))  = Left ("division par zero")
                                                h (VLitteralB (Entier n2)) = Right (VLitteralB (Entier (n `quot` n2)))
                                                h (VLitteralB (Bool b))    = Left (show b ++ " n'est pas un entier")
                                                h (VFonctionB _)           = Left ("λ n'est pas un entier")
              f (VLitteralB (Bool b)) = Left (show b ++ " n'est pas un entier")
              f (VFonctionB _)        = Left ("λ n'est pas un entier")

--Interprète traçant
data ValeurC = VLitteralC Litteral
             | VFonctionC (ValeurC -> OutValC)

type Trace   = String
type OutValC = (Trace, ValeurC)

instance Show ValeurC where
    show (VFonctionC _) = "λ"
                       -- ^ ou "VFonctionA _", ou "<fun>" ou toute
                       --   autre représentation des fonctions
    show (VLitteralC (Entier i)) = show i
    show (VLitteralC (Bool b)) = show b

interpreteC :: Environnement ValeurC -> Expression -> OutValC
interpreteC _ (Lit l) = ("", VLitteralC l)
interpreteC env (Var x)        = ("", fromJust (lookup x env))
interpreteC env (Lam nom expr) = ("", (VFonctionC (\v -> interpreteC ((nom,v):env) expr)))
interpreteC env (App e1 e2)    = ((s1 ++ s2 ++ "." ++ s), v)
                                 where (s1,VFonctionC f)  = interpreteC env e1
                                       (s2,v2)            = interpreteC env e2
                                       (s, v)             = f v2

pingC :: ValeurC
pingC =  VFonctionC (\x -> ("p",x))

--Interprète monadique

data ValeurM m = VLitteralM Litteral
               | VFonctionM (ValeurM m -> m (ValeurM m))

instance Show (ValeurM m) where
    show (VFonctionM _) = "λ"
                       -- ^ ou "VFonctionA _", ou "<fun>" ou toute
                       --   autre représentation des fonctions
    show (VLitteralM (Entier i)) = show i
    show (VLitteralM (Bool b)) = show b

data SimpleM v = S v
               deriving Show

interpreteSimpleM :: Environnement (ValeurM SimpleM) -> Expression -> SimpleM (ValeurM SimpleM)
interpreteSimpleM _ (Lit x)          = S (VLitteralM x)
interpreteSimpleM env (Var x)        = S (fromJust (lookup x env)) 
interpreteSimpleM env (Lam nom expr) = S (VFonctionM (\v -> interpreteM ((nom,v):env) expr))
interpreteSimpleM env (App e1 e2)    = f v2
                                where S (VFonctionM f) = interpreteM env e1
                                      S v2             = interpreteM env e2

instance Monad SimpleM where
    return      = S
    (S v) >>= f = f v

interpreteM :: Monad m => Environnement (ValeurM m) -> Expression -> m (ValeurM m)
interpreteM _ (Lit x)          = return (VLitteralM x)
interpreteM env (Var x)        = return (fromJust (lookup x env)) 
interpreteM env (Lam nom expr) = return (VFonctionM (\v -> interpreteM ((nom,v):env) expr))
interpreteM env (App e1 e2)    = do VFonctionM f <- interpreteM env e1
                                    v            <- interpreteM env e2
                                    f v

type InterpreteM m = Environnement (ValeurM m) -> Expression -> m (ValeurM m)

interpreteS :: InterpreteM SimpleM
interpreteS = interpreteM

data TraceM v = T (Trace, v)
              deriving Show

instance Monad TraceM where
    return v      = T ("",v)
    T (t,v) >>= f = T (t ++ tres, vres)
        where T (tres,vres) = f v

interpreteMT :: InterpreteM TraceM
interpreteMT = interpreteM

pingM :: ValeurM TraceM
pingM = VFonctionM (\v -> T ("p", v))

interpreteMT' :: InterpreteM TraceM
interpreteMT' env (App e1 e2) = T ((s1 ++ s2 ++ "." ++ sr), vr)
                                  where T (s1, VFonctionM f) = interpreteMT' env e1
                                        T (s2, v2) = interpreteMT' env e2
                                        T (sr, vr) = f v2
interpreteMT' env (Lam nom e) = T ("", VFonctionM (\v -> interpreteMT' ((nom,v):env) e))
interpreteMT' env s           = interpreteMT env s

data ErreurM v = Succes v
               | Erreur String
               deriving Show

instance Monad ErreurM where
    fail e            = Erreur e
    return            = Succes 
    (Succes v) >>= f  = f v
    (Erreur e) >>= _  = fail e

interpreteE :: InterpreteM ErreurM
interpreteE env (Var x)        = case lookup x env of
                                 Nothing -> fail ("la variable " ++ x ++ " n'est pas definie")
                                 Just v  -> return v
interpreteE env (App e1 e2)    = (interpreteE env e1 >>= \r ->
                                    case r of
                                    VFonctionM f -> interpreteE env e2 >>= \r2 ->
                                                    f r2
                                    VLitteralM (Entier n) -> fail (show n ++ " n'est pas une fonction, application impossible")
                                    VLitteralM (Bool b)   -> fail (show b ++ " n'est pas une fonction, application impossible"))
interpreteE env x = interpreteM env x

class Monad m => Injectable m t where
    injecte :: t -> ValeurM m

instance Monad m => Injectable m Bool where
    injecte = VLitteralM . Bool

instance Monad m => Injectable m Integer where
    injecte = VLitteralM . Entier

instance (Monad m, Injectable m t) => Injectable m (Bool -> t) where
  injecte f = VFonctionM (\(VLitteralM (Bool b)) -> return (injecte (f b)))

instance (Monad m, Injectable m t) => Injectable m (Integer -> t) where
  injecte f = VFonctionM (\(VLitteralM (Entier v)) -> return (injecte (f v)))

envM :: Monad m => Environnement (ValeurM m)
envM = [ ("add", injecte ((+) :: Integer -> Integer -> Integer))
       , ("soust", injecte ((-) :: Integer -> Integer -> Integer))
       , ("mult", injecte ((*) :: Integer -> Integer -> Integer))
       , ("quot", injecte (quot :: Integer -> Integer -> Integer))
       , ("et", injecte (&&))
       , ("ou", injecte (||))
       , ("non", injecte not) ]

main :: IO ()
main = do putStr "minilang>"
          hFlush stdout
          b <- isEOF
          if b 
            then putStr "\nGood bye.\n"
            else (do l <- getLine
                     putStr (show (interpreteE envM (ras l)) ++ "\n")
                     main )