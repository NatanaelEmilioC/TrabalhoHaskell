import Data.Maybe (fromJust)
import Data.List (nub)

data Formula = Lit Bool
            | Var String
            | E Formula Formula
            | Ou Formula Formula
            | Nao Formula
            | Imp Formula Formula
            | Bic Formula Formula
        deriving ( Show , Eq )

type Contexto = [(String, Bool)]

type TabelaVerdade = [(Contexto, Bool)]

booleanos = [True, False]

{-
main = do 
    putStrLn "Escolha a opcao:"
    putStrLn "1- Avaliar expressao"
    putStrLn "2- Tabela verdade"
    putStrLn "3- Tautologia"
    putStrLn "4- Contradição"
    opcao <- getLine
    putStrLn "Digite a entrada"
    entrada <- getLine
    case opcao of
               "1" -> avalia entrada
               "2" -> truthTable
               "3" -> tautologia
               "4" -> contradicao
               "5" -> teste entrada-}

teste :: String -> IO()
teste "True" = putStrLn "Teste Funcionando"

avalia :: Contexto -> Formula -> Bool
avalia contexto (Lit x) = x
avalia contexto (Var x) 
    | fst(head(contexto)) == x = snd(head(contexto)) 
    | otherwise = avalia (tail(contexto)) (Var x)
avalia contexto (E x y) = avalia contexto x && avalia contexto y
avalia contexto (Ou x y) = avalia contexto x || avalia contexto y
avalia contexto (Nao x) = not(avalia contexto x)
avalia contexto (Imp x y) = not (avalia contexto x) || avalia contexto y
avalia contexto (Bic x y) = avalia contexto (Imp x y) && avalia contexto (Imp y x)

truthTable :: Formula -> TabelaVerdade
truthTable expressao = [(contexto, avalia contexto expressao) | contexto <- tabelaBooleanos (variaveis expressao)]

tabelaBooleanos :: [String] -> [[(String, Bool)]]
tabelaBooleanos [] = [[]]
tabelaBooleanos (a:as) = [(a,b) : r | b <- booleanos, r <- tabelaBooleanos as]

variaveis :: Formula -> [String]
variaveis = nub . variaveisExpressao

variaveisExpressao :: Formula -> [String]
variaveisExpressao (Var v) = [v]
variaveisExpressao (Nao e) = variaveisExpressao e
variaveisExpressao (E x y) = variaveisExpressao x ++ variaveisExpressao y
variaveisExpressao (Ou x y) = variaveisExpressao x ++ variaveisExpressao y
variaveisExpressao (Imp x y) = variaveisExpressao x ++ variaveisExpressao y
variaveisExpressao (Bic x y) = variaveisExpressao x ++ variaveisExpressao y

tautologia :: Formula -> Bool
tautologia expressao = and $ map (==True) [snd(x) | x <- truthTable expressao]
    
contradicao :: Formula -> Bool
contradicao expressao = and $ map (==False) [snd(x) | x <- truthTable expressao]
