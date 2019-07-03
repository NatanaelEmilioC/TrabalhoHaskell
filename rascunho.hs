import System.Process

data Formula = Lit Bool
            | Var String
            | E Formula Formula
            | Ou Formula Formula
            | Nao Formula
        deriving ( Show , Eq )

type Contexto = [(String, Bool)]

type TabelaVerdade = [(Contexto, Bool)]
{-}
main = do 
    putStrLn "Escolha a opcao:"
    putStrLn "1- Avaliar formula"
    putStrLn "2- Tabela verdade"
    putStrLn "3- Tautologia"
    putStrLn "4- Contradição"
    opcao <- getLine
    putStrLn "Digite a entrada"
    entrada <- getLine
    case opcao of
               "1" -> avalia
               "2" -> truthTable
               "3" -> tautologia
               "4" -> contradicao
               "5" -> teste entrada

teste :: String -> IO()
teste "True" = putStrLn "Teste Funcionando"-}

avalia :: Contexto -> Formula -> Bool
avalia [(y, True)] (Var x) | fst (y, True) == x = True
avalia [(y, False)] (Var x) | fst (y, False) == x = False
avalia [(p,pv),(q,qv)] (E x y) = avalia [(p,pv)] x && avalia [(q,qv)] y
avalia [(p,pv),(q,qv)] (Ou x y) = avalia [(p,pv)] x || avalia [(q,qv)] y
avalia [(p,pv)](Nao x) = not(avalia [(p,pv)] x)
avalia [] (Lit True) = True
avalia [] (Lit False) = False
avalia [] (E x y) = avalia [] x && avalia [] y
avalia [] (Ou x y) = avalia [] x || avalia [] y
avalia [] (Nao x) = not(avalia [] x)

{-
avalia :: Contexto -> Formula -> Bool
avalia x y
    |avalia [(y, True)] (Var x) | fst (y, True) == x = True
    |avalia [(y, False)] (Var x) | fst (y, False) == x = False
    |avalia [(p,pv),(q,qv)] (E x y) = avalia [(p,pv)] x && avalia [(q,qv)] y
    |avalia [(p,pv),(q,qv)] (Ou x y) = avalia [(p,pv)] x || avalia [(q,qv)] y
    |avalia [(p,pv)](Nao x) = not(avalia [(p,pv)] x)
    |avalia [] (Lit True) = True
    |avalia [] (Lit False) = False
    |avalia [] (E x y) = avalia [] x && avalia [] y
    |avalia [] (Ou x y) = avalia [] x || avalia [] y
    |avalia [] (Nao x) = not(avalia [] x)
-}

{-
lst2for :: [[Bool]] -> Formula
lst2for (x:[]) = troca x
lst2for (x:xs) = (Ou (troca x)(lst2for xs))
troca (True : []) = V
troca (False : []) = F
troca (True : xs) = (E V (troca xs))
troca (False : xs) = (E F (troca xs))

--for2lst :: Formula -> [[Bool]] que transforma uma formula em uma lista de lista de booleano.
for2lst :: Formula -> [[Bool]]
for2lst (Lit False) = [False] : []
for2lst (Lit True) = [True] : []
for2lst (Ou p q) = (for2lst p) ++ (for2lst q)
for2lst (E p q) = [[avaliaAux $ for2lst p]++[avaliaAux $ for2lst q]] 

--Exemplo: (V e F e V) ou V ou (F e F) = [[True, False, True], [True], [False, False]]
avaliaAux :: [[Bool]] -> Bool
avaliaAux xs = foldr(||) False $ [ y | x <- xs , let y = foldr(&&) True x ]
-}
truthTable :: Formula -> TabelaVerdade
truthTable = undefined

tautologia :: Formula -> Bool
tautologia (Lit True) = True

contradicao :: Formula -> Bool
contradicao =  undefined