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

--eval :: Formula ->  

avalia :: Contexto -> Formula -> Bool
--avalia :: Contexto -> String -> Bool
--avalia = undefined
avalia [(y, True)] x | fst (y, True) == x = True
avalia [(y, False)] x | fst (y, False) == x = False
{-avalia (E p q) = avalia p && avalia q
avalia (Ou p q) = avalia p || avalia q
avalia (Nao q) = not(avalia q)-}

truthTable :: Formula -> TabelaVerdade
truthTable = undefined

tautologia :: Formula -> Bool
tautologia (Lit True) = True

contradicao :: Formula -> Bool
contradicao =  undefined


{--
clear :: IO ()
clear = system "cls"

eval :: Formula -> Bool
eval (V) = True
eval (F) = False
eval (E p q) = eval p && eval q
eval (Ou p q) = eval p || eval q
eval (Nao q) = not(eval q)


avalia :: [[Bool]] -> Bool
avalia xs = foldr(||) False $ [ y | x <- xs , let y = foldr(&&) True x ]

for2lst :: Formula -> [[Bool]]
for2lst (F) = [eval F] : []
for2lst (V) = [eval V] : []
for2lst (Ou p q) = (for2lst p) ++ (for2lst q)
for2lst (E p q) = [[avalia $ for2lst p]++[avalia $ for2lst q]] 

lst2for :: [[Bool]] -> Formula
lst2for (x:[]) = troca x
lst2for (x:xs) = (Ou (troca x)(lst2for xs))
troca (True : []) = V
troca (False : []) = F
troca (True : xs) = (E V (troca xs))
troca (False : xs) = (E F (troca xs))
--}