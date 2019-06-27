import System.Process

data Formula = Lit Bool
            | Var String
            | E Formula Formula
            | Ou Formula Formula
            | Nao Formula
        deriving ( Show , Eq )

type Contexto = [(String, Bool)]

type TabelaVerdade = [(Contexto, Bool)]

menu :: IO ()
menu = do
      putStrLn . unlines $ map concatNums choices
      choice <- getLine
      case validate choice of
         Just n  -> executar . read $ choice
         Nothing -> putStrLn "Tente novamente"

      menu 

   where concatNums (i, (s, _)) = show i ++ " - " ++ s

validate :: String -> Maybe Int
validate s = isValid (reads s)
   where isValid []            = Nothing
         isValid ((n, _):_) 
               | (n < 1) || (n > length choices) = Nothing
               | otherwise     = Just n

choices :: [(Int, (String, IO ()))]
choices = zip [1.. ] [
   ("Avaliar formula", avalia)
 , ("Tabela Verdade", truthTable)
 , ("Tautologia", tautologia)
 , ("Contradição", contradicao)
 , ("Teste", teste True)
 ]

executar :: Int -> IO ()
executar n = doExec $ filter (\(i, _) -> i == n) choices
   where doExec ((_, (_,f)):_) = f

teste :: Bool -> IO()
teste True = putStrLn "Teste Funcionando"

--avalia :: Contexto -> Formula -> Bool
avalia = undefined

--truthTable :: Formula -> TabelaVerdade
truthTable = undefined

--tautologia :: Formula -> Bool
tautologia = undefined

--contradicao :: Formula -> Bool
contradicao =  undefined


clear :: IO ()
clear = system "cls"

{--
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