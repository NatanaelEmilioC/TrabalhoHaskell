import Data.Maybe (fromJust)
import Data.List (nub)
import System.IO
import Text.Html

data Formula = Lit Bool
            | Var String
            | E Formula Formula
            | Ou Formula Formula
            | Nao Formula
            | Imp Formula Formula
            | Bic Formula Formula
        deriving ( Show , Eq , Read)

type Contexto = [(String, Bool)]

type TabelaVerdade = [(Contexto, Bool)]

booleanos = [True, False]

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

html :: IO ()
html = do 
    
    putStrLn "Escolha a opcao:"
    --putStrLn "1- Avaliar expressao"
    putStrLn "2- Tabela verdade"
    --putStrLn "3- Tautologia"
    --putStrLn "4- Contradição"
    opcao <- getLine
    putStrLn "Digite a entrada"
    dado <- getLine
    let entrada = read dado :: Formula
    escreve $ escreveNoHtml entrada
    
    {-case opcao of
               --"1" -> avalia entrada
               "2" -> truthTable entrada
               "3" -> tautologia entrada
               "4" -> contradicao entrada
               "5" -> teste "True"

-}

lerTabelaVerdade :: Formula -> Html
lerTabelaVerdade (Lit v) = th $ stringToHtml $ show(v)
lerTabelaVerdade (Var v) = th $ stringToHtml v
lerTabelaVerdade (Nao f1) = lerTabelaVerdade f1
lerTabelaVerdade (E f1 f2) = lerTabelaVerdade f1 +++ lerTabelaVerdade f2
lerTabelaVerdade (Ou f1 f2) = lerTabelaVerdade (E f1 f2)
lerTabelaVerdade (Imp f1 f2) = lerTabelaVerdade (E f1 f2)
lerTabelaVerdade (Bic f1 f2) = lerTabelaVerdade (E f1 f2)

linhasTabelaVerdade :: TabelaVerdade -> Html
linhasTabelaVerdade (cabeca:[]) = tr ! [align "center"] $ do{
    (foldl1 (+++) [td $ stringToHtml v 
                        | let contexto = fst(cabeca), v <- map show $ map snd contexto])
    +++ (td $ stringToHtml $ show $ snd(cabeca)) 
    }
linhasTabelaVerdade (cabeca:cauda) = linhasTabelaVerdade [cabeca] +++ linhasTabelaVerdade cauda

constroiTabelaVerdadeHtml :: Formula -> Html
constroiTabelaVerdadeHtml f = tab ! [border 1, align "center"]
 where tab = table $ do{
    (tr $ th ! [colspan 40] $ stringToHtml $ "Tabela Verdade") +++
    (tr $ (lerTabelaVerdade f) +++ (th $ stringToHtml $ show(f))) +++
    (linhasTabelaVerdade $ truthTable f)
}

escreveNoHtml :: Formula -> Html
escreveNoHtml f = thehtml $ do {
    (header $ do thetitle $ stringToHtml "Table Truth") +++ 
    (body $ do {(constroiTabelaVerdadeHtml f)})
}

escreve :: Html -> IO()
escreve doc = do 
    let str = renderHtml doc
    writeFile "saida.html" str
