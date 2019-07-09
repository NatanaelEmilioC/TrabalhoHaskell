{-
Trabalho prático II Haskell - Tabele
Linguagens de programação
Natanael Emilio da Costa
Matricula - 16.8298
-}
import Data.Maybe (fromJust)
import Data.List (nub)
import System.IO
import Text.Html
import qualified Data.Text as Separador

data Formula = Lit Bool
            | Var String
            | E Formula Formula
            | Ou Formula Formula
            | Nao Formula
            | Imp Formula Formula -- inclusao da implicação
            | Bic Formula Formula -- inclusao da bicondicional
        deriving ( Show , Eq , Read)

type Contexto = [(String, Bool)]

type TabelaVerdade = [(Contexto, Bool)]

--função main para gerar menu de operações possiveis
main :: IO ()
main = do 
    putStrLn "Escolha a opcao:"
    putStrLn "1- Avaliar expressao"
    putStrLn "2- Tabela verdade"
    putStrLn "3- Tautologia"
    putStrLn "4- Contradição"
    putStrLn "5- Tabela verdade Html"
    opcao <- getLine
    case opcao of -- submenus para as operações
               "1" -> subMenuAvalia
               "2" -> subMenuTabelaVerdade
               "3" -> subMenuTautologia
               "4" -> subMenuContradicao
               "5" -> subMenuTabelaHtml

-- submenu para avaliação da formula
subMenuAvalia :: IO ()
subMenuAvalia = do
    putStrLn "Digite o contexto" -- solicita contexto 
    contexto <- getLine
    let x = read contexto :: Contexto
    putStrLn "Digite a formula" -- solicita a formula para a função
    formula <- getLine
    let y = read formula :: Formula -- parse entrada para Formula
    putStrLn $"saida : " ++ show(avalia x y)

-- submenu para gerar tabela verdade
subMenuTabelaVerdade :: IO ()
subMenuTabelaVerdade = do
    putStrLn "Digite a formula" -- solicita a formula para a função
    entrada <- getLine
    let y = read entrada :: Formula -- parse entrada para Formula
    putStrLn $"saida : " ++ show(truthTable y)

--submenu para avaliar tautologia    
subMenuTautologia :: IO ()
subMenuTautologia = do
    putStrLn "Digite a formula" -- solicita a formula para a função
    entrada <- getLine
    let y = read entrada :: Formula -- parse entrada para Formula
    putStrLn $"saida : " ++ show(tautologia y)

-- submenu para avaliar contradição
subMenuContradicao :: IO ()
subMenuContradicao = do
    putStrLn "Digite a formula" -- solicita a formula para a função
    entrada <- getLine
    let y = read entrada :: Formula -- parse entrada para Formula
    putStrLn $"saida : " ++ show(contradicao y)

-- submenu para gerar tabela verdade em html
subMenuTabelaHtml :: IO ()
subMenuTabelaHtml = do
    putStrLn "Digite a formula" -- solicita a formula para a função
    entrada <- getLine
    let y = read entrada :: Formula -- parse entrada para Formula
    escreveArquivo $ escreveNoHtml y -- executa a escrita do html com a entrada
    putStrLn $ "Tabela " ++ show(entrada) ++ " criada"

{- Avalia uma formula e retorna a resposta booleana correspondente-}
avalia :: Contexto -> Formula -> Bool
avalia contexto (Lit x) = x
avalia contexto (Var x) 
    | fst(head(contexto)) == x = snd(head(contexto)) 
    | otherwise = avalia (tail(contexto)) (Var x)
avalia contexto (E x y) = avalia contexto x && avalia contexto y
avalia contexto (Ou x y) = avalia contexto x || avalia contexto y
avalia contexto (Nao x) = not(avalia contexto x)
avalia contexto (Imp x y) = not (avalia contexto x) || avalia contexto y -- avaliar implicação
avalia contexto (Bic x y) = avalia contexto (Imp x y) && avalia contexto (Imp y x) -- avaliar bicondicional

{-Gera uma tabela verdade-}
truthTable :: Formula -> TabelaVerdade
truthTable expressao = do
    [(contexto, avalia contexto expressao) | contexto <- tabelaBooleanos (nub $ variaveisExpressao expressao)] -- remove as variaveis repetidas

{-Gera uma tabela de todas as combinações de variaveis e booleanos-}
tabelaBooleanos :: [String] -> [[(String, Bool)]]
tabelaBooleanos [] = [[]]
tabelaBooleanos (variavel:cauda)
    | variavel == "True" = [(variavel,True) : resto | resto <- tabelaBooleanos cauda ] -- estabelece um caso fixo para o literal True
    | variavel == "False" = [(variavel,False) : resto | resto <- tabelaBooleanos cauda ] -- estabelece um caso fixo para o literal False
    | otherwise = [(variavel,valor) : resto | valor <- [True,False], resto <- tabelaBooleanos cauda ]

{-Função que lista todas as variaveis da fórmula-}
variaveisExpressao :: Formula -> [String]   
variaveisExpressao (Lit l) = [show(l)]
variaveisExpressao (Var v) = [v]
variaveisExpressao (Nao e) = variaveisExpressao e
variaveisExpressao (E x y) = variaveisExpressao x ++ variaveisExpressao y
variaveisExpressao (Ou x y) = variaveisExpressao x ++ variaveisExpressao y
variaveisExpressao (Imp x y) = variaveisExpressao x ++ variaveisExpressao y
variaveisExpressao (Bic x y) = variaveisExpressao x ++ variaveisExpressao y

{- Defina as funções de tautologia e contradiçãoo para uma fórmula.
avalia se a formula é uma tautologia -}
tautologia :: Formula -> Bool
tautologia expressao = and $ map (==True) [snd(x) | x <- truthTable expressao]

--avalia se a formula é uma contradição    
contradicao :: Formula -> Bool
contradicao expressao = and $ map (==False) [snd(x) | x <- truthTable expressao]

{- funções para gerar o HTML
recebe uma fórmula e a transforma em html-}
lerTabelaVerdade :: Formula -> [Html]
lerTabelaVerdade x = map (\x -> th $ stringToHtml x ) $ nub $ variaveisExpressao x

{-constroi a tabela do html-}
constroiTabelaVerdadeHtml :: Formula -> Html
constroiTabelaVerdadeHtml formula = tabela ! [border 3, align "center"]
 where tabela = table $ do{ 
    (tr $ th ! [colspan 12, bgcolor "#bbffee"] $ stringToHtml $ "Tabela Verdade") +++ --cabeça da tabela
    (tr $ ( concatHtml $ lerTabelaVerdade formula) +++ --concatena as linhas da tabela
    (th ! [ bgcolor "#bbffee", nowrap, align "center"] $ stringToHtml $ show(formula))) +++
    (linhasTabelaVerdade $ truthTable formula) } -- avalia a tabela verdade

{-Escreve as parte do html-}
escreveNoHtml :: Formula -> Html
escreveNoHtml formula = thehtml $ do 
    { (header $ do {(thetitle $ stringToHtml "Tabela Verdade")}) --cabeçalho do html
    +++ (body $ do {(constroiTabelaVerdadeHtml formula)  -- corpo do html
    +++ (primHtml rodape)}) } -- inclusão do rodapé


{-Escreve as linhas da tabela verdade para o html-}
linhasTabelaVerdade :: TabelaVerdade -> Html
linhasTabelaVerdade (cabeca:[]) = tr ! [align "center"] $ do
    {(foldl1 (+++) [td ! [align "center"] $ stringToHtml conteudo 
        | let contexto = fst(cabeca), conteudo <- map show $ map snd contexto]) +++ 
    (td ! [bgcolor "#bbffee", align "center"] $ stringToHtml $ show $ snd(cabeca))}-- coluna de respostas
linhasTabelaVerdade (cabeca:cauda) = linhasTabelaVerdade [cabeca] +++ linhasTabelaVerdade cauda

--Rodapé para inclusão no html
rodape = "<footer><p>Desenvolvido por: Natanael Emilio da Costa</p>"++
        "<p>Contao: <a href=\"natanaelemilio@gmail.com\">natanaelemilio@gmail.com</a>.</p></footer>"

--Escreve o arquivo html
escreveArquivo :: Html -> IO()
escreveArquivo conteudo = do 
    let conteudoRenderizado = renderHtml conteudo
    outh <- openFile "tabelaVerdade.html" WriteMode
    hPutStr outh (conteudoRenderizado)
    hClose outh
