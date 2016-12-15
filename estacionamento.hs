--	PROJETO EM HASKELL - LPF 2016.2
--	ALUNOS: 
--		VINÍCIUS PINHEIRO
--		WANESSA SOUZA
--		YURI NAKAMORA

import Data.List
import System.IO
import System.Directory
import Data.Char
--CRIAÇÃO DE CONTROLE DE ESTACIONAMENTO DE VEICULOS

path = "database.txt"

-- DEFININDO UM TIPO DE DADOS PARA AS CARACTERÍSTICAS DOS CARROS
-- MODELO, PLACA, CODIGO DO PROPRIETÁRIO, TIPO DE VEICULO
data Veiculo = Veiculo String String Int Int | Vazio deriving Show

--CLASSE MESTRA PARA CRIAÇÃO DE OUTRAS "CLASSES" E MÉTODOS 
class Mestra a where
    vazio :: a
    propriedades :: a -> [String]
    setProp :: String -> a -> String -> a
    getProp :: String -> a -> String

exibe :: Mestra a => a -> IO ()
exibe b = exibeAux (propriedades b) b where
    exibeAux :: Mestra a => [String] -> a -> IO ()
    exibeAux [] a = return ()
    exibeAux (p : resto) b = do
        putStrLn ("valor de " ++ p ++ "e : " ++ (getProp p b))
        exibeAux resto b

--CLASSE DATABASE COM CLASSES E MÉTODOS PARA ARQUIVOS

class Database a where
    encode :: a -> String
    decode :: String -> a
    encodeList :: [a] -> String
    encodeList [] = ""
    encodeList [h] = encode h
    encodeList (h:t) = (encode h) ++ "," ++ (encodeList t)
    decodeList :: String -> [a]
    decodeList s = map decode (quebra ',' s)
     
quebra :: Char -> String -> [String]
quebra s "" = []
quebra s a = [k] ++ quebra s (drop(length k + 1) a) where
    k = takeWhile (\x -> x /= s) a

instance Mestra Veiculo where
    propriedades p = ["modelo", "placa", "codPro", "tipo"]
    setProp "modelo" (Veiculo modelo placa codPro tipo) modelo2 = Veiculo modelo2 placa codPro tipo
    setProp "placa" (Veiculo modelo placa codPro tipo) placa2 = Veiculo modelo placa2 codPro tipo 
    setProp "codPro" (Veiculo modelo placa codPro tipo) codPro2 = Veiculo modelo placa (read codPro2) tipo 
    setProp "tipo" (Veiculo modelo placa codPro tipo) tipo2 = Veiculo modelo placa codPro (read tipo2)
    getProp "modelo" (Veiculo modelo placa codPro tipo) = modelo
    getProp "placa" (Veiculo modelo placa codPro tipo) = placa
    getProp "codPro" (Veiculo modelo placa codPro tipo) = show codPro
    getProp "tipo" (Veiculo modelo placa codPro tipo) = show tipo
    
instance Database Veiculo where
    encode (Veiculo m p c t) = m ++ ","++ p  ++ "," ++ (show c) ++ "," ++ (show t)
    decode s = Veiculo m p c t where
        [m, p, codPro, tipo] = quebra ',' s
        c = read codPro
        t = read tipo

--FUNÇÃO SALVAR, DADO UM PATH E [a] SALVAR NO ARQUIVO ESPECIFICADO
salvar :: Database a => String -> [a] -> IO ()
salvar modeloDoArquivo l = writeFile modeloDoArquivo (encodeList l)

--FUNÇÃO LER, RETORNA O CONTEUDO DO ARQUIVO
ler :: Database a => String -> IO [a]
ler modeloDoArquivo = do 
    conteudo <- readFile modeloDoArquivo
    return (decodeList conteudo)

-- CADASTRAR
-- CONCULTAR (Buscar Veículos)
-- EXCLUIR
-- ALTERAR

--Exemplo

cadastro :: Veiculo -> IO()
cadastro Vazio = putStrLn "Sem Veiculos!"
cadastro p = appendFile path ((encode p) ++ "\n")


-- buscarTodos :: [Veiculo]
-- buscarTodos = do
--     x <- readFile path
--     arrayLines = quebra '\n' x
--     return (arrayVeiculos arrayLines) where
--         arrayVeiculos [] = []
--         arrayVeiculos (h:t) = (quebra ',' h):arrayVeiculos t

buscarTodos :: IO() 
buscarTodos = do
    x <- readFile path
    putStrLn x
    estacionamento

excluir :: IO()
excluir = do
    handle <- openFile path ReadMode
    tempdir <- getTemporaryDirectory
    (tempName, tempHandle) <- openTempFile tempdir "temp"
    contents <- hGetContents handle
    let 
        todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "Abaixo estao os veiculos cadastrados:"
    putStr $ unlines numberedTasks
    putStrLn "\n Digite o numero do item do veiculo que voce deseja excluir"
    numberString <- getLine
    let 
        number = read numberString
        newTodoveiculos = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoveiculos
    hClose handle
    hClose tempHandle
    removeFile path
    renameFile tempName path

alterar :: IO()
alterar = do
    handle <- openFile path ReadMode
    tempdir <- getTemporaryDirectory
    (tempName, tempHandle) <- openTempFile tempdir "temp"
    contents <- hGetContents handle
    let 
        todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "Abaixo estao os veiculos cadastrados:"
    putStrLn "modelo | placa | Codigo do proprietario | Tipo do Veiculo"
    putStr $ unlines numberedTasks
    putStrLn "\n Digite o numero do item do veiculo que voce deseja alterar"
    numberString <- getLine
    let 
        number = read numberString
        newTodoItens = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItens
    hClose handle
    hClose tempHandle
    removeFile path
    renameFile tempName path
    opcao2

buscarAux :: [String] -> [[String]]
buscarAux [] = [[]]
buscarAux (h:t) = [quebra ',' h] ++ buscarAux t  

compareBuscar :: String -> [[String]] -> [String]
--compareBuscar "" [[]] = (return ())
compareBuscar s t = head (filter (elem s) t) 
     

buscar :: String -> IO()
buscar s = do
    handle <- openFile path ReadMode
    tempdir <- getTemporaryDirectory
    (tempName, tempHandle) <- openTempFile tempdir "temp"
    contents <- hGetContents handle
    let
        todoTasks = lines contents
        numberedTasks = buscarAux todoTasks 
        comentario = ["\nVeiculo encontrado: ","modelo: ","placa: ", "Codigo do proprietario: ","Tipo do veiculo: "]
        opcoes = zipWith (++) comentario (compareBuscar s numberedTasks) 
    putStrLn "Buscas Encontradas: "
    putStr $ unlines opcoes


loop :: IO() -> IO()
loop p = do 
    putStr "Deseja Continuar (S/N) ?"
    linha <- getLine
    if (linha == "S") then menu else (return ())


--OPÇÕES DO menu

entrada = do
    putStrLn "\t\t MANOBRA HASKELL"
    putStrLn "\nCAIXA "
    putStr "Digite a placa do Veiculo: "   
    putStrLn "Digite 0 para finalizar"
    placa <- getLine 
    
    menu

    
    
menu = do
    putStrLn "\t\t\t\t MANOBRA HASKELL"
    putStrLn "\nDigite 1 para ESTACIONAMENTO"
    putStrLn "Digite 2 para ENTRADA"
    num <- getLine
    
    let 
        n = read num
    
    if(n == 1) then 
        estacionamento
    else if(n==2) then
        entrada
    else
        menu

        
    

estacionamento = do
    putStrLn "\t\t\t\t MANOBRA HASKELL"
    putStrLn "Controle de Veiculo"
    putStrLn "\t Digite 1 - Cadastro de Veiculo"
    putStr "\t Digite 2 - Buscar Veiculo \n"
    putStr "\t Digite 3 - Alterar Veiculo \n"
    putStr "\t Digite 4 - Excluir Veiculo \n"
    putStr "\t Digite 5 - Buscar Todos os Veiculos \n"
    putStr "\t Digite 6 para sair \n"
    respostas <- getLine
    putStrLn respostas
    
    let
        resposta = read respostas
        
    if (resposta == 1) then
        cadastrar_veiculo
    else if (resposta == 3) then
        alterar
    else if (resposta == 4) then
        excluir
    else if (resposta == 5) then
        buscarTodos
    else
        menu


opcao2 = do
    putStrLn "\nDigite o modelo do Veiculo que deseja buscar:"
    busca <- getLine
    putStrLn busca
    buscar busca
    estacionamento

cadastrar_veiculo = do
    putStrLn "Digite o modelo do Veiculo:"
    modelo <- getLine
    putStrLn "Digite a placa do Veiculo:"
    placa <- getLine
    putStrLn "Digite o Código do Proprietario:"
    codPros <- getLine
    putStrLn "Digite o Tipo do Veiculo [1-Pequeno 2- Médio 3-Grande]:"
    tipos <- getLine
    cadastro (Veiculo modelo placa (read codPros) (read tipos))
    estacionamento
