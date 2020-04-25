{- | Module              : Tarefa3_2018li1g160
   | Escola              : Universidade Do Minho
   | Description         : Codificação do jogo para uma String e descodificação do mesmo
   | Copyright           : Bruno Filipe de Sousa Dias <a89583@alunos.uminho.pt>
                           Luís Enes Sousa <a89597@alunos.uminho.pt>
= Introdução Tarefa 3:
Este módulo consiste em duas funções (comprime e descomprime).
  A função comprime pega no Estado do jogo atual e codifica-o para uma String.
  A função descomprime pega na String codificada e descodifica-a, retribuindo um Estado, correspondente ao estado do jogo na altura
em que este foi codificado.

= Objetivos e estratégias utilizadas (comprime):
  Na compressão do estado do jogo, comprimimos o mapa, os jogadores e os disparos individualmente, separando-os por um caratér único.
  Na compressão do mapa, comprimimos linha a linha. A estratégia utilizada foi:
  (1) Identificar um bloco;
  (2) Se o bloco seguinte fôr igual, aumentar um contador, e voltar a (1). Caso contrário, retribur uma letra identificativa do
  bloco em questão, precedida do número do contador.
  Na compressão dos jogadores e dos disparos, separámos cada um por um caratér específico e, dentro de cada jogador/disparo,
separámos as suas componentes por outro caratér.

= Objetivos e estratégias utilizadas (descomprime):
  Na descompressão começámos por separar o mapa, os jogadores e os disparos, usando uma função para descomprimir cada uma destas
componentes do Estado.
  De seguida, procedemos à descompressão do mapa, jogadores e disparos.
  Na descompressão do mapa descodificámos uma linha de cada vez, replicando o mapa conforme este tinha sido comprimido.
  No descompressão dos jogadores e dos disparos, começámos por separar estes individualmente, de forma a não haver conflito de
carateres, e aplicámos o processo inverso da compressão.

= Conclusão
  Concluindo, achámos que o objetivo foi cumprido, visto que atingimos uma taxa de compressão média de 95% em diversos testes e não
falhou a relação (descomprime (comprime e) = e) em nenhum deles.
-}

module Tarefa3_2018li1g160 where

import LI11819
import Tarefa0_2018li1g160
import Tarefa1_2018li1g160
import Tarefa2_2018li1g160
import Data.Char
import Data.List

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (1,1), direcaoJogador = D, vidasJogador = 2, lasersJogador = 2, choquesJogador = 2},Jogador {posicaoJogador = (2,3), direcaoJogador = C, vidasJogador = 1, lasersJogador = 2, choquesJogador = 3}], disparosEstado = [DisparoCanhao {jogadorDisparo = 1, posicaoDisparo = (2,2), direcaoDisparo = C},DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (1,2), direcaoDisparo = E},DisparoChoque {jogadorDisparo = 1, tempoDisparo = 3}]}]


--Estado (mapaInicial (6,6)) [Jogador (1,1) D 2 2 2, Jogador (2,3) C 1 2 3] [DisparoCanhao 1 (2,2) C, DisparoLaser 0 (1,2) E, DisparoChoque 1 3]

-- * Funções principais da Tarefa 3.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.
comprime :: Estado -> String
comprime (Estado mapa jogadores disparos) = comprimirMapa mapa ++ "|" ++ comprimirVariosJogadores jogadores ++ "|" ++ comprimirTodosDisparos disparos
-- Dividir a compressão entre Mapas, jogadores e etc. por "|" 



-- | Comprime o mapa para uma string
comprimirMapa :: Mapa -> String
comprimirMapa [] = ""
comprimirMapa [x] = comprimirLinhaMapa (group x)
comprimirMapa (h:t) = comprimirLinhaMapa (group h) ++ "/" ++ comprimirMapa t

-- | Comprime uma linha do mapa
comprimirLinhaMapa :: [[Peca]] -> String
comprimirLinhaMapa [] = ""
comprimirLinhaMapa (h:t) = auxComprimirMapa h ++ comprimirLinhaMapa t

-- | Recebe uma lista de peças iguais, e comprime-as, retribuindo apenas o numero de peças e um identificador seu.
-- | Se apenas receber uma peça, retribui apenas o identificador da peça.
auxComprimirMapa :: [Peca] -> String
auxComprimirMapa l | acumulado == 1 = blococomp
                   | otherwise = (show (length l)) ++ blococomp
                   where blococomp = interpretaBloco (head l)
                         acumulado = length l

-- | Recebe um bloco e retribui uma string correspondente
interpretaBloco :: Peca -> String
interpretaBloco Vazia = "V"
interpretaBloco (Bloco Indestrutivel) = "I"
interpretaBloco (Bloco Destrutivel) = "D"


{-
-- | Função responsável pela compressão de um Mapa dado, devolvendo uma única String
comprimirMapa :: Mapa -> String
comprimirMapa []  = ""
comprimirMapa mapa = posicoesMatrizColuna (posicoesMatrizTodasLinhas mapa)

-- Funções auxiliares:
-- | Função auxiliar que recebe o conjunto de Strings resultantes das funções seguintes e as separa com "/" para distinção das diferentes linhas do mapa
posicoesMatrizColuna :: [String] -> String
posicoesMatrizColuna [h]    = h
posicoesMatrizColuna (h:ht) = h ++ "/" ++ posicoesMatrizColuna ht


-- | Função auxiliar que transforma todas as linhas do mapa em Strings (recebe um Mapa e comprime todas as linhas deste, devolvendo uma String)
posicoesMatrizTodasLinhas :: Mapa -> [String]
posicoesMatrizTodasLinhas []     = []
posicoesMatrizTodasLinhas (h:ht) = posicoesMatrizLinhaTotalCmpri (posicoesMatrizLinha h) : posicoesMatrizTodasLinhas ht

-- | Função auxiliar que recebe uma String representante de Peças e agrupa 3 Peças iguais seguidas, caso existam, devolvendo uma String ainda mais comprimida (mais pequena)
posicoesMatrizLinhaTotalCmpri :: String -> String
posicoesMatrizLinhaTotalCmpri ""  = ""
posicoesMatrizLinhaTotalCmpri [x]     = [x]
posicoesMatrizLinhaTotalCmpri [x,y]   = x : [y]
posicoesMatrizLinhaTotalCmpri (x:y:z:xs) | x=='i' && y=='i' && z=='i' = "I" ++ posicoesMatrizLinhaTotalCmpri xs
                                         | x=='d' && y=='d' && z=='d' = "D" ++ posicoesMatrizLinhaTotalCmpri xs
                                         | x=='v' && y=='v' && z=='v' = "V" ++ posicoesMatrizLinhaTotalCmpri xs
                                         | otherwise = x : posicoesMatrizLinhaTotalCmpri (y:z:xs)

-- | Função auxiliar que recebe uma linha do Mapa (Conjunto de Peças) e comprime-a, devolvendo uma String
posicoesMatrizLinha :: [Peca] -> String
posicoesMatrizLinha []     = ""
posicoesMatrizLinha (h:ht) | h== Vazia = "v" ++ posicoesMatrizLinha ht
                           | h== Bloco Indestrutivel = "i" ++ posicoesMatrizLinha ht
                           | h== Bloco Destrutivel   = "d" ++ posicoesMatrizLinha ht
-}



-- | Função responsável pela compressão do estado relativo de uma lista de Jogadore que recebe, devolvendo uma String
comprimirVariosJogadores :: [Jogador] -> String
comprimirVariosJogadores []     = ""
comprimirVariosJogadores [h]    = comprimirUmJogador h
comprimirVariosJogadores (h:ht) = comprimirUmJogador h ++ "!" ++ comprimirVariosJogadores ht

-- | Função que se encarrega da compressão do Estado de um uníco jogador, devolvendo uma String
comprimirUmJogador :: Jogador -> String
comprimirUmJogador (Jogador (x,y) dir vid las cho) = stringPosicaoJogador (x,y) ++ stringDirecaoJogador dir ++ stringVidLasChoqJogador vid las cho


-- * Funções auxiliares:

-- | Função auxiliar que recebe a posiçao de um jogador e a comprime, devolvendo uma String
stringPosicaoJogador :: PosicaoGrelha -> String
stringPosicaoJogador (x,y) = show x ++ "," ++ show y ++ ";"

-- | Função auxiliar que recebe a direçao de um jogador e a comprime, devolvendo uma String
stringDirecaoJogador :: Direcao -> String
stringDirecaoJogador C = "C"
stringDirecaoJogador D = "D"
stringDirecaoJogador B = "B"
stringDirecaoJogador E = "E"

-- | Função auxiliar que recebe as vidas, lasers e choques (disponíveis) de um jogador e os comprime devolvendo uma String
stringVidLasChoqJogador :: Int -> Int -> Int -> String
stringVidLasChoqJogador v l c = show v ++ "-" ++ show l ++ "~" ++ show c ++ "."



-- | Função responsável pela compressão (vai devolver uma String) do estado relativo a um conjunto de vários Disparos
comprimirTodosDisparos :: [Disparo] -> String
comprimirTodosDisparos []     = ""
comprimirTodosDisparos [x]    = comprimirDisparo x
comprimirTodosDisparos (x:xs) = comprimirDisparo x ++ "^" ++ comprimirTodosDisparos xs

-- | Função que se encarrega da compressão (para uma String) de um disparo
comprimirDisparo :: Disparo -> String
comprimirDisparo (DisparoCanhao j p d) = "C" ++ stringJogPosDisp j p ++ stringDirDisp d
comprimirDisparo (DisparoLaser j p d)  = "L" ++ stringJogPosDisp j p ++ stringDirDisp d
comprimirDisparo (DisparoChoque j t)   = "H" ++ stringJogDisp j ++ stringTempoDisp t

-- |Função auxiliar que recebe a identificação do Jogador e a Posição do disparo e comprime ambos devolvendo uma String
stringJogPosDisp :: Int -> PosicaoGrelha -> String
stringJogPosDisp j (x,y) = show j ++ show x ++ "," ++ show y ++ ";"

-- |Função auxiliar que recebe a identificação do Jogador responsável pelo disparo e a comprime devolvendo numa String
stringJogDisp :: Int -> String
stringJogDisp j = show j

-- |Função auxiliar que recebe a direção em que disparo é feito e a comprime, devolvendo numa String
stringDirDisp :: Direcao -> String
stringDirDisp C = "C"
stringDirDisp D = "D"
stringDirDisp B = "B"
stringDirDisp E = "E"

-- |Função auxiliar que recebe os Ticks (unidades de tempo) de um disparo Choque e os comprime devolvendo numa String
stringTempoDisp :: Ticks -> String
stringTempoDisp t = show t


-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
-- __NB:__ A função 'comprime' é válida de for possível recuperar o 'Estado' utilizando a função 'descomprime', i.e.:
--
-- prop> descomprime . comprime = id
--
-- __NB:__ Esta propriedade é particularmente válida para a solução pré-definida:
--
-- prop> read . show = id
descomprime :: String -> Estado
descomprime st = Estado (descomprimeMapa st1) (descomprimeJogadores st2) (descomprimeDisparos st3)
               where st1 = takeWhile (/= '|') st
                     st2 = takeWhile (/= '|') (drop (length st1 + 1) st)
                     st3 = drop (length st1 + length st2 + 1) st

-- | Recebe um mapa comprimido (String) e retribui esse mapa descomprimido (Mapa)
descomprimeMapa :: String -> Mapa
descomprimeMapa [] = []
descomprimeMapa st = descomprimeLinhaMapa st2 : descomprimeMapa (drop 1 (dropWhile (/= '/') st))
                    where st2 = takeWhile (/= '/') st :: String
                       -- st3 = drop 1 (dropWhile (/= '/') st) :: String

-- | Recebe apenas uma linha de um mapa comprimido (String) e retribui essa mesma linha descomprimida ([Peca])
descomprimeLinhaMapa :: String -> [Peca]
descomprimeLinhaMapa [] = []
descomprimeLinhaMapa st | st2 == "" = [interpretaCharBloco charbloco] ++ descomprimeLinhaMapa (drop 1 st)
                        | otherwise = replicate (read st2) (interpretaCharBloco charbloco) ++ descomprimeLinhaMapa st3
                        where st2 = takeWhile (isDigit) st :: String
                              charbloco = head (drop (length st2) st) :: Char
                              st3 = drop ((length st2) + 1) st


-- | Recebe um carater (paça comprimida) e devolve a peça correspondente
interpretaCharBloco :: Char -> Peca
interpretaCharBloco 'V' = Vazia
interpretaCharBloco 'I' = Bloco Indestrutivel
interpretaCharBloco 'D' = Bloco Destrutivel






{-}
-- | Recebe um mapa comprimido (String) e descomprime-o, devolvendo um mapa
descomprimeMapa :: String -> Mapa
descomprimeMapa [] = []
descomprimeMapa st = descomprimeLinhaMapa st : descomprimeMapa st2
                   where st2 = drop 1 (dropWhile (/= '/') st)


-- | Funçao que recebe um mapa comprimido (String) e devolve apenas uma linha do mapa ([Peca])
descomprimeLinhaMapa :: String -> [Peca]
descomprimeLinhaMapa [] = []
descomprimeLinhaMapa ('|':t) = []
descomprimeLinhaMapa ('/':t) = []
descomprimeLinhaMapa ('V':t) = [Vazia, Vazia, Vazia] ++ descomprimeLinhaMapa t
descomprimeLinhaMapa ('I':t) = [Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel] ++ descomprimeLinhaMapa t
descomprimeLinhaMapa ('D':t) = [Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel] ++ descomprimeLinhaMapa t
descomprimeLinhaMapa ('v':t) = Vazia : descomprimeLinhaMapa t
descomprimeLinhaMapa ('i':t) = Bloco Indestrutivel : descomprimeLinhaMapa t
descomprimeLinhaMapa ('d':t) = Bloco Destrutivel : descomprimeLinhaMapa t
descomprimeLinhaMapa st = []
-}

-- | Recebe os estados dos jogadores comprimidos (String) e descomprime-os, devolvendo [Jogador]
descomprimeJogadores :: String -> [Jogador]
descomprimeJogadores [] = []
descomprimeJogadores (h:t) | isDigit h = descomprimeUmJogador (h:t) : descomprimeJogadores (dropWhile (/= '!') (h:t))
                           | h == '|' = descomprimeUmJogador t : descomprimeJogadores (dropWhile (/= '!') t)
                           | h == '!' = descomprimeUmJogador t : descomprimeJogadores (dropWhile (/= '!') t)
                           | otherwise = []

-- | Descomprime o estado de um so jogador
descomprimeUmJogador :: String -> Jogador
descomprimeUmJogador ('!':st) = Jogador (dcPosx st, dcPosy st2) (dcDirecao st3) (dcVidas st4) (dcLasers st5) (dcChoques st6)
                              where st2 = dropWhile (/= ',') st
                                    st3 = dropWhile (/= ';') st2
                                    st4 = drop 2 st3
                                    st5 = dropWhile (/= '-') st4
                                    st6 = dropWhile (/= '~') st5

descomprimeUmJogador st = Jogador (dcPosx st, dcPosy st2) (dcDirecao st3) (dcVidas st4) (dcLasers st5) (dcChoques st6)
                        where st2 = dropWhile (/= ',') st
                              st3 = dropWhile (/= ';') st2
                              st4 = drop 2 st3
                              st5 = dropWhile (/= '-') st4
                              st6 = dropWhile (/= '~') st5



-- | Função auxiliar que recebe um String e devolve um número inteiro (que vai ser relativo á Posição)
dcPosx :: String -> Int
dcPosx st = read posxcomprimida
          where posxcomprimida = takeWhile (/= ',') st

-- | Função auxiliar que recebe um String e devolve um número inteiro (que vai ser relativo á Posição)
dcPosy :: String -> Int
dcPosy (h:t) = read posycomprimida
          where posycomprimida = takeWhile (/= ';') t

-- | Função auxiliar que recebe um String e devolve uma Direção
dcDirecao :: String -> Direcao
dcDirecao (h:h2:t) = interpretaCharDirecao h2

-- | Função auxiliar que recebe um String e devolve um número inteiro (que vai ser relativo ao número de vidas que Jogador possui)
dcVidas :: String -> Int
dcVidas st = read vidascomprimidas
           where vidascomprimidas = takeWhile (/= '-') st

-- | Função auxiliar que recebe um String e devolve um número inteiro (que vai ser relativo ao número de lasers disponiveis do Jogador)
dcLasers :: String -> Int
dcLasers (h:t) = read laserscomprimidos
               where laserscomprimidos = takeWhile (/= '~') t

-- | Função auxiliar que recebe um String e devolve um número inteiro (que vai ser relativo ao número de choques disponiveis do Jogador)
dcChoques :: String -> Int
dcChoques (h:t) = read choquescomprimidos
                where choquescomprimidos = takeWhile (/= '.') t


-- | Recebe um carater e devolve uma direcao, consoante o carater
interpretaCharDirecao :: Char -> Direcao
interpretaCharDirecao 'C' = C
interpretaCharDirecao 'D' = D
interpretaCharDirecao 'B' = B
interpretaCharDirecao 'E' = E




-- | Recebe os estados dos disparos comprimidos (String) e descomprime-os, devolvendo [Disparo]
descomprimeDisparos :: String -> [Disparo]
descomprimeDisparos []    = []
descomprimeDisparos "|"   = []
descomprimeDisparos (h:t) = descomprimeUmDisparo t : descomprimeDisparos (dropWhile (/= '^') t)


-- | Descomprime o estado de um so disparo 
descomprimeUmDisparo :: String -> Disparo
descomprimeUmDisparo ('C':st) = DisparoCanhao (dcJogadorOuTicks st) (dcPosx (drop 1 st), dcPosy st2) (dcDirecao st3)
                              where st2 = dropWhile (/= ',') st
                                    st3 = dropWhile (/= ';') st2

descomprimeUmDisparo ('L':st) = DisparoLaser (dcJogadorOuTicks st) (dcPosx (drop 1 st), dcPosy st2) (dcDirecao st3)
                              where st2 = dropWhile (/= ',') st
                                    st3 = dropWhile (/= ';') st2

descomprimeUmDisparo ('H':st) = DisparoChoque (dcJogadorOuTicks st) (dcJogadorOuTicks (drop 1 st))


-- | Função auxiliar que recebe uma String e devolve um número inteiro
dcJogadorOuTicks :: String -> Int
dcJogadorOuTicks (h:t) = read [h]

