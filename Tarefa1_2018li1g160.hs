-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g160 where

import LI11819
import Tarefa0_2018li1g160

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [[Move C, Desenha, MudaTetromino, Roda, Move D, Desenha],
            [Desenha, MudaParede, MudaTetromino, MudaTetromino, Move D, Move D, Desenha],
            [MudaTetromino,MudaTetromino,MudaTetromino,Desenha,Move B,Move B, Desenha],
            [Move D, Move B, Move D, Roda, Desenha, Move E, MudaParede, Roda, Move C, Move C, Move B, MudaParede, Roda, MudaParede, MudaTetromino, Roda, MudaTetromino, MudaTetromino, MudaTetromino, Desenha, MudaTetromino, MudaTetromino, MudaTetromino]]

-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
instrucao :: Instrucao -- ^ A 'Instrucao' a aplicar.
          -> Editor    -- ^ O 'Editor' anterior.
          -> Editor    -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.

instrucao (Move C) (Editor (l,c) d e f g) = Editor (l-1,c) d e f g
instrucao (Move B) (Editor (l,c) d e f g) = Editor (l+1,c) d e f g
instrucao (Move E) (Editor (l,c) d e f g) = Editor (l,c-1) d e f g
instrucao (Move D) (Editor (l,c) d e f g) = Editor (l,c+1) d e f g

instrucao Roda (Editor a C b d e) = Editor a D b d e
instrucao Roda (Editor a D b d e) = Editor a B b d e
instrucao Roda (Editor a B b d e) = Editor a E b d e
instrucao Roda (Editor a E b d e) = Editor a C b d e

instrucao MudaTetromino (Editor a b I d e) = Editor a b J d e
instrucao MudaTetromino (Editor a b J d e) = Editor a b L d e
instrucao MudaTetromino (Editor a b L d e) = Editor a b O d e
instrucao MudaTetromino (Editor a b O d e) = Editor a b S d e
instrucao MudaTetromino (Editor a b S d e) = Editor a b T d e
instrucao MudaTetromino (Editor a b T d e) = Editor a b Z d e
instrucao MudaTetromino (Editor a b Z d e) = Editor a b I d e

instrucao MudaParede (Editor a b c Destrutivel e)   = Editor a b c Indestrutivel e
instrucao MudaParede (Editor a b c Indestrutivel e) = Editor a b c Destrutivel e


instrucao Desenha (Editor pos dir tet par mapa) = Editor pos dir tet par (desenhaMapa pos dir tet par mapa)

-- | Cria um novo mapa para substituir no editor

desenhaMapa :: Posicao -> Direcao -> Tetromino -> Parede -> Mapa -> Mapa
desenhaMapa pos dir tet Indestrutivel mapa = decideVaziaOuBlocoI (0,0) (criaMatrizTetromino dir tet) pos mapa
desenhaMapa pos dir tet Destrutivel mapa   = decideVaziaOuBlocoD (0,0) (criaMatrizTetromino dir tet) pos mapa

--Cria um mapa com um tetronimo indestrutivel

-- | Percorre a matriz ate encontrar a linha com o editor, chamando a funçao decideVaziaOuBlocoLinhaI
decideVaziaOuBlocoI :: Posicao -> Matriz Bool -> Posicao -> Mapa -> Mapa
decideVaziaOuBlocoI (_,_) [] (_,_) mapa = mapa
decideVaziaOuBlocoI (xa,ya) (h:t) (xe,ye) (hmap:tmap) | xa < xe = hmap : decideVaziaOuBlocoI (xa+1,0) (h:t) (xe,ye) tmap
                                                      | xa >= xe =  decideVaziaOuBlocoLinhaI ya h ye hmap : decideVaziaOuBlocoI (xa+1,0) t (xe,ye) tmap

-- | Percorre uma linha ate encontrar o editor, depois decide se coloca um bloco ou deixa vazia
decideVaziaOuBlocoLinhaI :: Int -> [Bool] -> Int -> [Peca] -> [Peca]
decideVaziaOuBlocoLinhaI _ [] _ linhamap = linhamap
decideVaziaOuBlocoLinhaI ya (h:t) ye (hmap:tmap) | ya < ye = hmap : decideVaziaOuBlocoLinhaI (ya+1) (h:t) ye tmap
                                                 | ya >= ye && h == True = Bloco Indestrutivel : decideVaziaOuBlocoLinhaI (ya+1) t ye tmap
                                                 | ya >= ye && h == False && hmap /= Vazia = hmap : decideVaziaOuBlocoLinhaI (ya+1) t ye tmap
                                                 | otherwise = Vazia : decideVaziaOuBlocoLinhaI (ya+1) t ye tmap

--Cria um mapa com um tetronimo destrutivel

-- | Percorre a matriz ate encontrar a linha com o editor, chamando a funçao decideVaziaOuBlocoLinhaD
decideVaziaOuBlocoD :: Posicao -> Matriz Bool -> Posicao -> Mapa -> Mapa
decideVaziaOuBlocoD (_,_) [] (_,_) mapa = mapa
decideVaziaOuBlocoD (xa,ya) (h:t) (xe,ye) (hmap:tmap) | xa < xe = hmap : decideVaziaOuBlocoD (xa+1,0) (h:t) (xe,ye) tmap
                                                      | xa >= xe =  decideVaziaOuBlocoLinhaD ya h ye hmap : decideVaziaOuBlocoD (xa+1,0) t (xe,ye) tmap

-- | Percorre uma linha ate encontrar o editor, depois decide se coloca um bloco ou deixa vazia
decideVaziaOuBlocoLinhaD :: Int -> [Bool] -> Int -> [Peca] -> [Peca]
decideVaziaOuBlocoLinhaD _ [] _ linhamap = linhamap
decideVaziaOuBlocoLinhaD ya (h:t) ye (hmap:tmap) | ya < ye = hmap : decideVaziaOuBlocoLinhaD (ya+1) (h:t) ye tmap
                                                 | ya >= ye && h == True = Bloco Destrutivel : decideVaziaOuBlocoLinhaD (ya+1) t ye tmap
                                                 | ya >= ye && h == False && hmap /= Vazia = hmap : decideVaziaOuBlocoLinhaD (ya+1) t ye tmap
                                                 | otherwise = Vazia : decideVaziaOuBlocoLinhaD (ya+1) t ye tmap

-- | Cria uma matriz de bools para cada tetronimo

criaMatrizTetromino :: Direcao -> Tetromino -> Matriz Bool
criaMatrizTetromino C tet = tetrominoParaMatriz tet
criaMatrizTetromino D tet = rodaMatriz (tetrominoParaMatriz tet)
criaMatrizTetromino B tet = rodaMatriz(rodaMatriz (tetrominoParaMatriz tet))
criaMatrizTetromino E tet = rodaMatriz(rodaMatriz(rodaMatriz (tetrominoParaMatriz tet)))


-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
instrucoes :: Instrucoes -- ^ As 'Instrucoes' a aplicar.
           -> Editor     -- ^ O 'Editor' anterior.
           -> Editor     -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.

instrucoes [] (Editor a b c d e)    = Editor a b c d e
instrucoes (h:t) (Editor a b c d e) = instrucoes t (instrucao h (Editor a b c d e))


-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
mapaInicial :: Dimensao -- ^ A 'Dimensao' do 'Mapa' a criar.
            -> Mapa     -- ^ O 'Mapa' resultante com a 'Dimensao' dada.
mapaInicial (x,y) = atualizaMapaInicial (0,0) (criaMatriz (x,y) Vazia)

-- | Coloca blocos indestrutiveis nas bordas do mapa
atualizaMapaInicial :: Posicao -> Mapa -> Mapa
atualizaMapaInicial (x,y) m | x==length m = []
                            | x==0 || x==length m - 1 = atualizaLinha1 (x,y) m : atualizaMapaInicial (x+1,0) m
                            | otherwise = atualizaLinha2 (x,y) m : atualizaMapaInicial (x+1,0) m

-- | Atualiza as bordas na primeira e ultima linha do mapa, onde todas as colunas sao constituidas por blocos indestrutiveis
atualizaLinha1 :: Posicao -> Mapa -> [Peca]
atualizaLinha1 (x,y) m | y==length (head m) = []
                       | otherwise = Bloco Indestrutivel : atualizaLinha1 (x,y+1) m

-- | Atualiza as bordas nas restantes linhas do mapa, onde apenas a primeira e ultima coluna sao constituidaspor blocos indestrutiveis
atualizaLinha2 :: Posicao -> Mapa -> [Peca]
atualizaLinha2 (x,y) m | y==0 || y==length (head m) - 1 = Bloco Indestrutivel : atualizaLinha2 (x,y+1) m
                       | y==length (head m) = []
                       | otherwise = Vazia : atualizaLinha2 (x,y+1) m



-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
editorInicial :: Instrucoes  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
              -> Editor      -- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.

editorInicial is = Editor (posicaoInicial is) C I Indestrutivel (mapaInicial (dimensaoInicial is))

-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
         -> Mapa       -- ^ O 'Mapa' resultante.
constroi is = retribuiMapa (instrucoes is (editorInicial is))

-- | Auxiliar que retribui o mapa de um editor
retribuiMapa :: Editor -> Mapa
retribuiMapa (Editor _ _ _ _ mapa) = mapa





