-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g160 where

import LI11819
import Tarefa0_2018li1g160
import Tarefa1_2018li1g160

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0,Movimenta C,Estado (mapaInicial (6,6)) [Jogador (1,1) C 1 1 1, Jogador (3,3) B 1 1 1] []),     --testa choque entre tanque e bloco indestrutivel
            
            (1,Movimenta C,Estado (mapaInicial (6,6)) [Jogador (1,1) C 1 1 1, Jogador (3,2) C 1 1 1] []),     -- testa choque entre dois tanques
            
            (1,Movimenta E,Estado (mapaInicial (6,6)) [Jogador (1,1) C 1 1 1, Jogador (3,2) E 1 1 1] []),    --testa uma movimentaçao normal, embora os dois tanques se toquem
            
            (0,Movimenta B,Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1, Jogador (3,2) C 1 1 1] []),     --testa um choque entre tanques

            (1,Movimenta C,Estado (mapaInicial (6,6)) [Jogador (1,1) C 1 1 1, Jogador (3,2) E 1 1 1] []),     -- testa uma rotaçao de um tanque

            (1,Movimenta C,Estado (mapaInicial (8,8)) [Jogador (1,1) C 1 1 1, Jogador (5,5) C 1 1 1, Jogador (2,4) C 1 1 1] [DisparoChoque 0 1]),    -- testa um movimento quando um choque esta ativo mas nao afeta o jogador

            (2,Movimenta C,Estado (mapaInicial (8,8)) [Jogador (1,1) C 1 1 1, Jogador (5,5) C 1 1 1, Jogador (2,4) C 1 1 1] [DisparoCanhao 0 (2,2) E, DisparoChoque 0 1]),    -- testa um movimento sob o efeito de um choque

            (2,Movimenta E,Estado (mapaInicial (8,8)) [Jogador (1,1) C 1 1 1, Jogador (5,5) C 1 1 1, Jogador (2,4) C 1 1 1] [DisparoChoque 0 1]),     -- testa uma rotaçao sob o efeito de um choque
            
            (0,Movimenta B, Estado (mapaInicial (8,8)) [Jogador (1,1) B 1 1 1] [DisparoChoque 0 1]),      --testa um movimento se o proprio jogador tiver ativado um choque
            
            (0, Dispara Canhao, Estado (mapaInicial (6,6)) [Jogador (1,1) D 1 1 1] []),  --testa disparo canhao
         
            (1, Dispara Laser, Estado (mapaInicial (6,6)) [Jogador (1,1) D 1 1 1, Jogador (3,3) C 1 1 1] []),  --testa disparo laser, quando o jogador tem lasers
         
            (0, Dispara Laser, Estado (mapaInicial (6,6)) [Jogador (1,1) D 1 0 1] []),  --testa disparo laser, quando o jogador nao tem lasers
         
            (1, Dispara Choque, Estado (mapaInicial (6,6)) [Jogador (1,1) D 1 1 1, Jogador (3,3) D 1 1 1] []),  --testa disparo choque, quando o jogador tem choques
         
            (0, Dispara Choque, Estado (mapaInicial (6,6)) [Jogador (1,1) D 1 1 0] []),  --testa disparo laser, quando o jogador nao tem choques
         
            (0, Dispara Canhao, Estado (mapaInicial (6,6)) [Jogador (1,1) E 1 1 1] []),  --testa disparo canhao, quando o cano do jogador se encontra encostado a uma parede
         
            (0, Movimenta C, Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 1 1, Jogador (3,3) B 1 1 1] []),   --testa quando um tanque se move paralelo ao outro, na vertical

            (0, Dispara Canhao, Estado (mapaInicial (6,6)) [Jogador (3,3) C 1 1 1, Jogador (1,1) D 1 1 1] [DisparoChoque 1 3]),    --testa disparo canhao, quando o jogador esta afetado por um choque

            (0,Movimenta D, Estado (mapaInicial (6,6)) [Jogador (1,1) D 1 1 1] []),   --testa um movimento para a direita

            (0,Movimenta B, Estado (mapaInicial (6,6)) [Jogador (1,1) B 0 1 1] []),   --testa um movimento quando o jogador tem 0 vidas

            (0, Movimenta D, Estado (mapaInicial (6,6)) [Jogador (1,3) D 1 1 1] []),   --testa um movimento com um bloco à sua direita

            (0, Movimenta D, Estado (mapaInicial (6,6)) [Jogador (1,1) D 1 1 1, Jogador (3,1) E 1 1 1] []),  --testa um movimento possivel para a direita, quando existe outro jogador no mapa

            (0, Movimenta B, Estado (mapaInicial (6,6)) [Jogador (1,1) B 1 1 1, Jogador (1,3) E 1 1 1] []),  --testa um movimento possivel para baixo, quando existe outro jogador no mapa

            (0, Movimenta E, Estado (mapaInicial (6,6)) [Jogador (1,3) E 1 1 1, Jogador (3,1) E 1 1 1] []),  --testa um movimento possivel para a esquerda, quando existe outro jogador no mapa

            (0, Dispara Canhao, Estado (mapaInicial (6,6)) [Jogador (1,1) D 0 1 1] [])]  --testa um disparo quando o jogador tem 0 vidas






-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.

--Jogada Movimenta
jogada ind (Movimenta dir) (Estado mapa jogs dis) | verificaIndice ind jogs = movimenta ind dir (escolheJogador ind jogs) (Estado mapa jogs dis)
                                                  | otherwise = Estado mapa jogs dis

jogada ind (Dispara arma) (Estado mapa jogs dis) | verificaIndice ind jogs = dispara ind arma (escolheJogador ind jogs) (Estado mapa jogs dis)
                                                 | otherwise = Estado mapa jogs dis

{-
jogada ind (Movimenta C) (Estado mapa jogadores disparos) = movimenta ind C (escolheJogador ind jogadores) (Estado mapa jogadores disparos)
jogada ind (Movimenta D) (Estado mapa jogadores disparos) = movimenta ind D (escolheJogador ind jogadores) (Estado mapa jogadores disparos)
jogada ind (Movimenta B) (Estado mapa jogadores disparos) = movimenta ind B (escolheJogador ind jogadores) (Estado mapa jogadores disparos)
jogada ind (Movimenta E) (Estado mapa jogadores disparos) = movimenta ind E (escolheJogador ind jogadores) (Estado mapa jogadores disparos)

jogada ind (Dispara Canhao) (Estado mapa jogadores disparos) = dispara ind Canhao (escolheJogador ind jogadores) (Estado mapa jogadores disparos)
jogada ind (Dispara Laser) (Estado mapa jogadores disparos) = dispara ind Laser (escolheJogador ind jogadores) (Estado mapa jogadores disparos)
jogada ind (Dispara Choque) (Estado mapa jogadores disparos) = dispara ind Choque (escolheJogador ind jogadores) (Estado mapa jogadores disparos)

jogada ind j (Estado mapa jogs dis) = if (verificaIndice ind jogs) then (jogada ind j (Estado mapa jogs dis)) else (Estado mapa jogs dis)
-}

-- | Verifica se o índice recebido existe na lista de jogadores do estado
verificaIndice :: Int -> [Jogador] -> Bool
verificaIndice _ [] = False
verificaIndice 0 l = True
verificaIndice x (h:t) = verificaIndice (x-1) t



-- | Executa o movimento
movimenta :: Int -> Direcao -> Jogador -> Estado -> Estado
movimenta _ _ (Jogador (_,_) _ 0 _ _) estado = estado    --Não efetua a jogada quando o jogador tem 0 vidas

movimenta ind dirmov (Jogador (x,y) dir _ _ _) (Estado mapa jogadores disparos) | dirmov == dir && verificaSePodeMover dirmov (x,y) mapa jogadores disparos = Estado mapa (aplicaMovimentoAoEstado ind dirmov jogadores) disparos    --Aplica o movimento, verificando se este é possivel
                                                                                | otherwise = Estado mapa (aplicaRotacaoAoEstado ind dirmov jogadores) disparos     --Apenas muda a direçao quando o tanque nao esta virado para onde se quer mover

-- | Aplica a jogada ao estado quando o jogador se move numa direçao
aplicaMovimentoAoEstado :: Int -> Direcao -> [Jogador] -> [Jogador]
aplicaMovimentoAoEstado 0 dirmov (Jogador (x,y) _ vidas lasers choques : js) = Jogador (somaVetores (x,y) (direcaoParaVetor dirmov)) dirmov vidas lasers choques : js
aplicaMovimentoAoEstado ind dirmov (ja:js) = ja : aplicaMovimentoAoEstado (ind-1) dirmov js

-- | Aplica a jogada ao estado quando o jogador apenas muda de direçao
aplicaRotacaoAoEstado :: Int -> Direcao -> [Jogador] -> [Jogador]
aplicaRotacaoAoEstado 0 dirmov (Jogador (x,y) _ vidas lasers choques : js) = Jogador (x,y) dirmov vidas lasers choques : js
aplicaRotacaoAoEstado ind dirmov (ja:js) = ja : aplicaRotacaoAoEstado (ind-1) dirmov js

-- | Verifica se o movimento é valido
verificaSePodeMover :: Direcao -> Posicao -> Mapa -> [Jogador] -> [Disparo] -> Bool
verificaSePodeMover dir pos mapa js dis = verificaBloco dir pos mapa && verificaAdversario dir pos js && verificaChoque pos dis js

-- | Verifica se os dois blocos para onde se quer movimentar estao vazios
verificaBloco :: Direcao -> Posicao -> Mapa -> Bool
verificaBloco C (x,y) mapa = encontraPosicaoMatriz (x-1,y) mapa == Vazia && encontraPosicaoMatriz (x-1,y+1) mapa == Vazia
verificaBloco D (x,y) mapa = encontraPosicaoMatriz (x,y+2) mapa == Vazia && encontraPosicaoMatriz (x+1,y+2) mapa == Vazia
verificaBloco B (x,y) mapa = encontraPosicaoMatriz (x+2,y) mapa == Vazia && encontraPosicaoMatriz (x+2,y+1) mapa == Vazia
verificaBloco E (x,y) mapa = encontraPosicaoMatriz (x,y-1) mapa == Vazia && encontraPosicaoMatriz (x+1,y-1) mapa == Vazia

-- | Verfica se os dois blocos para onde se quer movimentar nao se encontram ocupados por outro jogador
verificaAdversario :: Direcao -> Posicao -> [Jogador] -> Bool
verificaAdversario _ (_,_) [] = True                 --Concluiu que o tanque nao tem nenhum adversario no caminho

verificaAdversario dir (xj,yj) (Jogador (_,_) _ 0 _ _:js) = verificaAdversario dir (xj,yj) js    --Ignora um jogador quando este nao tem vidas

verificaAdversario C (xj,yj) (Jogador (x,y) _ _ _ _:js) | xj==x && yj==y = verificaAdversario C (xj,yj) js            --Quando esta a comparar a posiçao do proprio jogador
                                                        | (yj-y > -2 && yj-y < 2) && (xj-x == 2) = False              --Se um jogador estiver a ocupar uma posiçao acima
                                                        | otherwise = verificaAdversario C (xj,yj) js                 --Continua a verificar para os outros jogadores

verificaAdversario D (xj,yj) (Jogador (x,y) _ _ _ _:js) | xj==x && yj==y = verificaAdversario D (xj,yj) js            --Quando esta a comparar a posiçao do proprio jogador
                                                        | (xj-x > -2 && xj-x < 2) && (yj-y == -2) = False             --Se um jogador estiver a ocupar uma posiçao à direita
                                                        | otherwise = verificaAdversario D (xj,yj) js                 --Continua a verificar para os outros jogadores

verificaAdversario B (xj,yj) (Jogador (x,y) _ _ _ _:js) | xj==x && yj==y = verificaAdversario B (xj,yj) js            --Quando esta a comparar a posiçao do proprio jogador
                                                        | (yj-y > -2 && yj-y < 2) && (xj-x == -2) = False             --Se um jogador estiver a ocupar uma posiçao abaixo
                                                        | otherwise = verificaAdversario B (xj,yj) js                 --Continua a verificar para os outros jogadores

verificaAdversario E (xj,yj) (Jogador (x,y) _ _ _ _:js) | xj==x && yj==y = verificaAdversario E (xj,yj) js            --Quando esta a comparar a posiçao do proprio jogador
                                                        | (xj-x > -2 && xj-x < 2) && (yj-y == 2) = False              --Se um jogador estiver a ocupar uma posiçao à esquerda
                                                        | otherwise = verificaAdversario E (xj,yj) js                 --Continua a verificar para os outros jogadores

-- | Verifica se um dos 4 blocos do tanque esta afetado por um choque adversario
verificaChoque :: Posicao -> [Disparo] -> [Jogador] -> Bool
verificaChoque (_,_) [] _ = True                        --Conclui que o tanque nao esta sob o alcance de um choque adversario
verificaChoque (xj,yj) (DisparoChoque ind _:ds) js | xj==x && yj==y = verificaChoque (xj,yj) ds js                   --Quando esta a comparar a posiçao do proprio jogador
                                                   | (xj-x > -4 && xj-x < 4) && (yj-y > -4 && yj-y < 4) = False      --Se o jogador estiver no alcance de um choque adversario
                                                   | otherwise = verificaChoque (xj,yj) ds js                        --Continua a verificar para os outros jogadores
                                                   where (x,y) = retribuiPosicaoJogador (escolheJogador ind js)

verificaChoque (x,y) (_:ds) js = verificaChoque (x,y) ds js    --Procura um DisparoChoque na lista de disparos do Estado

-- | Retrubui a posiçao de um jogador
retribuiPosicaoJogador :: Jogador -> Posicao
retribuiPosicaoJogador (Jogador (x,y) _ _ _ _) = (x,y)

-- | Escolhe o jogador que efetuou a jogada
escolheJogador :: Int -> [Jogador] -> Jogador
escolheJogador 0 (ja:_)   = ja
escolheJogador ind (_:js) = escolheJogador (ind-1) js


--Jogada Dispara


-- | Executa o disparo
dispara :: Int -> Arma -> Jogador -> Estado -> Estado
dispara _ _ (Jogador (_,_) _ 0 _ _) estado = estado          --Nao efetua a jogada quando o jogador tem 0 vidas

dispara ind Canhao (Jogador (xj,yj) dir _ _ _) (Estado mapa jogadores disparos) = Estado mapa jogadores (DisparoCanhao ind (somaVetores (xj,yj) (direcaoParaVetor dir)) dir : disparos)

dispara ind Laser (Jogador (xj,yj) dir _ lasers _) (Estado mapa jogadores disparos) | lasers == 0 = Estado mapa jogadores disparos
                                                                                    | otherwise = Estado mapa (removeLaser ind jogadores) (DisparoLaser ind (somaVetores (xj,yj) (direcaoParaVetor dir)) dir : disparos)


dispara ind Choque (Jogador (_,_) _ _ _ choques) (Estado mapa jogadores disparos) | choques == 0 = Estado mapa jogadores disparos
                                                                                  | otherwise = Estado mapa (removeChoque ind jogadores) (DisparoChoque ind 5 : disparos)


-- | Substrai uma muniçao laser ao estado, apos o disparo
removeLaser :: Int -> [Jogador] -> [Jogador]
removeLaser 0 (Jogador (x,y) dir vidas lasers choques:js) = Jogador (x,y) dir vidas (lasers-1) choques : js
removeLaser ind (ja:js) = ja : removeLaser (ind-1) js

-- | Substrai uma muniçao choque ao estado, apos o disparo
removeChoque :: Int -> [Jogador] -> [Jogador]
removeChoque 0 (Jogador (x,y) dir vidas lasers choques:js) = Jogador (x,y) dir vidas lasers (choques-1) : js
removeChoque ind (ja:js) = ja : removeChoque (ind-1) js

