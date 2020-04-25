{- | Module              : Tarefa6_2018li1g160
   | Escola              : Universidade Do Minho
   | Description         : Implementação de um 'bot' capaz de jogar o jogo automaticamente
   | Copyright           : Bruno Filipe de Sousa Dias <a89583@alunos.uminho.pt>
                           Luís Enes Sousa <a89597@alunos.uminho.pt>
= Introdução Tarefa 6:
  Este módulo consiste numa função (bot) que abrange os vários casos aos quais o bot pode ser sujeito, aplicando a ação correta em
cada um destes casos.

= Objetivos e estratégias utilizadas:
  A nossa estratégia consistiu em pensar nas várias situações em que o bot pode encontrar-se. Demos prioridade às jogadas defensivas,
isto é, o bot verifica primeiro se está em perigo de perder uma vida. Caso alguma destas situações se verifique, este executa uma
jogada específica, de forma a evitar ser atingido. Caso contrário, procede para os movimentos ofensivos.
  Os movimentos ofensivos funcionam da mesma forma que os defensivos. Verificam-se certas condições que, caso se verifiquem, executam
uma jogada correspondente.
  Caso nenhuma destas jogadas "especiais" seja executada, o bot procura um Bloco Destrutível por perto, e tenta destruí-lo. Se, mais
uma vez, isto não se verificar, o bot verifica as suas possibilidades de movimento, executando uma delas. Se estas forem nulas, acaba
por executar um DisparoCanhao.

= Conclusão
  Concluindo, partilhámos da opinião que o bot poderia estar mais aprimorado, mas que este tem um desempenho satisfatório.
-}

module Tarefa6_2018li1g160 where

import LI11819
import Tarefa0_2018li1g160
import Tarefa1_2018li1g160
import Tarefa2_2018li1g160
import Tarefa4_2018li1g160
import Data.List

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot 0 (Estado m j d) = decideJogada 0 (escolheJogador 0 j) (Estado m j d)
bot 1 (Estado m j d) = decideJogada 1 (escolheJogador 1 j) (Estado m j d)
bot 2 (Estado m j d) = decideJogada 2 (escolheJogador 2 j) (Estado m j d)
bot 3 (Estado m j d) = decideJogada 3 (escolheJogador 3 j) (Estado m j d)



-- | Recebe o jogador que quer efetuar a jogada (bot) e o estado atual do jogo, e decide uma jogada
decideJogada :: Int -> Jogador -> Estado -> Maybe Jogada
decideJogada i (Jogador p d v l c) (Estado mapa jogs dis) | v == 0 = Nothing
                                                          | verificaDisparoLaserNaPropriaPosicao p (selecionaLasers dis) = Nothing
                                                          | a /= Nothing = a
                                                          | otherwise = executaJogadaOfensiva i jogador estado
                                                          where a = executaJogadaDefensiva jogador estado
                                                                jogador = Jogador p d v l c
                                                                estado = Estado mapa jogs dis



-- | Executa uma jogada ofensiva
executaJogadaOfensiva :: Int -> Jogador -> Estado -> Maybe Jogada
executaJogadaOfensiva i jogador estado | jogadaOfensiva1 i jogador estado = executaJogadaOfensiva1 jogador estado
                                       | jogadaOfensiva2 i jogador estado = Just (Dispara Choque)
                                       | jogadaOfensiva3 jogador estado /= Nothing = jogadaOfensiva3 jogador estado
                                       | otherwise = jogadaOfensivaFinal jogador estado


-- | Se tiver um DisparoChoque ativo (apenas executa um DisparoChoque se tiver um adversario no alcance, logo tem um adversario retido no Choque), vai ao encontro do adversario e DisparaCanhao
jogadaOfensiva1 :: Int -> Jogador -> Estado -> Bool
jogadaOfensiva1 i jogador (Estado m j d) = verificaChoqueProprio i d


-- | Recebe a lista de disparos do estado atual e verifica se contem algum DisparoChoque do bot
verificaChoqueProprio :: Int -> [Disparo] -> Bool
verificaChoqueProprio _ [] = False
verificaChoqueProprio ind (DisparoChoque i _ : ds) | i == ind = True
                                                   | otherwise = verificaChoqueProprio ind ds
verificaChoqueProprio ind (_:ds) = verificaChoqueProprio ind ds


-- | Executa a Jogada 1
executaJogadaOfensiva1 :: Jogador -> Estado -> Maybe Jogada
executaJogadaOfensiva1 (Jogador (x,y) C vid las cho) (Estado mapa jogs dis) | (x-x1 == 3 || x-x1 == 2) && (y-y1 > -2 && y-y1 < 2) = Just (Dispara Canhao)
                                                                            | otherwise = movimentaAteAdversario (Jogador (x,y) C vid las cho) (Jogador (x1,y1) d1 v1 l1 c1)
                                                                            where Jogador (x1,y1) d1 v1 l1 c1 = escolheJogador (encontraAdversarioNoChoque (0,0,1000) (x,y) jogs) jogs

executaJogadaOfensiva1 (Jogador (x,y) D vid las cho) (Estado mapa jogs dis) | (y-y1 == -3 || y-y1 == -2) && (x-x1 > -2 && x-x1 < 2) = Just (Dispara Canhao)
                                                                            | otherwise = movimentaAteAdversario (Jogador (x,y) D vid las cho) (Jogador (x1,y1) d1 v1 l1 c1)
                                                                            where Jogador (x1,y1) d1 v1 l1 c1 = escolheJogador (encontraAdversarioNoChoque (0,0,1000) (x,y) jogs) jogs

executaJogadaOfensiva1 (Jogador (x,y) B vid las cho) (Estado mapa jogs dis) | (x-x1 == -3 || x-x1 == -2) && (y-y1 > -2 && y-y1 < 2) = Just (Dispara Canhao)
                                                                            | otherwise = movimentaAteAdversario (Jogador (x,y) B vid las cho) (Jogador (x1,y1) d1 v1 l1 c1)
                                                                            where Jogador (x1,y1) d1 v1 l1 c1 = escolheJogador (encontraAdversarioNoChoque (0,0,1000) (x,y) jogs) jogs

executaJogadaOfensiva1 (Jogador (x,y) E vid las cho) (Estado mapa jogs dis) | (y-y1 == 3 || y-y1 == 2) && (x-x1 > -2 && x-x1 < 2) = Just (Dispara Canhao)
                                                                            | otherwise = movimentaAteAdversario (Jogador (x,y) E vid las cho) (Jogador (x1,y1) d1 v1 l1 c1)
                                                                            where Jogador (x1,y1) d1 v1 l1 c1 = escolheJogador (encontraAdversarioNoChoque (0,0,1000) (x,y) jogs) jogs



-- | Desloca-se ate estar alinhado com um adversario que esteja sob o efeito do seu choque
movimentaAteAdversario :: Jogador -> Jogador -> Maybe Jogada
movimentaAteAdversario (Jogador (xb,yb) C _ _ _) (Jogador (x,y) _ _ _ _) | xb-x < -1 = Just (Movimenta B)
                                                                         | xb-x > 1 = Just (Movimenta C)
                                                                         | yb-y > 1 = Just (Movimenta E)
                                                                         | otherwise = Just (Movimenta D)

movimentaAteAdversario (Jogador (xb,yb) D _ _ _) (Jogador (x,y) _ _ _ _) | yb-y < -1 = Just (Movimenta D)
                                                                         | yb-y > 1 = Just (Movimenta E)
                                                                         | xb-x > 1 = Just (Movimenta C)
                                                                         | otherwise = Just (Movimenta B)

movimentaAteAdversario (Jogador (xb,yb) B _ _ _) (Jogador (x,y) _ _ _ _) | xb-x < -1 = Just (Movimenta B)
                                                                         | xb-x > 1 = Just (Movimenta C)
                                                                         | yb-y > 1 = Just (Movimenta E)
                                                                         | otherwise = Just (Movimenta D)

movimentaAteAdversario (Jogador (xb,yb) E _ _ _) (Jogador (x,y) _ _ _ _) | yb-y < -1 = Just (Movimenta D)
                                                                         | yb-y > 1 = Just (Movimenta E)
                                                                         | xb-x > 1 = Just (Movimenta C)
                                                                         | otherwise = Just (Movimenta B)



-- | Encontra o adversario que esta afetado pelo choque
encontraAdversarioNoChoque :: (Int,Int,Float) -> PosicaoGrelha -> [Jogador] -> Int     --(indice do jogador a ser analisado, indice do jogador mais perto, distancia do jogador mais perto)
encontraAdversarioNoChoque (a,b,c) _ [] = b
encontraAdversarioNoChoque (a,b,c) (xb,yb) ((Jogador _ _ 0 _ _) : js) = encontraAdversarioNoChoque (a+1,b,c) (xb,yb) js
encontraAdversarioNoChoque (a,b,c) (xb,yb) ((Jogador (x,y) _ _ _ _) : js) | xb == x && yb == y = encontraAdversarioNoChoque (a+1,b,c) (xb,yb) js
                                                                          | dist < c = encontraAdversarioNoChoque (a+1,a,dist) (xb,yb) js
                                                                          | otherwise = encontraAdversarioNoChoque (a+1,b,c) (xb,yb) js
                                                                          where dist = distanciaEntreTanques (xb,yb) (x,y)


-- | Calcula a distancia entre dois tanques
distanciaEntreTanques :: PosicaoGrelha -> PosicaoGrelha -> Float
distanciaEntreTanques (x,y) (z,w) = sqrt (fromIntegral ((x1*x1) + (y1*y1)))
                                  where x1 = x-z
                                        y1 = y-w






-- | Se tiver um adversario no alcance de um Disparo Choque e tiver choques disponiveis, efetua um DisparoChoque
jogadaOfensiva2 :: Int -> Jogador -> Estado -> Bool
jogadaOfensiva2 i (Jogador _ _ _ _ 0) _ = False
jogadaOfensiva2 _ _ (Estado _ [] _) = False
jogadaOfensiva2 i (Jogador pos d v l c) (Estado mapa jogadores disparos) = verificaAdversarioNoAlcance pos js
                                                                         where js = delete (escolheJogador i jogadores) jogadores


-- | Verifica se algum adversario se encontra no alcance de um DisparoChoque
verificaAdversarioNoAlcance :: PosicaoGrelha -> [Jogador] -> Bool
verificaAdversarioNoAlcance _ [] = False
verificaAdversarioNoAlcance (xb,yb) (Jogador _ _ 0 _ _ : js) = verificaAdversarioNoAlcance (xb,yb) js
verificaAdversarioNoAlcance (xb,yb) (Jogador (x,y) _ _ _ _ : js) | abs(xb-x) < 4 && abs(yb-y) < 4 = True
                                                                 | otherwise = verificaAdversarioNoAlcance (xb,yb) js






-- | Se um adversario estiver alinhado com o canhao do bot e nao tiver nenhum Bloco Indestrutivel no caminho, efetua um DisparoLaser, caso nao tenha lasers disponiveis Dispara Canhao
jogadaOfensiva3 :: Jogador -> Estado -> Maybe Jogada
jogadaOfensiva3 (Jogador p d _ l _) (Estado mapa jogadores disparos) | a && verificaMapaJogadaOfensiva3 p pa d mapa && l /= 0 = Just (Dispara Laser)
                                                                     | a && verificaMapaJogadaOfensiva3 p pa d mapa = Just (Dispara Canhao)
                                                                     | otherwise = Nothing
                                                                     where (a,pa) = encontraJogadorJogadaOfensiva3 p d jogadores


-- | Verifica se o jogador acabou de efetuar um DisparoLaser
verificaDisparoLaserNaPropriaPosicao :: PosicaoGrelha -> [Disparo] -> Bool
verificaDisparoLaserNaPropriaPosicao _ [] = False
verificaDisparoLaserNaPropriaPosicao (xb,yb) (DisparoLaser _ (x,y) C : ds) = (xb == x+1 && yb == y) || verificaDisparoLaserNaPropriaPosicao (xb,yb) ds
verificaDisparoLaserNaPropriaPosicao (xb,yb) (DisparoLaser _ (x,y) D : ds) = (xb == x && yb == y-1) || verificaDisparoLaserNaPropriaPosicao (xb,yb) ds
verificaDisparoLaserNaPropriaPosicao (xb,yb) (DisparoLaser _ (x,y) B : ds) = (xb == x-1 && yb == y) || verificaDisparoLaserNaPropriaPosicao (xb,yb) ds
verificaDisparoLaserNaPropriaPosicao (xb,yb) (DisparoLaser _ (x,y) E : ds) = (xb == x && yb == y+1) || verificaDisparoLaserNaPropriaPosicao (xb,yb) ds



-- | Procura um adversario alinhado com o cano do bot
encontraJogadorJogadaOfensiva3 :: PosicaoGrelha -> Direcao -> [Jogador] -> (Bool,PosicaoGrelha)
encontraJogadorJogadaOfensiva3 _ _ [] = (False,(0,0))
encontraJogadorJogadaOfensiva3 (xb,yb) dir (Jogador _ _ 0 _ _ : js) = encontraJogadorJogadaOfensiva3 (xb,yb) dir js
encontraJogadorJogadaOfensiva3 (xb,yb) C (Jogador (x,y) d _ _ _ : js) | (xb,yb) == (x,y) = encontraJogadorJogadaOfensiva3 (xb,yb) C js
                                                                      | yb == y && xb > x = (True,(x,y))
                                                                      | otherwise = encontraJogadorJogadaOfensiva3 (xb,yb) C js

encontraJogadorJogadaOfensiva3 (xb,yb) D (Jogador (x,y) d _ _ _ : js) | (xb,yb) == (x,y) = encontraJogadorJogadaOfensiva3 (xb,yb) D js
                                                                      | xb == x && yb < y = (True,(x,y))
                                                                      | otherwise = encontraJogadorJogadaOfensiva3 (xb,yb) D js

encontraJogadorJogadaOfensiva3 (xb,yb) B (Jogador (x,y) d _ _ _ : js) | (xb,yb) == (x,y) = encontraJogadorJogadaOfensiva3 (xb,yb) B js
                                                                      | yb == y && xb < x = (True,(x,y))
                                                                      | otherwise = encontraJogadorJogadaOfensiva3 (xb,yb) B js

encontraJogadorJogadaOfensiva3 (xb,yb) E (Jogador (x,y) d _ _ _ : js) | (xb,yb) == (x,y) = encontraJogadorJogadaOfensiva3 (xb,yb) E js
                                                                      | xb == x && yb > y = (True,(x,y))
                                                                      | otherwise = encontraJogadorJogadaOfensiva3 (xb,yb) E js


-- | Verifica se o caminho entre o bot e o adversario alinhado nao tem nenhum Bloco Indestrutivel
verificaMapaJogadaOfensiva3 :: PosicaoGrelha -> PosicaoGrelha -> Direcao -> Mapa -> Bool
verificaMapaJogadaOfensiva3 (x,y) (a,b) C  mapa | (x,y) == (a,b) = True
                                                | bloco1 == Bloco Indestrutivel || bloco2 == Bloco Indestrutivel = False
                                                | otherwise = verificaMapaJogadaOfensiva3 (x-1,y) (a,b) C mapa
                                                where bloco1 = encontraPosicaoMatriz (x-1,y) mapa
                                                      bloco2 = encontraPosicaoMatriz (x-1,y+1) mapa

verificaMapaJogadaOfensiva3 (x,y) (a,b) D  mapa | (x,y) == (a,b) = True
                                                | bloco1 == Bloco Indestrutivel || bloco2 == Bloco Indestrutivel = False
                                                | otherwise = verificaMapaJogadaOfensiva3 (x,y+1) (a,b) D mapa
                                                where bloco1 = encontraPosicaoMatriz (x,y+2) mapa
                                                      bloco2 = encontraPosicaoMatriz (x+1,y+2) mapa

verificaMapaJogadaOfensiva3 (x,y) (a,b) B  mapa | (x,y) == (a,b) = True
                                                | bloco1 == Bloco Indestrutivel || bloco2 == Bloco Indestrutivel = False
                                                | otherwise = verificaMapaJogadaOfensiva3 (x+1,y) (a,b) B mapa
                                                where bloco1 = encontraPosicaoMatriz (x+2,y) mapa
                                                      bloco2 = encontraPosicaoMatriz (x+2,y+1) mapa

verificaMapaJogadaOfensiva3 (x,y) (a,b) E  mapa | (x,y) == (a,b) = True
                                                | bloco1 == Bloco Indestrutivel || bloco2 == Bloco Indestrutivel = False
                                                | otherwise = verificaMapaJogadaOfensiva3 (x,y-1) (a,b) E mapa
                                                where bloco1 = encontraPosicaoMatriz (x,y-1) mapa
                                                      bloco2 = encontraPosicaoMatriz (x+1,y-1) mapa


{-}

-- | Se tiver um adversario alinhado com o cano e perto, Dispara Canhao
jogadaOfensiva4 :: Jogador -> Estado -> Bool
jogadaOfensiva4 (Jogador p d _ _ _) (Estado mapa jogs dis) = verificaJogadaOfensiva4 p d jogs


-- | Verifica se deve executar a jogada ofensiva 4
verificaJogadaOfensiva4 :: PosicaoGrelha -> Direcao -> [Jogador] -> Bool
verificaJogadaOfensiva4 _ _ [] = False
verificaJogadaOfensiva4 (xb,yb) C (Jogador (x,y) dir _ _ _ : js) | xb-x == 2 && abs(yb-y) < 2 = True
                                                                 | xb-x == 3 && yb == y = True
                                                                 | otherwise = verificaJogadaOfensiva4 (xb,yb) C js

verificaJogadaOfensiva4 (xb,yb) D (Jogador (x,y) dir _ _ _ : js) | yb-y == -2 && abs(xb-x) < 2 = True
                                                                 | yb-y == -3 && xb == x = True
                                                                 | otherwise = verificaJogadaOfensiva4 (xb,yb) D js

verificaJogadaOfensiva4 (xb,yb) B (Jogador (x,y) dir _ _ _ : js) | xb-x == -2 && abs(yb-y) < 2 = True
                                                                 | xb-x == -3 && yb == y = True
                                                                 | otherwise = verificaJogadaOfensiva4 (xb,yb) B js

verificaJogadaOfensiva4 (xb,yb) E (Jogador (x,y) dir _ _ _ : js) | yb-y == 2 && abs(xb-x) < 2 = True
                                                                 | yb-y == 3 && xb == x = True
                                                                 | otherwise = verificaJogadaOfensiva4 (xb,yb) E js
-}





-- | Executa esta jogada, depois de verificar todas as outras jogadas "especiais"
jogadaOfensivaFinal :: Jogador -> Estado -> Maybe Jogada
jogadaOfensivaFinal (Jogador p d _ _ _) (Estado mapa jogs dis) | blocoDesPorPerto p d mapa /= Nothing = blocoDesPorPerto p d mapa
                                                               | otherwise = decideSeMovimenta [C,D,E,B] p d (Estado mapa jogs dis)



-- | Decide se executa um movimento ou fica parado
decideSeMovimenta :: [Direcao] -> PosicaoGrelha -> Direcao -> Estado -> Maybe Jogada
decideSeMovimenta l pos dir (Estado mapa jogs dis) | l4 == [] = Just (Dispara Canhao)
                                                   | otherwise = decideParaOndeMovimenta l4 dir
                                                   where l2 = decideSeMovimentaMapa l pos mapa
                                                         l3 = decideSeMovimentaDisparosChoque l2 pos (selecionaChoques dis) jogs
                                                         l4 = decideSeMovimentaDisparos l3 pos dis mapa


-- | Recebe as varias possibilidades de movimento e decide qual executa
decideParaOndeMovimenta :: [Direcao] -> Direcao -> Maybe Jogada
decideParaOndeMovimenta [x] _ = Just (Movimenta x)
decideParaOndeMovimenta l C = if elem C l then Just (Movimenta C) else decideParaOndeMovimenta (delete B l) D
decideParaOndeMovimenta l D = if elem D l then Just (Movimenta D) else decideParaOndeMovimenta (delete E l) B
decideParaOndeMovimenta l B = if elem B l then Just (Movimenta B) else decideParaOndeMovimenta (delete C l) E
decideParaOndeMovimenta l E = if elem E l then Just (Movimenta E) else decideParaOndeMovimenta (delete D l) C






-- | Verifica se existe um Bloco Destrutivel por perto
blocoDesPorPerto :: PosicaoGrelha -> Direcao -> Mapa -> Maybe Jogada
blocoDesPorPerto pos dir mapa | verificaBlocoDesC pos mapa && dir == C = Just (Dispara Canhao)
                              | verificaBlocoDesC pos mapa = Just (Movimenta C)
                              | verificaBlocoDesD pos mapa && dir == D = Just (Dispara Canhao)
                              | verificaBlocoDesD pos mapa = Just (Movimenta D)
                              | verificaBlocoDesB pos mapa && dir == B = Just (Dispara Canhao)
                              | verificaBlocoDesB pos mapa = Just (Movimenta B)
                              | verificaBlocoDesE pos mapa && dir == E = Just (Dispara Canhao)
                              | verificaBlocoDesE pos mapa = Just (Movimenta E)
                              | otherwise = Nothing


-- | Verifica se tem um Bloco Destrutivel em cima do tanque
verificaBlocoDesC :: PosicaoGrelha -> Mapa -> Bool
verificaBlocoDesC (x,y) mapa = blococimaesquerda == Bloco Destrutivel || blococimadireita == Bloco Destrutivel
                             where blococimaesquerda = encontraPosicaoMatriz (x-1,y) mapa
                                   blococimadireita = encontraPosicaoMatriz (x-1,y+1) mapa

-- | Verifica se tem um Bloco Destrutivel à direita do tanque
verificaBlocoDesD :: PosicaoGrelha -> Mapa -> Bool
verificaBlocoDesD (x,y) mapa = blocodireitacima == Bloco Destrutivel || blocodireitabaixo == Bloco Destrutivel
                             where blocodireitacima = encontraPosicaoMatriz (x,y+2) mapa
                                   blocodireitabaixo = encontraPosicaoMatriz (x+1,y+2) mapa

-- | Verifica se tem um Bloco Destrutivel a baixo do tanque
verificaBlocoDesB :: PosicaoGrelha -> Mapa -> Bool
verificaBlocoDesB (x,y) mapa = blocobaixoesquerda == Bloco Destrutivel || blocobaixodireita == Bloco Destrutivel
                             where blocobaixoesquerda = encontraPosicaoMatriz (x+2,y) mapa
                                   blocobaixodireita = encontraPosicaoMatriz (x+2,y+1) mapa

-- | Verifica se tem um Bloco Destrutivel à esquerda do tanque
verificaBlocoDesE :: PosicaoGrelha -> Mapa -> Bool
verificaBlocoDesE (x,y) mapa = blocoesquerdacima == Bloco Destrutivel || blocoesquerdabaixo == Bloco Destrutivel
                             where blocoesquerdacima = encontraPosicaoMatriz (x,y-1) mapa
                                   blocoesquerdabaixo = encontraPosicaoMatriz (x+1,y-1) mapa









-- | Recebe uma lista de Disparos e retorna só os DisparoChoque
selecionaChoques :: [Disparo] -> [Disparo]
selecionaChoques [] = []
selecionaChoques (DisparoChoque a b : ds) = DisparoChoque a b : selecionaChoques ds
selecionaChoques (_:ds) = selecionaChoques ds



-- | Verifica os blocos à volta do tanque
decideSeMovimentaMapa :: [Direcao] -> PosicaoGrelha -> Mapa -> [Direcao]
decideSeMovimentaMapa l pos mapa = l4
                                 where l1 = verificaC l pos mapa
                                       l2 = verificaD l1 pos mapa
                                       l3 = verificaB l2 pos mapa
                                       l4 = verificaE l3 pos mapa


-- | Verifica os blocos em cima do tanque
verificaC :: [Direcao] -> PosicaoGrelha -> Mapa -> [Direcao]
verificaC l (x,y) mapa | blococimaesquerda /= Vazia || blococimadireita /= Vazia = delete C l
                       | otherwise = l
                       where blococimaesquerda = encontraPosicaoMatriz (x-1,y) mapa
                             blococimadireita = encontraPosicaoMatriz (x-1,y+1) mapa

-- | Verifica os blocos à direita do tanque
verificaD :: [Direcao] -> PosicaoGrelha -> Mapa -> [Direcao]
verificaD l (x,y) mapa | blocodireitacima /= Vazia || blocodireitabaixo /= Vazia = delete D l
                       | otherwise = l
                       where blocodireitacima = encontraPosicaoMatriz (x,y+2) mapa
                             blocodireitabaixo = encontraPosicaoMatriz (x+1,y+2) mapa

-- | Verifica os blocos a baixo do tanque
verificaB :: [Direcao] -> PosicaoGrelha -> Mapa -> [Direcao]
verificaB l (x,y) mapa | blocobaixoesquerda /= Vazia || blocobaixodireita /= Vazia = delete B l
                       | otherwise = l
                       where blocobaixoesquerda = encontraPosicaoMatriz (x+2,y) mapa
                             blocobaixodireita = encontraPosicaoMatriz (x+2,y+1) mapa

-- | Verifica os blocos à esquerda do tanque
verificaE :: [Direcao] -> PosicaoGrelha -> Mapa -> [Direcao]
verificaE l (x,y) mapa | blocoesquerdacima /= Vazia || blocoesquerdabaixo /= Vazia = delete E l
                       | otherwise = l
                       where blocoesquerdacima = encontraPosicaoMatriz (x,y-1) mapa
                             blocoesquerdabaixo = encontraPosicaoMatriz (x+1,y-1) mapa



-- | Verifica os DisparoChoque do estado e decide se movimenta
decideSeMovimentaDisparosChoque :: [Direcao] -> PosicaoGrelha -> [Disparo] -> [Jogador] -> [Direcao]
decideSeMovimentaDisparosChoque l _ [] _ = l
decideSeMovimentaDisparosChoque l pos (DisparoChoque _ 0 : ds) jogs = decideSeMovimentaDisparosChoque l pos ds jogs
decideSeMovimentaDisparosChoque l (xb,yb) (DisparoChoque i _ : ds) jogs | xb == x && yb == y = decideSeMovimentaDisparosChoque l (xb,yb) ds jogs
                                                                        | xb-x == 4 && abs(yb-y) < 4 = decideSeMovimentaDisparosChoque (delete C l) (xb,yb) ds jogs
                                                                        | xb-x == -4 && abs(yb-y) < 4 = decideSeMovimentaDisparosChoque (delete B l) (xb,yb) ds jogs
                                                                        | yb-y == -4 && abs(xb-x) < 4 = decideSeMovimentaDisparosChoque (delete D l) (xb,yb) ds jogs
                                                                        | yb-y == 4 && abs(xb-x) < 4 = decideSeMovimentaDisparosChoque (delete E l) (xb,yb) ds jogs
                                                                        | otherwise = decideSeMovimentaDisparosChoque l (xb,yb) ds jogs
                                                                        where Jogador (x,y) _ _ _ _ = escolheJogador i jogs



-- | Verifica os disparos no estado, e movimenta-se conforme as suas posiçoes
decideSeMovimentaDisparos :: [Direcao] -> PosicaoGrelha -> [Disparo] -> Mapa -> [Direcao]
decideSeMovimentaDisparos l pos dis mapa = l3
                                         where l2 = verificaDisparosLaser l pos (selecionaLasers dis) mapa
                                               l3 = verificaDisparosCanhao l2 pos (selecionaCanhoes dis)



-- | Verifica os DisparoLaser do estado e decide se movimenta
verificaDisparosLaser :: [Direcao] -> PosicaoGrelha -> [Disparo] -> Mapa -> [Direcao]
verificaDisparosLaser l _ [] _ = l
verificaDisparosLaser l (xb,yb) (DisparoLaser _ (x,y) d : ds) mapa | verificaMapaJogadaOfensiva3 (x,y) (xb-2,yb) d mapa = verificaDisparosLaser (delete C l) (xb,yb) ds mapa
                                                                   | verificaMapaJogadaOfensiva3 (x,y) (xb+2,yb) d mapa = verificaDisparosLaser (delete B l) (xb,yb) ds mapa
                                                                   | verificaMapaJogadaOfensiva3 (x,y) (xb,yb+2) d mapa = verificaDisparosLaser (delete D l) (xb,yb) ds mapa
                                                                   | verificaMapaJogadaOfensiva3 (x,y) (xb,yb-2) d mapa = verificaDisparosLaser (delete E l) (xb,yb) ds mapa
                                                                   | otherwise = verificaDisparosLaser l (xb,yb) ds mapa


-- | Verifica os DisparoCanhao do estado e decide se movimenta
verificaDisparosCanhao :: [Direcao] -> PosicaoGrelha -> [Disparo] -> [Direcao]
verificaDisparosCanhao l _ [] = l
verificaDisparosCanhao l (xb,yb) (DisparoCanhao _ (x,y) C : ds) | yb-y == 2 && (xb-x > -3 && xb-x < 1) = verificaDisparosCanhao (delete E l) (xb,yb) ds
                                                                | yb-y == -2 && (xb-x > -3 && xb-x < 1) = verificaDisparosCanhao (delete D l) (xb,yb) ds

verificaDisparosCanhao l (xb,yb) (DisparoCanhao _ (x,y) D : ds) | xb-x == 2 && (yb-y > -1 && yb-y < 3) = verificaDisparosCanhao (delete C l) (xb,yb) ds
                                                                | xb-x == -2 && (yb-y > -1 && yb-y < 3) = verificaDisparosCanhao (delete B l) (xb,yb) ds

verificaDisparosCanhao l (xb,yb) (DisparoCanhao _ (x,y) B : ds) | yb-y == 2 && (xb-x > -1 && xb-x < 3) = verificaDisparosCanhao (delete E l) (xb,yb) ds
                                                                | yb-y == -2 && (xb-x > -1 && xb-x < 3) = verificaDisparosCanhao (delete D l) (xb,yb) ds

verificaDisparosCanhao l (xb,yb) (DisparoCanhao _ (x,y) E : ds) | xb-x == 2 && (yb-y > -3 && yb-y < 1) = verificaDisparosCanhao (delete C l) (xb,yb) ds
                                                                | xb-x == -2 && (yb-y > -3 && yb-y < 1) = verificaDisparosCanhao (delete B l) (xb,yb) ds

verificaDisparosCanhao l (xb,yb) (_:ds) = verificaDisparosCanhao l (xb,yb) ds




-- | Executa uma jogada defensiva
executaJogadaDefensiva :: Jogador -> Estado -> Maybe Jogada
executaJogadaDefensiva jogador (Estado mapa jogs dis) | jogadaDefensiva1 jogador estado /= Nothing = jogadaDefensiva1 jogador estado
                                                      | jogadaDefensiva2 jogador estado /= Nothing = jogadaDefensiva2 jogador estado
--                                                      | jogadaDefensiva3 jogador jogs estado /= Nothing = jogadaDefensiva3 jogador estado
--                                                      | jogadaDefensiva4 jogador estado = executaJogadaDefensiva4 jogador estado
                                                      | otherwise = Nothing
                                                      where estado = Estado mapa jogs dis



-- | Desvia se de um DisparoLaser a vir na sua direçao
jogadaDefensiva1 :: Jogador -> Estado -> Maybe Jogada
jogadaDefensiva1 (Jogador pos dir _ _ _) (Estado mapa jogs dis) = verificaLaserNaSuaDirecao pos dir lasersestado mapa
                                                                where lasersestado = selecionaLasers dis


-- | Verifica se tem algum DisparoLaser no estado, virado para si
verificaLaserNaSuaDirecao :: PosicaoGrelha -> Direcao -> [Disparo] -> Mapa -> Maybe Jogada
verificaLaserNaSuaDirecao _ _ [] _ = Nothing
verificaLaserNaSuaDirecao (xb,yb) dirb (DisparoLaser ind (x,y) C : ds) mapa | y-yb == 1 && (verificaMapaJogadaOfensiva3 (x,y) (xb,y) C mapa) = Just (Movimenta E)
                                                                            | y-yb == -1 && (verificaMapaJogadaOfensiva3 (x,y) (xb,y) C mapa) = Just (Movimenta D)
                                                                            | otherwise = verificaLaserNaSuaDirecao (xb,yb) dirb ds mapa

verificaLaserNaSuaDirecao (xb,yb) dirb (DisparoLaser ind (x,y) D : ds) mapa | x-xb == 1 && (verificaMapaJogadaOfensiva3 (x,y) (x,yb) D mapa) = Just (Movimenta C)
                                                                            | x-xb == -1 && (verificaMapaJogadaOfensiva3 (x,y) (x,yb) D mapa) = Just (Movimenta B)
                                                                            | otherwise = verificaLaserNaSuaDirecao (xb,yb) dirb ds mapa

verificaLaserNaSuaDirecao (xb,yb) dirb (DisparoLaser ind (x,y) B : ds) mapa | y-yb == 1 && (verificaMapaJogadaOfensiva3 (x,y) (xb,y) B mapa) = Just (Movimenta E)
                                                                            | y-yb == -1 && (verificaMapaJogadaOfensiva3 (x,y) (xb,y) B mapa) = Just (Movimenta D)
                                                                            | otherwise = verificaLaserNaSuaDirecao (xb,yb) dirb ds mapa

verificaLaserNaSuaDirecao (xb,yb) dirb (DisparoLaser ind (x,y) E : ds) mapa | x-xb == 1 && (verificaMapaJogadaOfensiva3 (x,y) (x,yb) E mapa) = Just (Movimenta C)
                                                                            | x-xb == -1 && (verificaMapaJogadaOfensiva3 (x,y) (x,yb) E mapa) = Just (Movimenta B)
                                                                            | otherwise = verificaLaserNaSuaDirecao (xb,yb) dirb ds mapa





-- | Desvia se de uma bala canhao por perto na sua direçao
jogadaDefensiva2 :: Jogador -> Estado -> Maybe Jogada
jogadaDefensiva2 (Jogador pos dir _ _ _) (Estado mapa jogs dis) = verificaBalaCanhaoPorPerto pos dir mapa canhoes
                                                where canhoes = selecionaCanhoes dis


-- | Verifica se deve tem um DisparoCanhao por perto na sua direçao
verificaBalaCanhaoPorPerto :: PosicaoGrelha -> Direcao -> Mapa -> [Disparo] -> Maybe Jogada
verificaBalaCanhaoPorPerto _ _ _ [] = Nothing
verificaBalaCanhaoPorPerto (xb,yb) dirb mapa (DisparoCanhao _ (x,y) C : ds) | x-xb == 1 && y-yb == 1 && dirb == E = Just (Movimenta E)
                                                                            | x-xb == 1 && y-yb == -1 && dirb == D = Just (Movimenta D)
                                                                            | x-xb == 2 && y==yb && dirb == B = Just(Dispara Laser)
                                                                            | x-xb == 2 && y==yb && blocodireitacima == Vazia && blocodireitabaixo == Vazia = Just (Movimenta D)
                                                                            | x-xb == 2 && y==yb = Just (Movimenta E)
                                                                            | x-xb == 3 && y==yb && dirb == B = Just (Dispara Canhao)
                                                                            | otherwise = verificaBalaCanhaoPorPerto (xb,yb) dirb mapa ds
                                                                            where blocodireitacima = encontraPosicaoMatriz (xb,yb+2) mapa
                                                                                  blocodireitabaixo = encontraPosicaoMatriz (xb+1,yb+2) mapa

verificaBalaCanhaoPorPerto (xb,yb) dirb mapa (DisparoCanhao _ (x,y) B : ds) | x-xb == -1 && y-yb == 1 && dirb == E = Just (Movimenta E)
                                                                            | x-xb == -1 && y-yb == -1 && dirb == D = Just (Movimenta D)
                                                                            | x-xb == -2 && y==yb && dirb == C = Just(Dispara Laser)
                                                                            | x-xb == -2 && y==yb && blocodireitacima == Vazia && blocodireitabaixo == Vazia = Just (Movimenta D)
                                                                            | x-xb == -2 && y==yb = Just (Movimenta E)
                                                                            | x-xb == -3 && y==yb && dirb == C = Just (Dispara Canhao)
                                                                            | otherwise = verificaBalaCanhaoPorPerto (xb,yb) dirb mapa ds
                                                                            where blocodireitacima = encontraPosicaoMatriz (xb,yb+2) mapa
                                                                                  blocodireitabaixo = encontraPosicaoMatriz (xb+1,yb+2) mapa

verificaBalaCanhaoPorPerto (xb,yb) dirb mapa (DisparoCanhao _ (x,y) D : ds) | y-yb == -1 && x-xb == 1 && dirb == C = Just (Movimenta C)
                                                                            | y-yb == -1 && x-xb == -1 && dirb == B = Just (Movimenta B)
                                                                            | y-yb == -2 && x==xb && dirb == E = Just(Dispara Laser)
                                                                            | y-yb == -2 && x==xb && blococimaesquerda == Vazia && blococimadireita == Vazia = Just (Movimenta C)
                                                                            | y-yb == -2 && x==xb = Just (Movimenta B)
                                                                            | y-yb == -3 && x==xb && dirb == E = Just (Dispara Canhao)
                                                                            | otherwise = verificaBalaCanhaoPorPerto (xb,yb) dirb mapa ds
                                                                            where blococimaesquerda = encontraPosicaoMatriz (xb-1,yb) mapa
                                                                                  blococimadireita = encontraPosicaoMatriz (xb-1,yb+1) mapa

verificaBalaCanhaoPorPerto (xb,yb) dirb mapa (DisparoCanhao _ (x,y) E : ds) | y-yb == 1 && x-xb == 1 && dirb == C = Just (Movimenta C)
                                                                            | y-yb == 1 && x-xb == -1 && dirb == B = Just (Movimenta B)
                                                                            | y-yb == 2 && x==xb && dirb == D = Just(Dispara Laser)
                                                                            | y-yb == 2 && x==xb && blococimaesquerda == Vazia && blococimadireita == Vazia = Just (Movimenta C)
                                                                            | y-yb == 2 && x==xb = Just (Movimenta B)
                                                                            | y-yb == 3 && x==xb && dirb == D = Just (Dispara Canhao)
                                                                            | otherwise = verificaBalaCanhaoPorPerto (xb,yb) dirb mapa ds
                                                                            where blococimaesquerda = encontraPosicaoMatriz (xb-1,yb) mapa
                                                                                  blococimadireita = encontraPosicaoMatriz (xb-1,yb+1) mapa







