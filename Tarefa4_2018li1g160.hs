-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g160 where

import LI11819
import Tarefa0_2018li1g160
import Tarefa1_2018li1g160


-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [Estado (mapaInicial (8,8)) [Jogador (5,5) C 1 1 1] [DisparoCanhao 0 (1,1) D, DisparoCanhao 0 (1,2) E],  --testa quando dois DisparoCanhao se atravessam
            Estado (mapaInicial (8,8)) [Jogador (5,5) C 1 1 1] [DisparoCanhao 0 (1,2) D, DisparoCanhao 0 (1,1) E],  --testa quando dois DisparoCanhao ja se atravessaram
            Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [Jogador (1,1) D 1 1 1] [DisparoCanhao 0 (1,2) D],   --testa o embate de um DisparoCanhao num Bloco Indestrutivel e num Bloco Destrutivel simultaneamente
            Estado (mapaInicial (6,6)) [Jogador (1,1) D 1 1 1] [DisparoCanhao 0 (2,1) C],  --testa um embate de um DisparoCanhao num jogador
            Estado (mapaInicial (8,8)) [Jogador (5,5) C 1 1 1] [DisparoCanhao 0 (1,1) C, DisparoCanhao 0 (1,1) E],   --testa o embate entre dois DisparoCanhao
            Estado (mapaInicial (8,8)) [Jogador (5,5) C 1 1 1] [DisparoCanhao 0 (1,1) D],  --testa um movimento normal de um DisparoCanhao
            Estado (atualizaPosicaoMatriz (2,3) (Bloco Destrutivel) (mapaInicial (6,6))) [Jogador (1,1) C 1 1 1] [DisparoCanhao 0 (2,2) C],  --testa o embate de um DisparoCanhao com um jogador e um Bloco Destrutivel simultaneamente
            Estado (mapaInicial (4,4)) [Jogador (1,1) D 1 1 1] [DisparoChoque 0 1], --testa o processamento de um DisparoChoque
            Estado (mapaInicial (4,4)) [Jogador (1,1) D 1 1 1] [DisparoChoque 0 0], --testa a remoçao de um DisparoChoque
            Estado (mapaInicial (8,8)) [Jogador (5,5) C 1 1 1] [DisparoLaser 0 (2,2) B, DisparoLaser 0 (4,4) E], --testa um choque entre dois DisparoLaser
            Estado (mapaInicial (8,8)) [Jogador (1,1) D 1 1 1, Jogador (1,3) E 2 2 2] [DisparoLaser 0 (1,4) E],  --testa um choque entre um DisparoLaser e dois tanques
            Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel],
                    [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [Jogador (1,1) C 1 1 1] [DisparoLaser 0 (1,2) D],  --testa um embate de um DisparoLaser num Bloco Indestrutivel e num Bloco Destrutivel simultaneamente
            Estado (mapaInicial (8,8)) [Jogador (5,5) C 1 1 1] [DisparoLaser 0 (2,2) D, DisparoCanhao 0 (2,3) B, DisparoCanhao 0 (1,4) B], --testa um embate entre um DisparoLaser e dois DisparoCanhao
            Estado (mapaInicial (8,8)) [Jogador (1,1) D 5 5 5] [DisparoCanhao 0 (2,2) B], --testa um DisparoCanhao a passar a frente de um jogador
            Estado (mapaInicial (8,8)) [Jogador (1,1) D 5 5 5] [DisparoCanhao 0 (1,2) B], --testa um DisparoCanhao a passar a frente de um jogador
            Estado (mapaInicial (8,8)) [Jogador (1,1) D 5 5 5] [DisparoCanhao 0 (1,2) D]] --testa um DisparoCanhao a passar a frente de um jogador



-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick = tickChoques . tickCanhoes . tickLasers

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers (Estado mapa jogadores disparos) = Estado (aplicaLasersMapa (selecionaLasers disparos) mapa) (aplicaLasersJogadores (selecionaLasers disparos) jogadores mapa) (aplicaLasersDisparos (selecionaLasers disparos) mapa (removeLasers disparos))


-- | Remove DisparoLaser da lista de disparos do estado seguinte
removeLasers :: [Disparo] -> [Disparo]
removeLasers [] = []
removeLasers (DisparoLaser _ _ _ :ds) = removeLasers ds
removeLasers (d:ds) = d : removeLasers ds


-- | Recebe a lista de disparos do estado anterior, e fica só com os DisparoLaser
selecionaLasers :: [Disparo] -> [Disparo]
selecionaLasers [] = []
selecionaLasers (DisparoLaser i p d :ds) = (DisparoLaser i p d) : selecionaLasers ds
selecionaLasers (d:ds) = selecionaLasers ds


-- | Recebe uma lista com DisparoLaser e aplica os seus efeitos no mapa
aplicaLasersMapa :: [Disparo] -> Mapa -> Mapa
aplicaLasersMapa [] mapa = mapa
aplicaLasersMapa (d:ds) mapa = aplicaLasersMapa ds (aplicaUmLaserMapa d (expandeLaser d mapa) mapa)


-- | Determina o tamanho do laser e indica qual dos blocos é que o fez parar, depois de expandir
expandeLaser :: Disparo -> Mapa -> (Int,Int)
expandeLaser (DisparoLaser i (x,y) C) mapa | proximobloco1 == Bloco Indestrutivel && proximobloco2 /= Bloco Indestrutivel = (0,0)
                                           | proximobloco1 /= Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = (0,1)
                                           | proximobloco1 == Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = (0,3)
                                           | otherwise = somaVetores (1,0) (expandeLaser (DisparoLaser i (x-1,y) C) mapa)
                                           where proximobloco1 = encontraPosicaoMatriz (x,y) mapa
                                                 proximobloco2 = encontraPosicaoMatriz (x,y+1) mapa

expandeLaser (DisparoLaser i (x,y) D) mapa | proximobloco1 == Bloco Indestrutivel && proximobloco2 /= Bloco Indestrutivel = (0,0)
                                           | proximobloco1 /= Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = (0,1)
                                           | proximobloco1 == Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = (0,3)
                                           | otherwise = somaVetores (1,0) (expandeLaser (DisparoLaser i (x,y+1) D) mapa)
                                           where proximobloco1 = encontraPosicaoMatriz (x,y+1) mapa
                                                 proximobloco2 = encontraPosicaoMatriz (x+1,y+1) mapa

expandeLaser (DisparoLaser i (x,y) B) mapa | proximobloco1 == Bloco Indestrutivel && proximobloco2 /= Bloco Indestrutivel = (0,0)
                                           | proximobloco1 /= Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = (0,1)
                                           | proximobloco1 == Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = (0,3)                                           
                                           | otherwise = somaVetores (1,0) (expandeLaser (DisparoLaser i (x+1,y) B) mapa)
                                           where proximobloco1 = encontraPosicaoMatriz (x+1,y) mapa
                                                 proximobloco2 = encontraPosicaoMatriz (x+1,y+1) mapa

expandeLaser (DisparoLaser i (x,y) E) mapa | proximobloco1 == Bloco Indestrutivel && proximobloco2 /= Bloco Indestrutivel = (0,0)
                                           | proximobloco1 /= Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = (0,1)
                                           | proximobloco1 == Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = (0,3)
                                           | otherwise = somaVetores (1,0) (expandeLaser (DisparoLaser i (x,y-1) E) mapa)
                                           where proximobloco1 = encontraPosicaoMatriz (x,y) mapa
                                                 proximobloco2 = encontraPosicaoMatriz (x+1,y) mapa



-- | Recebe apenas um DisparoLaser e aplica os seus efeitos no mapa
aplicaUmLaserMapa :: Disparo -> (Int,Int) -> Mapa -> Mapa
aplicaUmLaserMapa (DisparoLaser i pos dir) (0,3) mapa = mapa
aplicaUmLaserMapa (DisparoLaser i pos dir) (0,b) mapa = alteraMapa b pos dir mapa
aplicaUmLaserMapa (DisparoLaser i pos dir) (a,b) mapa = aplicaUmLaserMapa (DisparoLaser i (somaVetores pos (direcaoParaVetor dir)) dir) (a-1,b) (alteraMapa 2 pos dir mapa)


-- | Atualiza o mapa, colocando blocos Vazios por onde o laser passa
alteraMapa :: Int -> PosicaoGrelha -> Direcao -> Mapa -> Mapa
alteraMapa 2 (x,y) C mapa = alteraMapa 0 (x,y) C (alteraMapa 1 (x,y) C mapa)
alteraMapa 1 (x,y) C mapa = atualizaPosicaoMatriz (x,y) Vazia mapa
alteraMapa 0 (x,y) C mapa = atualizaPosicaoMatriz (x,y+1) Vazia mapa

alteraMapa 2 (x,y) D mapa = alteraMapa 0 (x,y) D (alteraMapa 1 (x,y) D mapa)
alteraMapa 1 (x,y) D mapa = atualizaPosicaoMatriz (x,y+1) Vazia mapa
alteraMapa 0 (x,y) D mapa = atualizaPosicaoMatriz (x+1,y+1) Vazia mapa

alteraMapa 2 (x,y) B mapa = alteraMapa 0 (x,y) B (alteraMapa 1 (x,y) B mapa)
alteraMapa 1 (x,y) B mapa = atualizaPosicaoMatriz (x+1,y) Vazia mapa
alteraMapa 0 (x,y) B mapa = atualizaPosicaoMatriz (x+1,y+1) Vazia mapa

alteraMapa 2 (x,y) E mapa = alteraMapa 0 (x,y) E (alteraMapa 1 (x,y) E mapa)
alteraMapa 1 (x,y) E mapa = atualizaPosicaoMatriz (x,y) Vazia mapa
alteraMapa 0 (x,y) E mapa = atualizaPosicaoMatriz (x+1,y) Vazia mapa





-- | Recebe uma lista com DisparoLaser e aplica os seus efeitos nos jogadores
aplicaLasersJogadores :: [Disparo] -> [Jogador] -> Mapa -> [Jogador]
aplicaLasersJogadores [] js _ = js
aplicaLasersJogadores _ [] _ = []
aplicaLasersJogadores disparos (j:js) mapa = aplicaLasersUmJogador disparos j mapa : aplicaLasersJogadores disparos js mapa


-- | Recebe uma lista com DisparoLaser e aplica os seus efeitos a um jogador
aplicaLasersUmJogador :: [Disparo] -> Jogador -> Mapa -> Jogador
aplicaLasersUmJogador _ (Jogador pos dir 0 las cho) _ = Jogador pos dir 0 las cho
aplicaLasersUmJogador [d] j mapa = aplicaUmLaserUmJogador d (fst(expandeLaser d mapa)) j
aplicaLasersUmJogador (d:ds) j mapa = aplicaUmLaserUmJogador d (fst(expandeLaser d mapa)) (aplicaLasersUmJogador ds j mapa)


-- | Recebe um unico laser e aplica os seus efeitos num unico jogador
aplicaUmLaserUmJogador :: Disparo -> Int -> Jogador -> Jogador
aplicaUmLaserUmJogador (DisparoLaser i (x,y) C) 0 (Jogador (xj,yj) dirj vid las cho) | x-xj == 1 && (y-yj > -2 && y-yj < 2) = Jogador (xj,yj) dirj (vid-1) las cho
                                                                                     | otherwise = Jogador (xj,yj) dirj vid las cho

aplicaUmLaserUmJogador (DisparoLaser i (x,y) C) a (Jogador (xj,yj) dirj vid las cho) | x-xj == 1 && (y-yj > -2 && y-yj < 2) = Jogador (xj,yj) dirj (vid-1) las cho
                                                                                     | otherwise = aplicaUmLaserUmJogador (DisparoLaser i (x-1,y) C) (a-1) (Jogador (xj,yj) dirj vid las cho)


aplicaUmLaserUmJogador (DisparoLaser i (x,y) D) 0 (Jogador (xj,yj) dirj vid las cho) | y-yj == -1 && (x-xj > -2 && x-xj < 2) = Jogador (xj,yj) dirj (vid-1) las cho
                                                                                     | otherwise = Jogador (xj,yj) dirj vid las cho

aplicaUmLaserUmJogador (DisparoLaser i (x,y) D) a (Jogador (xj,yj) dirj vid las cho) | y-yj == -1 && (x-xj > -2 && x-xj < 2) = Jogador (xj,yj) dirj (vid-1) las cho
                                                                                     | otherwise = aplicaUmLaserUmJogador (DisparoLaser i (x,y+1) D) (a-1) (Jogador (xj,yj) dirj vid las cho)


aplicaUmLaserUmJogador (DisparoLaser i (x,y) B) 0 (Jogador (xj,yj) dirj vid las cho) | x-xj == -1 && (y-yj > -2 && y-yj < 2) = Jogador (xj,yj) dirj (vid-1) las cho
                                                                                     | otherwise = Jogador (xj,yj) dirj vid las cho

aplicaUmLaserUmJogador (DisparoLaser i (x,y) B) a (Jogador (xj,yj) dirj vid las cho) | x-xj == -1 && (y-yj > -2 && y-yj < 2) = Jogador (xj,yj) dirj (vid-1) las cho
                                                                                     | otherwise = aplicaUmLaserUmJogador (DisparoLaser i (x+1,y) B) (a-1) (Jogador (xj,yj) dirj vid las cho)


aplicaUmLaserUmJogador (DisparoLaser i (x,y) E) 0 (Jogador (xj,yj) dirj vid las cho) | y-yj == 1 && (x-xj > -2 && x-xj < 2) = Jogador (xj,yj) dirj (vid-1) las cho
                                                                                     | otherwise = Jogador (xj,yj) dirj vid las cho

aplicaUmLaserUmJogador (DisparoLaser i (x,y) E) a (Jogador (xj,yj) dirj vid las cho) | y-yj == 1 && (x-xj > -2 && x-xj < 2) = Jogador (xj,yj) dirj (vid-1) las cho
                                                                                     | otherwise = aplicaUmLaserUmJogador (DisparoLaser i (x,y-1) E) (a-1) (Jogador (xj,yj) dirj vid las cho)



-- | Recebe uma lista com DisparoLaser e aplica os seus efeitos na lista de disparos do estado seguinte
aplicaLasersDisparos :: [Disparo] -> Mapa -> [Disparo] -> [Disparo]
aplicaLasersDisparos [] _ disparos = disparos
aplicaLasersDisparos [l] mapa disparos = aplicaUmLaserDisparos l mapa disparos
aplicaLasersDisparos (l:ls) mapa disparos = aplicaUmLaserDisparos l mapa (aplicaLasersDisparos ls mapa disparos)


-- | Recebe apenas um DisparoLaser e aplica os seus efeitos na lista de disparos do estado
aplicaUmLaserDisparos :: Disparo -> Mapa -> [Disparo] -> [Disparo]
aplicaUmLaserDisparos _ _ [] = []
aplicaUmLaserDisparos laser mapa [d] = aplicaUmLaserUmDisparo laser (fst(expandeLaser laser mapa)) d
aplicaUmLaserDisparos laser mapa (d:ds) = aplicaUmLaserUmDisparo laser (fst(expandeLaser laser mapa)) d ++ aplicaUmLaserDisparos laser mapa ds


-- | Recebe apenas um DisparoLaser e aplica os seus efeitos num unico disparo
aplicaUmLaserUmDisparo :: Disparo -> Int -> Disparo -> [Disparo]
aplicaUmLaserUmDisparo _ _ (DisparoChoque a b) = [DisparoChoque a b]
aplicaUmLaserUmDisparo laser 0 canhao = [canhao]

aplicaUmLaserUmDisparo (DisparoLaser i pos dir) a (DisparoCanhao ic posc dirc) | pos == posc = []
                                                                               | otherwise = aplicaUmLaserUmDisparo (DisparoLaser i (somaVetores pos (direcaoParaVetor dir)) dir) (a-1) (DisparoCanhao ic posc dirc)



-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.
tickCanhoes :: Estado -> Estado
tickCanhoes (Estado mapa jogadores disparos) = Estado (aplicaCanhoesMapa (selecionaCanhoes disparos) mapa) (aplicaCanhoesJogadores (selecionaCanhoes disparos) jogadores) (avancaCanhoes (filtraCanhoes (aplicaCanhoesDisparos (selecionaCanhoes disparos) disparos) mapa jogadores) )


-- | Recebe a lista de disparos e retribui apenas os DisparoCanhao
selecionaCanhoes :: [Disparo] -> [Disparo]
selecionaCanhoes [] = []
selecionaCanhoes (DisparoCanhao i pos dir : ds) = (DisparoCanhao i pos dir) : selecionaCanhoes ds
selecionaCanhoes (d:ds) = selecionaCanhoes ds


-- | Recebe uma lista com DisparoCanhao e aplica os seus efeitos no mapa
aplicaCanhoesMapa :: [Disparo] -> Mapa -> Mapa
aplicaCanhoesMapa [] mapa = mapa
aplicaCanhoesMapa [d] mapa = aplicaCanhaoMapa d mapa
aplicaCanhoesMapa (d:ds) mapa = aplicaCanhaoMapa d (aplicaCanhoesMapa ds mapa)


-- | Recebe apenas um DisparoCanhao e aplica os seus efeitos no mapa
aplicaCanhaoMapa :: Disparo -> Mapa -> Mapa
aplicaCanhaoMapa (DisparoCanhao i (x,y) C) mapa | proximobloco1 == Bloco Indestrutivel && proximobloco2 /= Bloco Indestrutivel = alteraMapa 0 (x,y) C mapa
                                                | proximobloco1 /= Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = alteraMapa 1 (x,y) C mapa
                                                | proximobloco1 == Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = mapa
                                                | otherwise = alteraMapa 2 (x,y) C mapa
                                                where proximobloco1 = encontraPosicaoMatriz (x,y) mapa
                                                      proximobloco2 = encontraPosicaoMatriz (x,y+1) mapa

aplicaCanhaoMapa (DisparoCanhao i (x,y) D) mapa | proximobloco1 == Bloco Indestrutivel && proximobloco2 /= Bloco Indestrutivel = alteraMapa 0 (x,y) D mapa
                                                | proximobloco1 /= Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = alteraMapa 1 (x,y) D mapa
                                                | proximobloco1 == Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = mapa
                                                | otherwise = alteraMapa 2 (x,y) D mapa
                                                where proximobloco1 = encontraPosicaoMatriz (x,y+1) mapa
                                                      proximobloco2 = encontraPosicaoMatriz (x+1,y+1) mapa

aplicaCanhaoMapa (DisparoCanhao i (x,y) B) mapa | proximobloco1 == Bloco Indestrutivel && proximobloco2 /= Bloco Indestrutivel = alteraMapa 0 (x,y) B mapa
                                                | proximobloco1 /= Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = alteraMapa 1 (x,y) B mapa
                                                | proximobloco1 == Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = mapa
                                                | otherwise = alteraMapa 2 (x,y) B mapa
                                                where proximobloco1 = encontraPosicaoMatriz (x+1,y) mapa
                                                      proximobloco2 = encontraPosicaoMatriz (x+1,y+1) mapa

aplicaCanhaoMapa (DisparoCanhao i (x,y) E) mapa | proximobloco1 == Bloco Indestrutivel && proximobloco2 /= Bloco Indestrutivel = alteraMapa 0 (x,y) E mapa
                                                | proximobloco1 /= Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = alteraMapa 1 (x,y) E mapa
                                                | proximobloco1 == Bloco Indestrutivel && proximobloco2 == Bloco Indestrutivel = mapa
                                                | otherwise = alteraMapa 2 (x,y) E mapa
                                                where proximobloco1 = encontraPosicaoMatriz (x,y) mapa
                                                      proximobloco2 = encontraPosicaoMatriz (x+1,y) mapa


-- | Recebe uma lista com DisparoCanhao e aplica os seus efeitos nos jogadores do estado
aplicaCanhoesJogadores :: [Disparo] -> [Jogador] -> [Jogador]
aplicaCanhoesJogadores _ [] = []
aplicaCanhoesJogadores canhoes (j:js) = aplicaCanhoesUmJogador canhoes j : aplicaCanhoesJogadores canhoes js


-- | Recebe uma lista com DisparoCanhao e aplica os seus efeitos num unico jogador
aplicaCanhoesUmJogador :: [Disparo] -> Jogador -> Jogador
aplicaCanhoesUmJogador _ (Jogador pos dir 0 las cho) = Jogador pos dir 0 las cho
aplicaCanhoesUmJogador [] jogador = jogador
aplicaCanhoesUmJogador [c] jogador = aplicaUmCanhaoUmJogador c jogador
aplicaCanhoesUmJogador (c:cs) jogador = aplicaUmCanhaoUmJogador c (aplicaCanhoesUmJogador cs jogador)


-- | Recebe um DisparoCanhao e aplica os seus efeitos num unico jogador
aplicaUmCanhaoUmJogador :: Disparo -> Jogador -> Jogador
aplicaUmCanhaoUmJogador canhao (Jogador posj dirj vid las cho) | verificaEmbateBalaJogador canhao [Jogador posj dirj vid las cho] = Jogador posj dirj vid las cho
                                                               | otherwise = Jogador posj dirj (vid-1) las cho




-- | Recebe uma lista com DisparoCanhao e aplica os seus efeitos nos disparos do estado
aplicaCanhoesDisparos :: [Disparo] -> [Disparo] -> [Disparo]
aplicaCanhoesDisparos [] disparos = disparos
aplicaCanhoesDisparos [c] disparos = aplicaUmCanhaoDisparos c disparos
aplicaCanhoesDisparos (c:cs) disparos = aplicaUmCanhaoDisparos c (aplicaCanhoesDisparos cs disparos)


-- | Recebe um DisparoCanhao e aplica os seus efeitos no disparos do estado
aplicaUmCanhaoDisparos :: Disparo -> [Disparo] -> [Disparo]
aplicaUmCanhaoDisparos _ [] = []
aplicaUmCanhaoDisparos canhao ((DisparoCanhao a b c):ds) = aplicaUmCanhaoUmDisparo canhao (DisparoCanhao a b c) ++ aplicaUmCanhaoDisparos canhao ds
aplicaUmCanhaoDisparos canhao (d:ds) = [d] ++ aplicaUmCanhaoDisparos canhao ds

-- | Recebe um DisparoCanhao e aplica os seus efeitos num unico disparo do estado
aplicaUmCanhaoUmDisparo :: Disparo -> Disparo -> [Disparo]
aplicaUmCanhaoUmDisparo (DisparoCanhao i pos dir) (DisparoCanhao i2 pos2 dir2) | i == i2 && pos == pos2 && dir == dir2 = [DisparoCanhao i2 pos2 dir2]
                                                                               | pos == pos2 = []
                                                                               | pos == (somaVetores pos2 (direcaoParaVetor dir)) = []
                                                                               | otherwise = [DisparoCanhao i2 pos2 dir2]



-- | Recebe os disparos do estado e remove aqueles que embateram num bloco ou num jogador
filtraCanhoes :: [Disparo] -> Mapa -> [Jogador] -> [Disparo]
filtraCanhoes [] _ _ = []
filtraCanhoes (DisparoCanhao i pos dir : ds) mapa jogadores | verificaEmbateBalaJogador (DisparoCanhao i pos dir) jogadores && verificaEmbateBalaBloco (DisparoCanhao i pos dir) mapa = DisparoCanhao i pos dir : filtraCanhoes ds mapa jogadores
                                                            | otherwise = filtraCanhoes ds mapa jogadores

filtraCanhoes (d:ds) mapa jogadores = d : filtraCanhoes ds mapa jogadores




-- | Recebe um DisparoCanhao e verifica se embateu num jogador: retribui True caso nao embata
verificaEmbateBalaJogador :: Disparo -> [Jogador] -> Bool
verificaEmbateBalaJogador _ [] = True
verificaEmbateBalaJogador (DisparoCanhao ind (xb,yb) dirb) (Jogador (xj,yj) dirj 0 las cho : js) = verificaEmbateBalaJogador (DisparoCanhao ind (xb,yb) dirb) js

verificaEmbateBalaJogador (DisparoCanhao ind (xb,yb) C) (Jogador (xj,yj) dirj vid las cho : js) | (xb-xj == 1 || xb == xj) && (yb-yj > -2 && yb-yj < 2) = False
                                                                                                | otherwise = verificaEmbateBalaJogador (DisparoCanhao ind (xb,yb) C) js

verificaEmbateBalaJogador (DisparoCanhao ind (xb,yb) D) (Jogador (xj,yj) dirj vid las cho : js) | (xb-xj > -2 && xb-xj < 2) && (yb-yj == -1 || yb == yj) = False
                                                                                                | otherwise = verificaEmbateBalaJogador (DisparoCanhao ind (xb,yb) D) js

verificaEmbateBalaJogador (DisparoCanhao ind (xb,yb) B) (Jogador (xj,yj) dirj vid las cho : js) | (xb-xj == -1 || xb == xj) && (yb-yj > -2 && yb-yj < 2) = False
                                                                                                | otherwise = verificaEmbateBalaJogador (DisparoCanhao ind (xb,yb) B) js

verificaEmbateBalaJogador (DisparoCanhao ind (xb,yb) E) (Jogador (xj,yj) dirj vid las cho : js) | (xb-xj > -2 && xb-xj < 2) && (yb-yj == 1 || yb == yj) = False
                                                                                                | otherwise = verificaEmbateBalaJogador (DisparoCanhao ind (xb,yb) E) js


-- | Recebe um DisparoCanhao e verifica se embateu num bloco: retribui True caso nao embata
verificaEmbateBalaBloco :: Disparo -> Mapa -> Bool
verificaEmbateBalaBloco (DisparoCanhao ind (xb,yb) C) mapa = bloco1 == Vazia && bloco2 == Vazia
                                                           where bloco1 = encontraPosicaoMatriz (xb,yb) mapa
                                                                 bloco2 = encontraPosicaoMatriz (xb,yb+1) mapa

verificaEmbateBalaBloco (DisparoCanhao ind (xb,yb) D) mapa = bloco1 == Vazia && bloco2 == Vazia
                                                           where bloco1 = encontraPosicaoMatriz (xb,yb+1) mapa
                                                                 bloco2 = encontraPosicaoMatriz (xb+1,yb+1) mapa

verificaEmbateBalaBloco (DisparoCanhao ind (xb,yb) B) mapa = bloco1 == Vazia && bloco2 == Vazia
                                                           where bloco1 = encontraPosicaoMatriz (xb+1,yb) mapa
                                                                 bloco2 = encontraPosicaoMatriz (xb+1,yb+1) mapa

verificaEmbateBalaBloco (DisparoCanhao ind (xb,yb) E) mapa = bloco1 == Vazia && bloco2 == Vazia
                                                           where bloco1 = encontraPosicaoMatriz (xb,yb) mapa
                                                                 bloco2 = encontraPosicaoMatriz (xb+1,yb) mapa


-- | Recebe a lista de disparos do estado e executa o movimento dos DisparoCanhao
avancaCanhoes :: [Disparo] -> [Disparo]
avancaCanhoes [] = []
avancaCanhoes (DisparoCanhao i pos dir : ds) = DisparoCanhao i (somaVetores pos (direcaoParaVetor dir)) dir : avancaCanhoes ds
avancaCanhoes (d:ds) = d : avancaCanhoes ds


{-
verificaEmbateBalaBala :: Disparo -> [Disparo] -> Bool
verificaEmbateBalaBala _ [] = True
verificaEmbateBalaBala (DisparoCanhao ind (xb,yb) dirb) (DisparoCanhao _ (x,y) dir : ds) | nxb == nx && nyb == ny = False
                                                                                         | xb == nx && yb == ny = False
                                                                                         | otherwise = verificaEmbateBalaBala (DisparoCanhao ind (xb,yb) dirb) ds
                                                                                         where (nxb,nyb) = somaVetores (xb,yb) (direcaoParaVetor dirb)
                                                                                               (nx,ny) = somaVetores (x,y) (direcaoParaVetor dir)
-}


-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques (Estado mapa jogadores disparos) = Estado mapa jogadores (auxChoques disparos)


-- | Recebe a lista de disparos do estado e remove um tick aos DisparoChoque
auxChoques :: [Disparo] -> [Disparo]
auxChoques [] = []
auxChoques (DisparoChoque _ 0 : ds) = auxChoques ds
auxChoques (DisparoChoque ind tick : ds) = DisparoChoque ind (tick -1) : auxChoques ds
auxChoques (d:ds) = d : auxChoques ds


