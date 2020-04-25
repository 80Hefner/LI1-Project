{- | Module      : Tarefa5_2018li1g160
   | Escola      : Universidade Do Minho
   | Description : Implementação do jogo em Gloss
   | Copyright   : Bruno Filipe de Sousa Dias <a89583@alunos.uminho.pt>
                    Luís Enes Sousa <a89597@alunos.uminho.pt>

= Introdução Tarefa 5:
Este módulo era um módulo livre, onde os alunos ficaram "á vontade" para a realização do mesmo, já que era uma Tarefa completamente livre!
O objetivo deste módulo era implementar o jogo em Gloss! Para isso utilizamos a biblioteca Gloss e as outras Tarefas realizadas, sendo as mais rquisitadas a Tarefa 2 e a Tarefa 4.

= Objetivos e Estratégias utilizadas:
Um dos nossos princípais objetivos para este módulo, era ter um jogo mais dinâmico em que não entravamos no jogo e nos deparávamos logo com o Mapa e os Jogadores prontos a jogar! 
Queríamos ter uma tela Inicial apelativa a jogar, onde podesse logo haver uma interação com a pessoa que ia jogar! Para isso criamos um novo tipo denominada "TankJogo" que vai ajudar 
em muito nisso e vai ser essencial noutros aspetos que vamos referir asseguir! Outro dos objetivos passa pela importância que nós direcionamos para o dinamismo que queríamos que o jogo tivesse! 
Neste caso gostávamos que o(s) jogador(es) tivessem a opurtonidade de escolha quer do mapa, quer do número de "players" que o jogo iria ter! Outro objetivo foi o de o jogador poder 
obter mais informação do que aquelas que o mapa dá durante o jogo! Com isto quere-mos referir-nos a aspetos importantes dos Jogadores (como as vidas de cada jogador, o número de 
lasers e choques disponíveis) e do número de Ticks decorridos! Não desprezamos os canhões, mas como estes podem ser utilizados infinitamente, não achamos relevante coloca-los 
na Tabela que criamos para a disposição de todas estas coisas! Outro objetivo que tinhamos era o de poder pôr mapas de diferentes tamanhos e estes respeitarem sempre o mesmo número de pixeis! 
Criamos para isso uma escala que iria interferir com o tamanho de tudo (quer do Mapa, jogadores ou disparos) e assim respeitarmos sempre da mesma forma o mesmo número de pixeis, o que faz com que 
apesar de haver disponivilidade de escolha quanto aos diferentes mapas (que inclusive têem todos tamanhos diferentes) faz com que estes respeitando escalas diferentes, ocupem sempre o mesmo número de pixeis! 
Além da tela inicial queriamos também criar uma tela quando o jogo acabasse e ainda houvesse jogadores vivos, o mais parecido com um TimeOut, ao que chamamos GameOver, que acontece quando são ultrapassados 
os 480 ticks (2 minutos)! Para além dessas telas criamos também telas que representariam o Jogador que ganha (onde consideramos o que ganha o único que acaba vivo, estando todos os outros Players com 0 vidas)! 
Por fim , queriamos algo diferente, que apesar de ser um jogo baseado em "Tanks", este "tivesse outra cara"! Para isso utilizamos gráficos que apelavam aos Navios e ao Mar!

= Conclusão:
Em suma, queríamos criar o jogo e torná-lo apelativo logo ao abrir, dinámico em termos das escolhas e possíbilidade de diferir e acima de tudo que fosse divertido. Conseguimos realizar a maioria dos 
nossos objetivos, embora hajam aspetos que gostaríamos que estivessem melhores! No entanto, no geral, achamos que fomos bem sucedidos e que apesar de não haver requisitos estipulados, fizemos um bom trabalho!
-}

module Main where

-- Imports necesários para a realização desta Tarefa
import LI11819
import Tarefa0_2018li1g160
import Tarefa1_2018li1g160
import Tarefa2_2018li1g160
import Tarefa3_2018li1g160
import Tarefa4_2018li1g160
import Tarefa6_2018li1g160
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game


-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.

-- | Função principal da Tarefa 5.
main :: IO ()
main = do blocoVaz <- loadBMP "images/agua.bmp"
          blocoDis <- loadBMP "images/caixa.bmp"
          blocoInd <- loadBMP "images/parede.bmp"
          tanque0 <- loadBMP "images/navio1.bmp"
          tanque1 <- loadBMP "images/navio2.bmp"
          tanque2 <- loadBMP "images/navio3.bmp"
          tanque3 <- loadBMP "images/navio4.bmp"
          choque0 <- loadBMP "images/choque1.bmp"
          choque1 <- loadBMP "images/choque2.bmp"
          choque2 <- loadBMP "images/choque3.bmp"
          choque3 <- loadBMP "images/choque4.bmp"
          canhao0 <- loadBMP "images/tiro1.bmp"
          canhao1 <- loadBMP "images/tiro2.bmp"
          canhao2 <- loadBMP "images/tiro3.bmp"
          canhao3 <- loadBMP "images/tiro4.bmp"
          laser0 <- loadBMP "images/laser1.bmp"
          laser1 <- loadBMP "images/laser2.bmp"
          laser2 <- loadBMP "images/laser3.bmp"
          laser3 <- loadBMP "images/laser4.bmp"
          gameOver <- loadBMP "images/GameOver.bmp"
          jogador0Ganhou <- loadBMP "images/jogador1ganha.bmp"
          jogador1Ganhou <- loadBMP "images/jogador2ganha.bmp"
          jogador2Ganhou <- loadBMP "images/jogador3ganha.bmp"
          jogador3Ganhou <- loadBMP "images/jogador4ganha.bmp"
          escolheMapa <- loadBMP "images/telaEscolheMapa.bmp"
          escolheJogadores <- loadBMP "images/escolheJogadores.bmp"
          telaInicial <- loadBMP "images/fundoTela.bmp"
          let l1 = [blocoVaz,blocoDis,blocoInd]
          let l2 = [tanque0,tanque1,tanque2,tanque3]
          let l3 = [canhao0,canhao1,canhao2,canhao3]
          let l4 = [laser0,laser1,laser2,laser3]
          let l5 = [choque0,choque1,choque2,choque3]
          let l6 = [gameOver,jogador0Ganhou,jogador1Ganhou,jogador2Ganhou,jogador3Ganhou,escolheMapa,escolheJogadores,telaInicial]
          play FullScreen
               background
               fps
               TelaInicialDeTodoJogo
               (desenhaTanksJogo l1 l2 [l3,l4,l5] l6)
               reageEvento
               reageTempo
     


-- | Criação de um tipo de Estado, que nós dá um Estado Individual de um Ponto, ou seja, um conjunto de Floats.
type EstadoIndividual = (Float,Float)

-- | Criação de um tipo conhecido como Estado Gloss, que é composto por uma posição (Estado Individual) na sua primeira parcela e uma Pictures na sua segunda parcela. 
type EstadoIndividualGloss = (EstadoIndividual, Picture)

-- | Criação de um novo tipo para a realização do Trabalho!
data TanksJogo = TelaInicialDeTodoJogo
               | TelaInicialEscolheMapa 
               | TelaEscolheJogadores {mapaEscolhido :: Int
                                      }
               | AJogar {estadoDoJogo :: Estado
                        ,numeroTicks  :: Int
                        }
               | JogadorXGanha {nrIndiceJogador :: Int
                               }
               | AcabouJogo
             deriving Show



-- Informações para os Estados caso o Mapa 1 seja escolhido.
-- | Caso o Mapa 1 seja escolhido e a opção 4 jogadores
estadoInicialmapa1jogadores4 = Estado mapa1 jogadores4mapa1 disparos1
-- | Caso o Mapa 1 seja escolhido e a opção 3 jogadores
estadoInicialmapa1jogadores3 = Estado mapa1 jogadores3mapa1 disparos1
-- | Caso o Mapa 1 seja escolhido e a opção 2 jogadores
estadoInicialmapa1jogadores2 = Estado mapa1 jogadores2mapa1 disparos1
-- | Retribui o mapa referente ao Mapa 1
mapa1           = [[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Vazia, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Bloco Destrutivel, Vazia, Bloco Destrutivel, Vazia, Bloco Destrutivel, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Vazia, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel]]
-- | Retribui os jogadores referentes ao Mapa 1, com a opção 4 jogadores
jogadores4mapa1 = [Jogador (1,1) D 5 5 5, Jogador (6,6) C 5 5 5, Jogador (6,1) E 5 5 5, Jogador (1,6) B 5 5 5]
-- | Retribui os jogadores referentes ao Mapa 1, com a opção 3 jogadores
jogadores3mapa1 = [Jogador (1,1) D 5 5 5, Jogador (6,6) C 5 5 5, Jogador (6,1) E 5 5 5, Jogador (1,6) B 0 5 5]
-- | Retribui os jogadores referentes ao Mapa 1, com a opção 2 jogadores
jogadores2mapa1 = [Jogador (1,1) D 5 5 5, Jogador (6,6) C 5 5 5, Jogador (6,1) E 0 5 5, Jogador (1,6) B 0 5 5]
-- | Retribui os disparos referentes ao Mapa 1
disparos1       = []


-- Informações para os Estados caso o Mapa 2 seja escolhido.
-- | Caso o Mapa 2 seja escolhido e a opção 4 jogadores
estadoInicialmapa2jogadores4 = Estado mapa2 jogadores4mapa2 disparos2
-- | Caso o Mapa 2 seja escolhido e a opção 3 jogadores
estadoInicialmapa2jogadores3 = Estado mapa2 jogadores3mapa2 disparos2
-- | Caso o Mapa 2 seja escolhido e a opção 2 jogadores
estadoInicialmapa2jogadores2 = Estado mapa2 jogadores2mapa2 disparos2
-- | Retribui o mapa referente ao Mapa 2
mapa2           = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel ,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
-- | Retribui os jogadores referentes ao Mapa 2, com a opção 4 jogadores
jogadores4mapa2 = [Jogador (1,1) D 5 5 5, Jogador (7,7) C 5 5 5, Jogador (7,1) E 5 5 5, Jogador (1,7) B 5 5 5]
-- | Retribui os jogadores referentes ao Mapa 2, com a opção 3 jogadores
jogadores3mapa2 = [Jogador (1,1) D 5 5 5, Jogador (7,7) C 5 5 5, Jogador (7,1) E 5 5 5, Jogador (1,7) B 0 5 5]
-- | Retribui os jogadores referentes ao Mapa 2, com a opção 2 jogadores
jogadores2mapa2 = [Jogador (1,1) D 5 5 5, Jogador (7,7) C 5 5 5, Jogador (7,1) E 0 5 5, Jogador (1,7) B 0 5 5]
-- | Retribui os disparos referentes ao Mapa 2
disparos2       = []




-- Informações para os Estados caso o Mapa 3 seja escolhido.
-- | Caso o Mapa 3 seja escolhido e a opção 4 jogadores
estadoInicialmapa3jogadores4 = Estado mapa3 jogadores4mapa3 disparos3
-- | Caso o Mapa 3 seja escolhido e a opção 3 jogadores
estadoInicialmapa3jogadores3 = Estado mapa3 jogadores3mapa3 disparos3
-- | Caso o Mapa 3 seja escolhido e a opção 2 jogadores
estadoInicialmapa3jogadores2 = Estado mapa3 jogadores2mapa3 disparos3
-- | Retribui o mapa referente ao Mapa 3
mapa3           = [[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel], [Bloco Indestrutivel, Vazia,Vazia,Vazia, Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia, Vazia,Vazia,Bloco Indestrutivel], [Bloco Indestrutivel, Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel], [Bloco Indestrutivel, Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel], [Bloco Indestrutivel, Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel], [Bloco Indestrutivel, Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia, Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Vazia,Vazia,Vazia, Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia, Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel]]
-- | Retribui os jogadores referentes ao Mapa 3, com a opção 4 jogadores
jogadores4mapa3 = [Jogador (1,1) D 5 5 5, Jogador (10,10) C 5 5 5, Jogador (10,1) E 5 5 5, Jogador (1,10) B 5 5 5]
-- | Retribui os jogadores referentes ao Mapa 3, com a opção 3 jogadores
jogadores3mapa3 = [Jogador (1,1) D 5 5 5, Jogador (10,10) C 5 5 5, Jogador (10,1) E 5 5 5, Jogador (1,10) B 0 5 5]
-- | Retribui os jogadores referentes ao Mapa 3, com a opção 2 jogadores
jogadores2mapa3 = [Jogador (1,1) D 5 5 5, Jogador (10,10) C 5 5 5, Jogador (10,1) E 0 5 5, Jogador (1,10) B 0 5 5]
-- | Retribui os disparos referentes ao Mapa 3
disparos3       = []


-- Informações para os Estados caso o Mapa 4 seja escolhido.
-- | Caso o Mapa 4 seja escolhido e a opção 4 jogadores
estadoInicialmapa4jogadores4 = Estado mapa4 jogadores4mapa4 disparos4
-- | Caso o Mapa 4 seja escolhido e a opção 3 jogadores
estadoInicialmapa4jogadores3 = Estado mapa4 jogadores3mapa4 disparos4
-- | Caso o Mapa 4 seja escolhido e a opção 2 jogadores
estadoInicialmapa4jogadores2 = Estado mapa4 jogadores2mapa4 disparos4
-- | Retribui o mapa referente ao Mapa 4
mapa4           = [[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Indestrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Indestrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Vazia, Vazia, Vazia, Vazia, Vazia, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Destrutivel, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Vazia, Bloco Indestrutivel, Vazia, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Vazia, Vazia, Bloco Destrutivel, Vazia, Vazia, Bloco Indestrutivel],[Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel, Bloco Indestrutivel]]
-- | Retribui os jogadores referentes ao Mapa 4, com a opção 4 jogadores
jogadores4mapa4 = [Jogador (1,1) D 5 5 5, Jogador (12,12) C 5 5 5, Jogador (12,1) E 5 5 5, Jogador (1,12) B 5 5 5]
-- | Retribui os jogadores referentes ao Mapa 4, com a opção 3 jogadores
jogadores3mapa4 = [Jogador (1,1) D 5 5 5, Jogador (12,12) C 5 5 5, Jogador (12,1) E 5 5 5, Jogador (1,12) B 0 5 5]
-- | Retribui os jogadores referentes ao Mapa 4, com a opção 2 jogadores
jogadores2mapa4 = [Jogador (1,1) D 5 5 5, Jogador (12,12) C 5 5 5, Jogador (12,1) E 0 5 5, Jogador (1,12) B 0 5 5]
-- | Retribui os disparos referentes ao Mapa 4
disparos4       = []



-- | Função que nos devolve uma cor e que vai ser usada na função main para dar cor ao fundo do ecrã (Background).
background :: Color
background = makeColorI 176 233 252 40

-- | Númeo de Ticks ultrapassado por cada unidade de tempo da vida real (por cada Segundo).
fps :: Int 
fps = 4


-- | Função que reage consoante a passagem do Tempo! Ter em atenção que esta função tem diferentes propriedades consoante o Estado do "Jogo Dos Tanques" no momento!
reageTempo :: Float -> TanksJogo -> TanksJogo
reageTempo t (TelaInicialDeTodoJogo)  = TelaInicialDeTodoJogo
reageTempo t (TelaInicialEscolheMapa) = TelaInicialEscolheMapa 
reageTempo t (TelaEscolheJogadores x) = TelaEscolheJogadores x
reageTempo t (JogadorXGanha x)        = JogadorXGanha x
reageTempo t (AcabouJogo)             = AcabouJogo
reageTempo t (AJogar (Estado map jog dis) nrTicks) | (desbreNumerosDeJogadoresVivos jog) == 1 = JogadorXGanha (descobreOJogadoorVivo jog)
                                                   | (desbreNumerosDeJogadoresVivos jog) == 0 = AcabouJogo
                                                   | otherwise = aumentaOsTicks (AJogar (Estado map jog dis) nrTicks)
                                                   where aumentaOsTicks (AJogar (Estado map jog dis) nrTicks) | (nrTicks<=480) = (AJogar (tick(Estado map jog dis)) (nrTicks+1))
                                                                                                              | otherwise      = AcabouJogo  


-- | Função que recebndo um dado evento reage ao mesmo e transforma o Estado do jogo num outro Estado!
reageEvento :: Event -> TanksJogo -> TanksJogo
--Eventos Jogador 1:
reageEvento (EventKey (Char 'w') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 0 (Movimenta C) (Estado mapa jogadores disparos)) x
reageEvento (EventKey (Char 'd') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 0 (Movimenta D) (Estado mapa jogadores disparos)) x
reageEvento (EventKey (Char 's') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 0 (Movimenta B) (Estado mapa jogadores disparos)) x
reageEvento (EventKey (Char 'a') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 0 (Movimenta E) (Estado mapa jogadores disparos)) x
reageEvento (EventKey (Char '1') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 0 (Dispara Canhao) (Estado mapa jogadores disparos)) x
reageEvento (EventKey (Char '2') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 0 (Dispara Laser) (Estado mapa jogadores disparos)) x
reageEvento (EventKey (Char '3') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 0 (Dispara Choque) (Estado mapa jogadores disparos)) x

--Eventos Jogador 2:
reageEvento (EventKey (Char 't') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 1 (Movimenta C) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char 'h') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 1 (Movimenta D) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char 'g') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 1 (Movimenta B) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char 'f') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 1 (Movimenta E) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char '4') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 1 (Dispara Canhao) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char '5') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 1 (Dispara Laser) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char '6') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 1 (Dispara Choque) (Estado mapa jogadores disparos)) x

--Eventos Jogador 3:
reageEvento (EventKey (Char 'i') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 2 (Movimenta C) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char 'l') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 2 (Movimenta D) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char 'k') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 2 (Movimenta B) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char 'j') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 2 (Movimenta E) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char '7') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 2 (Dispara Canhao) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char '8') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 2 (Dispara Laser) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char '9') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 2 (Dispara Choque) (Estado mapa jogadores disparos)) x 

--Eventos Jogador 4:
reageEvento (EventKey (SpecialKey KeyUp) Down _ _)    (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 3 (Movimenta C) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 3 (Movimenta D) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (SpecialKey KeyDown) Down _ _)  (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 3 (Movimenta B) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _)  (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 3 (Movimenta E) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char ',') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 3 (Dispara Canhao) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char '.') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 3 (Dispara Laser) (Estado mapa jogadores disparos)) x 
reageEvento (EventKey (Char '-') Down _ _) (AJogar (Estado mapa jogadores disparos) x) = AJogar (jogada 3 (Dispara Choque) (Estado mapa jogadores disparos)) x 

--Eventos Tela Inicial quando o jogo é aberto:
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) TelaInicialDeTodoJogo = TelaInicialEscolheMapa
reageEvento (EventKey (SpecialKey KeySpace) Down _ _) TelaInicialDeTodoJogo = TelaInicialEscolheMapa

--Eventos Que Vão Permitir Escolher o Mapa em que se pertende jogar:
reageEvento (EventKey (Char '1') Down _ _) (TelaInicialEscolheMapa) = TelaEscolheJogadores 1
reageEvento (EventKey (Char '2') Down _ _) (TelaInicialEscolheMapa) = TelaEscolheJogadores 2
reageEvento (EventKey (Char '3') Down _ _) (TelaInicialEscolheMapa) = TelaEscolheJogadores 3
reageEvento (EventKey (Char '4') Down _ _) (TelaInicialEscolheMapa) = TelaEscolheJogadores 4

--Eventos que vão permitir escolher o número de Jogadores com o qual se pertende jogar, tendo em conta o mapa escolhido anteriormente::
reageEvento (EventKey (Char '2') Down _ _) (TelaEscolheJogadores 1) = AJogar estadoInicialmapa1jogadores2 0
reageEvento (EventKey (Char '3') Down _ _) (TelaEscolheJogadores 1) = AJogar estadoInicialmapa1jogadores3 0
reageEvento (EventKey (Char '4') Down _ _) (TelaEscolheJogadores 1) = AJogar estadoInicialmapa1jogadores4 0
reageEvento (EventKey (Char '2') Down _ _) (TelaEscolheJogadores 2) = AJogar estadoInicialmapa2jogadores2 0
reageEvento (EventKey (Char '3') Down _ _) (TelaEscolheJogadores 2) = AJogar estadoInicialmapa2jogadores3 0
reageEvento (EventKey (Char '4') Down _ _) (TelaEscolheJogadores 2) = AJogar estadoInicialmapa2jogadores4 0
reageEvento (EventKey (Char '2') Down _ _) (TelaEscolheJogadores 3) = AJogar estadoInicialmapa3jogadores2 0
reageEvento (EventKey (Char '3') Down _ _) (TelaEscolheJogadores 3) = AJogar estadoInicialmapa3jogadores3 0
reageEvento (EventKey (Char '4') Down _ _) (TelaEscolheJogadores 3) = AJogar estadoInicialmapa3jogadores4 0
reageEvento (EventKey (Char '2') Down _ _) (TelaEscolheJogadores 4) = AJogar estadoInicialmapa4jogadores2 0
reageEvento (EventKey (Char '3') Down _ _) (TelaEscolheJogadores 4) = AJogar estadoInicialmapa4jogadores3 0
reageEvento (EventKey (Char '4') Down _ _) (TelaEscolheJogadores 4) = AJogar estadoInicialmapa4jogadores4 0

--Evento que dá Restart e volta ao estado inicial:
reageEvento (EventKey (Char 'r') Down _ _) _ = TelaInicialDeTodoJogo

--Para qualquer outro evento não referido, tudo permanece igual:
reageEvento _ e = e




-- | Função que Desenha, transformando numa só Imagem (Picture) a Totalidade do Jogo!
desenhaTanksJogo :: [Picture] -> [Picture] -> [[Picture]] -> [Picture] -> TanksJogo -> Picture
desenhaTanksJogo l1 l2 [l3,l4,l5] l6 (AJogar estadoJogo nrTickss) = pictures [desenhaCena l1 l2 [l3,l4,l5] estadoJogo, desenhaNumeroTicks nrTickss estadoJogo, desenhaMenu estadoJogo]
desenhaTanksJogo l1 l2 [l3,l4,l5] l6 (TelaInicialDeTodoJogo)      = Translate 0 0 (l6!!7)
desenhaTanksJogo l1 l2 [l3,l4,l5] l6 (TelaInicialEscolheMapa)     = Translate 0 0 (l6!!5)
desenhaTanksJogo l1 l2 [l3,l4,l5] l6 (TelaEscolheJogadores x)     = Translate 0 0 (l6!!6)
desenhaTanksJogo l1 l2 [l3,l4,l5] l6 (JogadorXGanha x)            = Translate 0 0 (l6!!(x+1))
desenhaTanksJogo l1 l2 [l3,l4,l5] l6 (AcabouJogo)                 = Translate 0 0 (l6!!0)



-- | Função que desenha uma "Tabela" do lado direito do mapa com as informações do Jogo consoante o Estado do jogo que recebe! Esta Função irá provenciar uma Tabela com informações sobre os Jogadores.
desenhaMenu :: Estado -> Picture
desenhaMenu (Estado map jog dis) = pictures (desenhaJogadoresMenu jog ((((larguraMapa map)/2)+20),((alturaMapa map)/2)-100) 1)
-- | Função que vai dar auxílio á anterior tranformando as infprmações de um conjunto de jogadores e nos dá a Tabela.
desenhaJogadoresMenu :: [Jogador] -> (Float,Float) -> Int -> [Picture]
desenhaJogadoresMenu [] (l,a) n     = []
desenhaJogadoresMenu (x:xs) (l,a) n = desenhaCadaJogadorMenu x (l,a) n : desenhaJogadoresMenu xs (l,(a-100)) (n+1)
-- | Função auxiliar que vai fornecer o conjuntos das "Pictures" necessárias para as funções anteriores! Esta função inside em cada jogador (um único de cada vez)!
desenhaCadaJogadorMenu :: Jogador -> (Float,Float) -> Int -> Picture
desenhaCadaJogadorMenu (Jogador (a,b) d v l c) (x,y) i | (v/=0) && (i==1) = pictures [Translate x y (Color blue (scale 0.25 0.25 (Text ("Jogador" ++ (show i) ++ ":"))))
                                                                                     ,Translate (x+10) (y-30) (scale 0.13 0.13 (Text ("("++(show v)++" V)("++(show l)++" L)("++(show c)++" Ch)")))] 
                                                       | (v/=0) && (i==2) = pictures [Translate x y (Color (dark green) (scale 0.25 0.25 (Text ("Jogador" ++ (show i) ++ ":"))))
                                                                                     ,Translate (x+10) (y-30) (scale 0.13 0.13 (Text ("("++(show v)++" V)("++(show l)++" L)("++(show c)++" Ch)")))] 
                                                       | (v/=0) && (i==3) = pictures [Translate x y (Color yellow (scale 0.25 0.25 (Text ("Jogador" ++ (show i) ++ ":"))))
                                                                                     ,Translate (x+10) (y-30) (scale 0.13 0.13 (Text ("("++(show v)++" V)("++(show l)++" L)("++(show c)++" Ch)")))] 
                                                       | (v/=0) && (i==4) = pictures [Translate x y (Color red (scale 0.25 0.25 (Text ("Jogador" ++ (show i) ++ ":"))))
                                                                                     ,Translate (x+10) (y-30) (scale 0.13 0.13 (Text ("("++(show v)++" V)("++(show l)++" L)("++(show c)++" Ch)")))]
                                                       | (v==0) && (i==1) = pictures [Translate x y (Color blue (scale 0.25 0.25 (Text ("Jogador" ++ (show i) ++ ":"))))
                                                                                     ,Translate (x+10) (y-30) (scale 0.16 0.16 (Text ("REST IN PEACE")))] 
                                                       | (v==0) && (i==2) = pictures [Translate x y (Color (dark green) (scale 0.25 0.25 (Text ("Jogador" ++ (show i) ++ ":"))))
                                                                                     ,Translate (x+10) (y-30) (scale 0.16 0.16 (Text ("REST IN PEACE")))] 
                                                       | (v==0) && (i==3) = pictures [Translate x y (Color yellow (scale 0.25 0.25 (Text ("Jogador" ++ (show i) ++ ":"))))
                                                                                     ,Translate (x+10) (y-30) (scale 0.16 0.16 (Text ("REST IN PEACE")))] 
                                                       | (v==0) && (i==4) = pictures [Translate x y (Color red (scale 0.25 0.25 (Text ("Jogador" ++ (show i) ++ ":"))))
                                                                                     ,Translate (x+10) (y-30) (scale 0.16 0.16 (Text ("REST IN PEACE")))]                                                   


-- | Função que desenha o Número de Ticks ao lado do canto inferior direito do Mapa e que nos permite perceber quantos Ticks de Jogo é que já decorreram!
desenhaNumeroTicks :: Int -> Estado -> Picture
desenhaNumeroTicks x es = Translate (((larguraMapa (mapaEscolhidinho es))/2)+25) (((-(alturaMapa (mapaEscolhidinho es)))/2)+25) (scale 0.3 0.3 (Text (show x)))

-- | Função que vai, com a ajuda de outras funções auxiliares desenhar um Estado, do tipo predefinido em (LI11819.hs) e utilizado durante todo o Trabalho!
desenhaCena :: [Picture] -> [Picture] -> [[Picture]] -> Estado -> Picture
desenhaCena l1 l2 [l3,l4,l5] (Estado map jog dis) = pictures [(desenhaMatrizEstadoGloss (converteMapaParaGloss map l1)), (desenhaListaEstadoGloss (converteJogadoresParaGloss 0 jog (Estado map jog dis) l2)),(desenhaListaEstadoGloss (converteDisparosParaGloss dis (Estado map jog dis) [l3,l4,l5] jog))]
  







-- | Função que converte um Mapa para uma Matriz de Estados Gloss.
converteMapaParaGloss :: Mapa -> [Picture] -> [[EstadoIndividualGloss]]
converteMapaParaGloss mapa l = auxiliarLinhas ((((-(larguraMapa mapa))/2)+(((escalaCerta mapa)*100)/2)),((((alturaMapa mapa)/2))-(((escalaCerta mapa)*100)/2))) mapa l mapa

-- | Função auxiliar que vai permitir colocar o mapa no sítio certo do ecrã tendo em conta as carateristicas do Mapa.
auxiliarLinhas :: EstadoIndividual -> Mapa -> [Picture] -> Mapa -> [[EstadoIndividualGloss]]
auxiliarLinhas (n1,n2) [] _ mapa = []
auxiliarLinhas (n1,n2) (x:xs) l mapa = converteLinhaParaGloss (n1,n2) x l mapa : auxiliarLinhas (n1,(n2-((escalaCerta mapa)*100))) xs l mapa

-- | Função que converte cada Linha do Mapa numa Lista de Estados Gloss.
converteLinhaParaGloss :: EstadoIndividual -> [Peca] -> [Picture] -> Mapa -> [EstadoIndividualGloss]
converteLinhaParaGloss (n3,n4) [] _ mapa = []
converteLinhaParaGloss (n1,n2) (x:xs) l mapa = converteCadaPecaParaGloss (n1,n2) x l mapa : converteLinhaParaGloss ((n1+((escalaCerta mapa)*100)),n2) xs l mapa

-- | Função que converte cada Peca do Mapa para um Estado Gloss tendo em conta as caraterísticas do Mapa.
converteCadaPecaParaGloss :: EstadoIndividual -> Peca -> [Picture] -> Mapa -> EstadoIndividualGloss
converteCadaPecaParaGloss (n1,n2) (Vazia)               l mapa = ((n1,n2),(scale (escalaCerta mapa) (escalaCerta mapa) (l !! 0)))
converteCadaPecaParaGloss (n1,n2) (Bloco Destrutivel)   l mapa = ((n1,n2),(scale (escalaCerta mapa) (escalaCerta mapa) (l !! 1)))
converteCadaPecaParaGloss (n1,n2) (Bloco Indestrutivel) l mapa = ((n1,n2),(scale (escalaCerta mapa) (escalaCerta mapa) (l !! 2)))








-- | Função que converte uma Lista de Jogadores para uma Lista de Estados Gloss.
converteJogadoresParaGloss :: Int -> [Jogador] -> Estado -> [Picture] -> [EstadoIndividualGloss]
converteJogadoresParaGloss _ [] es _      = []
converteJogadoresParaGloss x (j:js) es is = converteUmJogadorParaGloss j es (is !! x) : converteJogadoresParaGloss (x+1) js es is

-- | Função que converte cada Jogador num Estado Gloss, colocando-o nas proporçoes e posições certas no ecrã tendo em conta o mapa em que se encontra.
converteUmJogadorParaGloss :: Jogador -> Estado -> Picture -> EstadoIndividualGloss
converteUmJogadorParaGloss (Jogador (c,l) _ 0 _ _) es i   = ((0,0),Blank)
converteUmJogadorParaGloss (Jogador (c,l) dir _ _ _) es i = (p ,(scale (escalaCerta (mapaEscolhidinho es)) (escalaCerta (mapaEscolhidinho es)) (imagemrodada)))
                                                          where a = ((-(larguraMapa (mapaEscolhidinho es))) / 2) + ((escalaCerta (mapaEscolhidinho es))*100)
                                                                b = ((alturaMapa (mapaEscolhidinho es)) / 2) - ((escalaCerta (mapaEscolhidinho es))*100)
                                                                x = ((intToFloat l) * ((escalaCerta (mapaEscolhidinho es))*100))
                                                                y = ((intToFloat (-c)) * ((escalaCerta (mapaEscolhidinho es))*100))
                                                                p = somaVetoresFloat (x,y) (a,b)
                                                                imagemrodada | dir == B = i
                                                                             | dir == E = rotate 90 i
                                                                             | dir == C = rotate 180 i
                                                                             | otherwise = rotate 270 i





-- | Função que converte um conjunto de Disparos numa Lista de Estados Gloss.
converteDisparosParaGloss :: [Disparo] -> Estado -> [[Picture]] -> [Jogador] -> [EstadoIndividualGloss]
converteDisparosParaGloss [] es _ jog      = []
converteDisparosParaGloss (d:ds) es im jog = converteUmDisparoParaGloss d es im jog : converteDisparosParaGloss ds es im jog

-- | Função que converte cada Disparo num Estado Gloss, colocando-o nas proporçoes e posições certas no ecrã tendo em conta o mapa em que se encontra.
converteUmDisparoParaGloss :: Disparo -> Estado -> [[Picture]] -> [Jogador] -> EstadoIndividualGloss
converteUmDisparoParaGloss (DisparoCanhao ind (c,l) dir) es img jog = (g, (scale (escalaCerta (mapaEscolhidinho es)) (escalaCerta (mapaEscolhidinho es)) (imagemPosCerta)))
                                                                    where a = ((-(larguraMapa (mapaEscolhidinho es))) / 2) + ((escalaCerta (mapaEscolhidinho es))*100)
                                                                          b = ((alturaMapa (mapaEscolhidinho es)) / 2) - ((escalaCerta (mapaEscolhidinho es))*100)
                                                                          x = ((intToFloat l) * ((escalaCerta (mapaEscolhidinho es))*100))
                                                                          y = ((intToFloat (-c)) * ((escalaCerta (mapaEscolhidinho es))*100))
                                                                          g = somaVetoresFloat (x,y) (a,b)
                                                                          imagemPosCerta | dir == B = ((img!!0)!!ind)
                                                                                         | dir == E = rotate 90 ((img!!0)!!ind)
                                                                                         | dir == C = rotate 180 ((img!!0)!!ind)
                                                                                         | otherwise = rotate 270 ((img!!0)!!ind)

converteUmDisparoParaGloss (DisparoLaser ind (c,l) dir) es img jog = (g, (scale (escalaCerta (mapaEscolhidinho es)) (escalaCerta (mapaEscolhidinho es)) (imagemPosCerta)))
                                                                   where a = ((-(larguraMapa (mapaEscolhidinho es))) / 2) + ((escalaCerta (mapaEscolhidinho es))*100)
                                                                         b = ((alturaMapa (mapaEscolhidinho es)) / 2) - ((escalaCerta (mapaEscolhidinho es))*100)
                                                                         x = ((intToFloat l) * ((escalaCerta (mapaEscolhidinho es))*100))
                                                                         y = ((intToFloat (-c)) * ((escalaCerta (mapaEscolhidinho es))*100))
                                                                         g = somaVetoresFloat (x,y) (a,b)
                                                                         imagemPosCerta | dir == D = ((img!!1)!!ind)
                                                                                        | dir == B = rotate 90 ((img!!1)!!ind)
                                                                                        | dir == E = rotate 180 ((img!!1)!!ind)
                                                                                        | otherwise = rotate 270 ((img!!1)!!ind)

converteUmDisparoParaGloss (DisparoChoque ind tick) es img jog = (g, (scale (escalaCerta (mapaEscolhidinho es)) (escalaCerta (mapaEscolhidinho es)) ((img!!2)!!ind)))
                                                               where a = ((-(larguraMapa (mapaEscolhidinho es))) / 2) + ((escalaCerta (mapaEscolhidinho es))*100)
                                                                     b = ((alturaMapa (mapaEscolhidinho es)) / 2) - ((escalaCerta (mapaEscolhidinho es))*100)
                                                                     y = ((intToFloat (-(fst (posicaoJogador (jog!!ind)))))*((escalaCerta (mapaEscolhidinho es))*100))
                                                                     x = ((intToFloat (snd (posicaoJogador (jog!!ind))))*((escalaCerta (mapaEscolhidinho es))*100))
                                                                     g = somaVetoresFloat (x,y) (a,b)






-- FUNÇÕES AUXILIARES:


-- Funções auxiliares acerca de posições dos Jogadores:
-- | Função auxiliar que recebe um conjunto de Jogadores e utiliza a função seguint de modo a receber uma Posição Grelha
posicaoJogadore :: Int -> [Jogador] -> PosicaoGrelha
posicaoJogadore 0 (x:xs) = posicaoGrelhaJogador x
posicaoJogadore y (x:xs) = posicaoJogadore (y-1) xs
-- | Função auxiliar que recebe um Jogador e devolve a sua Posição Grelha
posicaoGrelhaJogador :: Jogador -> PosicaoGrelha
posicaoGrelhaJogador (Jogador (a,b) _ _ _ _) = (a,b)


-- Funções auxiliares utilizadas para descobrir o numero de vidas de jogadores, quantos ainda se encontram vivos, e no caso de haver apenas um vivo, qual o seu índice:
-- | Função que descobre o numero de Jogadores vivos
desbreNumerosDeJogadoresVivos :: [Jogador] -> Int
desbreNumerosDeJogadoresVivos []     = 0
desbreNumerosDeJogadoresVivos (x:xs) | (numeroVidasJogador x == 0) = desbreNumerosDeJogadoresVivos xs
                                     | otherwise = 1 + desbreNumerosDeJogadoresVivos xs
-- | Função que descobre qual o índice do Jogador que ainda se encontra vivo
descobreOJogadoorVivo :: [Jogador] -> Int
descobreOJogadoorVivo (x:xs) = auxiliarPraDescobrir 0 (x:xs)
-- | Função auxiliar para o descobrimento do Jogador que ainda se encontra vivo
auxiliarPraDescobrir :: Int -> [Jogador] -> Int
auxiliarPraDescobrir n (x:xs) | (numeroVidasJogador ((x:xs)!!0)) == 0 = auxiliarPraDescobrir (n+1) xs
                              | otherwise = n
-- | Função auxiliar que recebe um Jogador e devolve o número das suas vidas
numeroVidasJogador :: Jogador -> Int
numeroVidasJogador (Jogador _ _ x _ _) = x



-- Funções auxiliares relativas aos número e aos tipos INT e FLOAT:
-- | Função auxiliar que permite transformar um Int num Float
intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)
-- | Função auxiliar que permite fazer a soma de dois pares de Floats e obter um par de Floats
somaVetoresFloat :: (Float,Float) -> (Float,Float) -> (Float,Float)
somaVetoresFloat (x,y) (z,w) = (x+z,y+w)


-- Funções auxiliares relativas ao Estado de um jogo e á recolha de informações sobre o mesmo:
-- | Função auxiliar que recebe um Estado e nos devolve o mapa desse Estado
mapaEscolhidinho :: Estado -> Mapa
mapaEscolhidinho (Estado map jog dis) = map


-- Funções auxiliares que nos fornecem informações sobre os pixeis e escalas, consoante o mapa de um daodo Estado:
-- | Função que calcula os pixeis que a largura que um Mapa ocupa, tendo em conta a escala deste mesmo mapa.
larguraMapa :: Mapa -> Float
larguraMapa mapa = ((intToFloat(length(head mapa)))*((escalaCerta mapa)*100))
-- | Função que calcula os pixeis que a altura que um Mapa ocupa, tendo em conta a escala deste mesmo mapa.
alturaMapa :: Mapa -> Float
alturaMapa mapa = ((intToFloat(length mapa))*((escalaCerta mapa)*100))
-- | Função que vai dar uma escala de forma a que os mapas ocupem no máximo 720 pixeis na sua altura.
escalaCerta :: Mapa -> Float
escalaCerta map = ((720/(intToFloat(length map)))/100)


-- Funções auxiliares relativas ao Estados Gloss e aos conjuntos dos mesmo (Listas e Matrizes):
-- | Função que desenha um tipo de Estado Gloss e nós devolver uma Picture.
desenhaEstadoGloss :: EstadoIndividualGloss -> Picture
desenhaEstadoGloss ((x,y),obj) = Translate x y obj
-- | Função que recebe uma Matriz (uma lista de listas) de Estadoss Gloss e os desenha devolvendo uma Picture.
desenhaMatrizEstadoGloss :: [[EstadoIndividualGloss]] -> Picture
desenhaMatrizEstadoGloss []     = Blank
desenhaMatrizEstadoGloss (x:xs) = pictures (map desenhaListaEstadoGloss (x:xs))
-- | Função que desenha uma Lista de Estados Gloss e os transforma numa Pictures.
desenhaListaEstadoGloss :: [EstadoIndividualGloss] -> Picture
desenhaListaEstadoGloss []     = Blank
desenhaListaEstadoGloss (x:xs) = pictures (map desenhaEstadoGloss (x:xs))