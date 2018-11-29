#lista 2
#CARREGANDO ARQUIVO CBO2002_Familia.csv

#Setando diretorio de trabalho
setwd("/Users/cassio/Dropbox/Pos Data Science/Lista_2_Estatistica")

#Carregando base de dados
destilados <- read.csv("dados_dest.txt", sep = "\t", header = TRUE,na.strings = '')

View(destilados)


#1. Construa o gra ́fico de dispersa ̃o entre o n ́ıvel de hidrocarbonetos (X) e a pureza do oxigˆenio (Y).
#Analisando o gr ́afico, o que podemos concluir sobre a relac ̧ ̃ao entre X e Y?