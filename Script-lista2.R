#lista 2
#CARREGANDO ARQUIVO CBO2002_Familia.csv

#Setando diretorio de trabalho
setwd("/Users/cassio/Dropbox/Pos Data Science/Lista_2_Estatistica")

#Carregando base de dados
destilados <- read.csv("dados_dest.txt", sep = "\t", header = TRUE,na.strings = '')

View(destilados)


#1. Construa o gra ́fico de dispersa ̃o entre o n ́ıvel de hidrocarbonetos (X) e a pureza do oxigˆenio (Y).
#Analisando o gr ́afico, o que podemos concluir sobre a relac ̧ ̃ao entre X e Y?
plot(destilados, xlab = "Nivel Hidrocarbonetos", ylab = "Pureza Oxigenio")



#2. Calcule o coeficiente de correlac̃ao de Pearson entre X e Y
# Qual a interpretac ̧ ̃ao sobre o coeficiente? Ele confirma a sua resposta do item 1?
abline(lm(destilados$y~destilados$x), h=0,col="blue")


#3. Ajuste o modelo de regressa ̃o entre X e Y. Quais os valores ajustados para os coeficientes β0 (intercepto) e β1?
destilados_ajuste=lm(destilados$y~destilados$x)
destilados_ajuste
#Resp : 74.28 e 14.95

#4. A relacao linear entre o nıvel de hidrocarbonetos (X) e a pureza do oxigenio (Y)  e significativa?



#5. Qual e o coeficiente de determinacao̧ do modelo? O que esse numero representa?


#6. Fa ̧ca a an ́alise de res ́ıduos e conclua se o modelo ajustado  ́e adequado para o conjunto de dados.



#7. Qual  ́e o valor predito da pureza do oxigˆenio para um n ́ıvel de hidro- carbonetos igual a 1,08?
