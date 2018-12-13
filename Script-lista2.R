#lista 2
#CARREGANDO ARQUIVO CBO2002_Familia.csv

require(gmodels)
require(ggplot2)
require(dplyr)
require(plyr)
require(nortest)

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
cor(destilados)

#3. Ajuste o modelo de regressa ̃o entre X e Y. Quais os valores ajustados para os coeficientes β0 (intercepto) e β1?
destilados_ajuste=lm(destilados$y~destilados$x)
destilados_ajuste
confint(destilados_ajuste)
#Resp : 74.28 e 14.95

#4. A relacao linear entre o nıvel de hidrocarbonetos (X) e a pureza do oxigenio (Y)  e significativa?



#5. Qual é o coeficiente de determinacao do modelo ? o que esse numero representa ?



#6. Faça a analise de residuos e conclua se o modelo ajustado e adequado para o conjunto de dados.
require(nortest)
residuos <- destilados_ajuste$residuals
ad.test(residuos)

#7. Qual é o valor predito da pureza do oxigenio para um nivel de hidrobarnonetos igual a 1,08 ?
confint(destilados_ajuste)
valor <- data.frame(hidrocarboneto=c(1.08))
print(valor)

predict(destilados_ajuste,carboneto=c(1.08))

summary(destilados_ajuste)




#5. Qual e o coeficiente de determinacao̧ do modelo? O que esse numero representa?


#6. Fa ̧ca a an ́alise de res ́ıduos e conclua se o modelo ajustado  ́e adequado para o conjunto de dados.
lm(formula = destilados$y ~ destilados$x)


#7. Qual  ́e o valor predito da pureza do oxigˆenio para um n ́ıvel de hidro- carbonetos igual a 1,08?

cor.test(destilados$y~destilados$x)



