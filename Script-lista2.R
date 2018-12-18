#lista 2,
#limpado
rm(list=ls(all=TRUE))

#CARREGANDO ARQUIVO CBO2002_Familia.csv

require(gmodels)
require(ggplot2)
require(dplyr)
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
cor(destilados$x,destilados$y)


#3. Ajuste o modelo de regressa ̃o entre X e Y. Quais os valores ajustados para os coeficientes β0 (intercepto) e β1?
destilados_ajuste <- lm(y ~ x, data = destilados)

summary(destilados_ajuste) 
destilados_ajuste
confint(destilados_ajuste)
#Resp : 74.28 e 14.95


#4. A relacao linear entre o nıvel de hidrocarbonetos (X) e a pureza do oxigenio (Y)  e significativa?


#RESP: sim a relacao entre hidrocarbonetos e oxigenio e significativa


#5. Qual é o coeficiente de determinacao do modelo ? o que esse numero representa ?


#RESP: 0.9367154 e uma correlação linear forte.


#6. Faça a analise de residuos e conclua se o modelo ajustado e adequado para o conjunto de dados.
require(nortest)
residuos <- destilados_ajuste$residuals
ad.test(residuos)

#RESP: REJEITADO DEVIDO AO APLHA SER MENOR QUE O PVALOR
#7. Qual é o valor predito da pureza do oxigenio para um nivel de hidrobarnonetos igual a 1,08 ?
confint(destilados_ajuste)
#valor <- data.frame(x=c(1.08))
#print(valor)

predict(object = destilados_ajuste, newdata = data.frame(x = c(1.08)))



#Resp: 90.42659


######################################################################################################
#EXERCICIO 2
#O arquivo dados salario.csv cont ́em informa ̧co ̃es de sala ́rio-hora (em do ́lares), educa ̧ca ̃o (em anos de e
#estudo), idade (em anos), sexo e idioma de 3.987 canadenses.

rm(list=ls(all=TRUE))

#CARREGANDO PACOTES
require(gmodels)
require(ggplot2)
require(dplyr)
require(nortest)

#CARREGANDO ARQUIVO dados_salario.csv

#Setando diretorio de trabalho
setwd("/Users/cassio/Dropbox/Pos Data Science/Lista_2_Estatistica")

#Carregando base de dados
salarios <- read.csv("dados_salario.csv", sep = ",", header = TRUE, na.strings = "")
View(salarios)


#1. Construa gra ́ficos de dispersa ̃o entre o sal ́ario e as varia ́veis educa ̧c ̃ao e idade. Calcule os respectivos coeficientes de correla ̧c ̃ao.
plot(salarios$educacao, salarios$salario ,main = "Grafico - salario x educacao")
cor(salarios$educacao, salarios$salario)

plot(salarios$idade, salarios$salario ,main = "Grafico - salario x idade")
cor(salarios$idade, salarios$salario)


#2. Construa boxplots de sal ́ario por sexo e por idioma. Analise os box- plots.
boxplot(salario ~ idioma, data = salarios, col = "blue")

boxplot(sexo ~ idioma, data = salarios, col = "blue")


boxplot(salarios$salario, salarios$sexo)

?boxplot
#3.Ajuste o modelo de regressa ̃o linear mu ́ltipla. Quais covari ́aveis s ̃ao significativas para explicar o sala ́rio?
modelo<- lm(salarios)
modelo

#Resp: Genero e educacao sao significativas para justicar o salario.

#4. Interprete os coeficientes significativos.

#Resp: os coeficientes de genero e educacao foram relevantes para justificar o salario, onde quanto maior os coeficientes maior o salario


#5. Qual  ́e o coeficiente de determina ̧ca ̃o do modelo? O que esse nu ́mero representa?
#Resp: -7.888.78

#6. Fa ̧ca a an ́alise de res ́ıduos e conclua se o modelo ajustado  ́e adequado para o conjunto de dados.
require(nortest)

residuos <- modelo$residuals
ad.test(residuos)

#Resp. Modelo adequado devido ao alpha ser maior que o pvalor

#7. Qual o sal ́ario m ́edio esperado para um trabalhador do sexo masculino com 35 anos de idade e 10 anos de escolaridade? E do sexo feminino?
novo_male <- data.frame(idade=c(35), genero="Male", educacao=c(10), idioma="Other")
predict(object = modelo, newdata = novo_male)

novo_female <- data.frame(idade=c(35), genero="Female", educacao=c(10), idioma="Other")
predict(object = modelo, newdata = novo_female)
