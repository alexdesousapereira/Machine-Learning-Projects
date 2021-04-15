#########################################################################
#############################Aula 03####################################
########################################################################

#Carregando Pacotes

library(readxl)#Pacote importação Excel
library(MASS) #Pacote para Análise de Regressão (Modelagem Linear)
library(lmtest) #Pacote de Testes
library(car) #Pacote para realizar teste de multicolinearidade
library(forecast)#Pacote para utilizar função Box.Cox

#Importação Dados utilizando a opção "Import Dataset" (PublicidadeVendas)
#Importação Dados PublicidadeVendas

PublicidadeVendas=read_excel("C:\\Users\\alefr\\Desktop\\MESTRADO Alef\\Análise de Dados\\Métodos Quantitativos com Auxilio de Software\\ModuloMetodosQuantitativos\\dados\\PublicidadeVendas.xlsx")

attach(PublicidadeVendas) #comando para poder acessar cada coluna do DataFrame

#Estatísticas Descritivas

summary(Publicidade)
sd(Publicidade)

summary(Vendas)
sd(Vendas)

#Diagrama de Dispersão

plot(Publicidade,Vendas,xlab="Publicidade",ylab="Vendas")

#Correlação Linear

cor(Publicidade,Vendas)

# O R retornou o valor de 0.9011918 indicando forte relação linear entre as variaveis

###Obtendo Estimadores de Minimos dos Quadrados###

modelo1=lm(Vendas~Publicidade) #lm(y~x)
modelo1

#Teste para a validade da Regressão

summary(modelo1)

#No output observamos as estimativas dos parâmetros, o erro padrão
#associado a cada estimativa, uma estatística t e um p-valor associado.
# O resultado do teste t  é utilizado para saber se as estimativas
#são realmente diferentes de zero. Quanto mais asteriscos presentes 
#ao lado do efeito estimado, maior o nível de confiança com que podemos
#afirmar que o efeito não é nulo.

#Quanto ao R², ao utilizar apenas uma variável é normal que o valor não 
#seja extremamente alto. Nesse caso o R²=0,8121 é extremamente relevante

# Qualidade do Ajuste
#Verificar o quão distantes estão as observações reais da reta ajustada

plot(Publicidade,Vendas,xlab="Publicidades",ylab="Vendas")
abline(modelo1)

#Obtendo intervalos de confiança para Beta0 e Beta1

confint(modelo1)

#####ANÁLISE DE RESÍDUOS########

par(mfrow=c(2,2))
plot(modelo1,wich=c(1:4))

####Teste de Breusch-Pagan#######
####Hipoteses Estatísticas####
#H0:Resíduos Homocedasticos (variância Constante) p<0.05
#H1:Resíduos NÃO Homocedasticos p=>0.05
# ao um nivel de significância de 5% vamos a pré suposição será confirmada
#se o p-valor form maior que 0,05

bptest(Vendas~Publicidade)

# A nível de 5% de significancia,
#como p-value=  0.07492 Aceita-se H0 (adotamos aqui o critério de 10% de significancia devido ao tamanho da amostra e também pelo valor de p estar próximo de 0.05), 
#ou seja os dados SÂO homocedasticos

#Teste de Normalidade
#Teste Shapiro-Wilk

shapiro.test(modelo1$residuals)

#como p-value  = 0,7731 Aceita-se H0, 
#ou seja os residuos SÃO normais (se p<0.05 eu rejeitava H0, resíduos não seriam normais)

#Teste de Independência dos respiduos
#H0: Resíduos Independentes (p>0.05)

Box.test(modelo1$residuals,type=c("Ljung-Box"))

#como p-value  = 0.5036 ACEITA sE H0, 
#ou seja os residuos SÃO Independentes


#Uma vez verificado todas as pré-suposições podemos então utilizar
#o modelo ajustado para fazer previsões

#Vamos contruir gráficos com as estimativas pontuais e intervalares
#das respostas médias e individuais:

#Gráfico Observação média

x0=Publicidade
p1 = predict(modelo1,interval="confidence",se=T)
p1 #Valores previstos para Vendas (fit) juntamente como os limites inferior e superior
matplot(x0,p1$fit,lty=c(1,2,2),type="l",xlab="Publicidades",ylab="Vendas")
points(Publicidade,Vendas, type = "p")

##########################REGRESSÃO MULTIPLA###################

#Dados sobre o produto bruto real(Y), trabalho(X1) e capital real(X2) 
#no setor industrial de Taiwan

#Importando Dados
#Taiwan.xlsx

Taiwan=read_excel("C:\\Users\\alefr\\Desktop\\MESTRADO Alef\\Análise de Dados\\Métodos Quantitativos com Auxilio de Software\\ModuloMetodosQuantitativos\\dados\\Taiwan.xlsx")

#Análisar se a variável produto bruto real(Y) pode ser explicada pelas outras  variáveis

attach(Taiwan)

Reg<- lm(Y~X1+X2) #Comando Regressão Multipla (Mais de uma Variavél Independente)
summary(Reg) #lm(y~x1)... Faria isso pois X2 não é significativo
anova(Reg)


#Teste de Homocedasticidade
# Teste de Breush-Pagan

####Hipoteses Estatísticas####
#H0:Resíduos Homocedasticos (variância Constante)
#H1:Resíduos NÃO Homocedasticos 
# ao um nivel de significância de 5% vamos a pré suposição será confirmada
#se o p-valor form MAIOR que 0,05


bptest(Y~X1+X2 )

#como p-value=  0.05071 (apesar de muito proximo de 0,05) rejeita-se H0,
#ou seja os dados NÃO são homocedasticos

#Teste de Normalidade
#Teste Shapiro-Wilk

####Hipoteses Estatísticas####
#H0:Resíduos Seguem uma distribuição normal
#H1:Resíduos NÃO Seguem uma distribuição normal
# ao um nivel de significância de 5% vamos a pré suposição será confirmada
#se o p-valor form MAIOR que 0,05

shapiro.test(Reg$residuals)

#como p-value  = 0.3117  aceita sE H0, ou seja os residuos são normais

#TEste Independência dos Residuos

####Hipoteses Estatísticas####
#H0:Resíduos SÃO independentes
#H1:Resíduos NÃO são independentes
#ao um nivel de significância de 5% vamos a pré suposição será confirmada
#se o p-valor form MAIOR que 0,05


Box.test(Reg$residuals, type = c("Ljung-Box"))

#como p-value  = 0.03403  rejeita se H0, ou seja os residuos Não são Independentes (tem que ser)

####Utilizando a Transformação de Box-Cox para verificar melhor
#transformação nos dados###

#Primeira maneira (encontrar o valor de lambda por meio do grafico)

boxcox(Reg, plotit=T, lam=seq(-1, 1, 1/10))

#O gráfico mostra que a função que maximiza a função é aproximadamente 0.1, logo:

lambda=0.1
YT=((Y^lambda)-1)/lambda # ytransf=((y^lambda)-1)/lambda

#Regressão com dados transformados####

Reg2<- lm(YT~X1+X2)
summary(Reg2)
anova(Reg2)

##############vERIFICANDO PRÉ SUPOSIÇÕES DOS RESÍDUOS#########

par(mfrow=c(2,2))
plot(Reg2,which = c(1:4),pch=20)

#Teste de Homocedasticidade
# Teste de Breush-Pagan

bptest(YT~X1+X2 )

#como p-value= 0.5921 aceita-se H0, ou seja os dados são homocedasticos

#Teste de Normalidade
#Teste Shapiro-Wilk

shapiro.test(Reg2$residuals)

#como p-value=0.3546 Aceita SE H0, ou seja os residuos são normais

#TEste Independência dos Residuos

#H0: Residuos independentes

Box.test(Reg2$residuals, type = c("Ljung-Box"))

#como p-value  = 0.152  Aceita sE H0, ou seja os residuos são Independentes

###Verificando multicolinearidade###
#A multicolinearidade é um problema no ajuste do modelo que pode causar
#impactos na estimativa dos parâmetros. 
#Podemos diagnosticar Multicolinearidade por meio do 
#VIF (Variance Inflation Factor).

vif(Reg2) # o VIF>10  é indicativo de problemas de multicolinearidade
