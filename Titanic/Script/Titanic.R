#########################################################################
#############################Aula 05####################################
########################################################################

########Regressão Logistica########

#Carregando Pacotes

library(readxl)
library(lmtest)
library(MASS)
library(car)

#Importando Dados
#Titanic 
#Banco de dados com 891 passageiros

Titanic=read.csv("https://raw.githubusercontent.com/alexdesousapereira/Machine-Learning-Projects/main/Titanic/Dados/Titanic.csv")

#Desses 891 passageiro falta informação de idade para 177 passageiro
#Vamos retirar do nosso data.frame essas observações

Titanic=subset(Titanic, !is.na(Age))

#Definindo a Variável Dummy para o Sexo
#(Como tem duas categoria será criado apenas uma dummy)


n=length(Titanic$Sex)

DFeminino=rep(0,n)
#0 masculino
#1 Feminino

for(i in 1:n){
  if(Titanic$Sex[i]=="female"){
    DFeminino[i]=1
  } else{
    DFeminino[i] =0
  }
}

#Definindo Variável Dummy para Classe que viajou
#(Como tem três categoria será criado duas variáveis dummies)

D1Classe=rep(0,n)
#1 passageiro da 1ªClasse
#0 caso contrario

for(i in 1:n){
  if(Titanic$Pclass[i]==1){
    D1Classe[i]=1
  } else{
    D1Classe[i]=0
  }
}

D2Classe=rep(0,n)
#1 passageiro da 2ªClasse
#0 caso contrario

for(i in 1:n){
  if(Titanic$Pclass[i]==2){
    D2Classe[i]=1
  } else{
    D2Classe[i] =0
  }
}

#Passageiro da 3ª Classe será quando D1Classe=0 e D2Classe=0

#Incluindo as variáveis Dummies no conjunto de dados
Titanic = cbind(Titanic,DFeminino,D1Classe,D2Classe) 

attach(Titanic)

#####Regresão Logística#######
RegLogit=glm(Survived~Age+DFeminino+D1Classe+D2Classe,family=binomial(link="logit"))
summary(RegLogit)

#Razões de chance (Odds Ratio) e os seus respectivos IC(95%)

OR=exp(RegLogit$coefficients)
ICbeta=confint.default(RegLogit,level=0.95)
ICOR=exp(ICbeta)
round((cbind(OR, ICOR)),3)

#Avaliando Qualidade do Modelo

#Classificação do Modelo
Classif=table(Survived,predict(RegLogit,type = "response")>0.5)
Classif


VN=Classif[1,1]
FP=Classif[1,2]
FN=Classif[2,1]
VP=Classif[2,2]
Total=VN+FP+FN+VP

#ACURÁCIA:Razão das predições mais corretas do total de observações
#Taxa de acerto geral do modelo
acuracia=(VP+VN)/Total

#Sensibilidade:Quando a classificação é realmente "Sim" e o quanto ocorreu "Sim"
#Capacidade do modelo de prever que o índividuo iria sobreviver e realmente ele sobreviveu
Sensibilidade=VP/(VP+FN)

#Especificidade: Quando a classificação é realmente "Não" e o quanto ocorreu "Não" 
#Capacidade do modelo de prever que o índividuo Não  sobreviveria e realmente ele  Não sobreviveu
Especificidade=VN/(VN+FP)

round(acuracia,4)
round(Sensibilidade,4)
round(Especificidade,4)

#TEste Independência dos Residuos

#H0: Residuos independentes

Box.test(RegLogit$residuals, type = c("Ljung-Box"))

#como p-value  = 0.3059  Aceita sE H0, ou seja os residuos são Independentes
