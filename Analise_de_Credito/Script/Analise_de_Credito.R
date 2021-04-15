#*****************************************************************************
#
#               Microsoft Power BI Para Data Science, Versão 2.0
#
#                               Data Science Academy
#
#                                   Mini-Projeto 3
#       
#         Prevendo a Inadimplência de Clientes com Machine Learning e Power BI
#
#*****************************************************************************

# Definindo a pasta de trabalho
setwd("C:/Users/alexd/OneDrive/Meus Cursos/DataScienceAcadmy/Microsolft_Power_BI/Capútulo15")
getwd()

# Definição do Problema
# Leio o manual em pdf no Capituloo 15 do curso como a definicão do problema

# Instalando os pacotes para o projeto 
# Obs: os pacotes precisam ser instalados apenas uma vez
install.packages("Amelia") # funções valores ausentes
install.packages("caret") # contruir modelo mechine learning
install.packages("ggplot2") # gráficos
install.packages("dplyr") # tratar dados
install.packages("reshape") # manipulação
install.packages("randomForest") # machine learning
install.packages("e1071") # machine learning

# Carregando os pacotes 
library(Amelia)
library(ggplot2)
library(caret)
library(reshape)
library(randomForest)
library(dplyr)
library(e1071)

# Carregando o dataset
# Fonte: https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients
dados_clientes <- read.csv("dados/dataset.csv")

# Visualizando os dados e sua estrutura
View(dados_clientes)
dim(dados_clientes)
str(dados_clientes)  # resumo do conjunto de dados
summary(dados_clientes)


#################### Análise Exploratoria, Limpeza e Transformação ####################


# Removendo a primeira coluna ID
dados_clientes$ID <- NULL # porque ela é apenas uma indentificação
dim(dados_clientes)
View(dados_clientes)

# Renomeando a coluna de classe
colnames(dados_clientes)
colnames(dados_clientes)[24] <- "inadimplente"
colnames(dados_clientes)
View(dados_clientes)

# Verificando valores ausentes e removendo do dataset
sapply(dados_clientes, function(x) sum(is.na(x))) # identificando os valores ausentes
?missmap
missmap(dados_clientes, main = "Valores Missing Observados") # formula gráfica
dados_clientes <- na.omit(dados_clientes)

# Convertendo os atributos genero, escolaridade, estado civil e idade 
# para fatores (categorias)
str(dados_clientes) 

# Renomeando colunas categÃ³ricas
colnames(dados_clientes)
colnames(dados_clientes)[2] <- "Genero"
colnames(dados_clientes)[3] <- "Escolaridade"
colnames(dados_clientes)[4] <- "Estado_Civil"
colnames(dados_clientes)[5] <- "Idade"
colnames(dados_clientes)
View(dados_clientes)

# Genero
View(dados_clientes$Genero) 
str(dados_clientes$Genero) 
summary(dados_clientes$Genero) 
?cut
dados_clientes$Genero <- cut(dados_clientes$Genero, 
                             c(0,1,2), # sabemos que não tem mais valor 0
                             labels = c("Masculino",
                                        "Feminino"))
View(dados_clientes$Genero) 
str(dados_clientes$Genero) 
summary(dados_clientes$Genero) 

# Escolaridade
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade) 
dados_clientes$Escolaridade <- cut(dados_clientes$Escolaridade, 
                                   c(0,1,2,3,4), 
                                   labels = c("Pos Graduado",
                                              "Graduado",
                                              "Ensino Medio",
                                              "Outros"))
View(dados_clientes$Escolaridade) 
str(dados_clientes$Escolaridade) 
summary(dados_clientes$Escolaridade) 

#Note que temos valores NA nos dados. Vamos deixar para ver o que acontece futuramente

# Estado Civil
str(dados_clientes$Estado_Civil) 
summary(dados_clientes$Estado_Civil) 
dados_clientes$Estado_Civil <- cut(dados_clientes$Estado_Civil, 
                                   c(-1,0,1,2,3),
                                   labels = c("Desconhecido",
                                              "Casado",
                                              "Solteiro",
                                              "Outro"))
View(dados_clientes$Estado_Civil) 
str(dados_clientes$Estado_Civil) 
summary(dados_clientes$Estado_Civil) 

# Convertendo a variavel para o tipo fator com faixa etaria
str(dados_clientes$Idade) 
summary(dados_clientes$Idade) 
hist(dados_clientes$Idade)
dados_clientes$Idade <- cut(dados_clientes$Idade, 
                            c(0,30,50,100), 
                            labels = c("Jovem", 
                                       "Adulto", 
                                       "Idoso"))
View(dados_clientes$Idade) 
str(dados_clientes$Idade) 
summary(dados_clientes$Idade)
View(dados_clientes)

# Convertendo a variavel que indica pagamentos para o tipo fator
dados_clientes$PAY_0 <- as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_2 <- as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <- as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <- as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <- as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <- as.factor(dados_clientes$PAY_6)

# Dataset algumas conversões
str(dados_clientes) 
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = "Valores Missing Observados")
dados_clientes <- na.omit(dados_clientes) # remove valores ausentes
missmap(dados_clientes, main = "Valores Missing Observados")
dim(dados_clientes)
View(dados_clientes)

# Alterando a varia¡vel dependente para o tipo fator
str(dados_clientes$inadimplente) # tipo variável
colnames(dados_clientes)
dados_clientes$inadimplente <- as.factor(dados_clientes$inadimplente)#alterando tipo factor
str(dados_clientes$inadimplente)
View(dados_clientes)

# Total de inadimplentes versus não-inadimplentes
?table
table(dados_clientes$inadimplente)

# Vejamos as porcentagens entre as classes
prop.table(table(dados_clientes$inadimplente))

# Plot da distribuição usando ggplot2
qplot(inadimplente, data = dados_clientes, geom = "bar") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Set seed
set.seed(12345) # definindo uma semente

# Amostragem estratificada. 
# Seleciona as linhas de acordo com a variavel inadimplente como strata
?createDataPartition
indice <- createDataPartition(dados_clientes$inadimplente, p = 0.75, list = FALSE) #p= porcentagem dos dados retirados para o data set, list corresponde se quero uma lista
# create deta retorna o número de linhas que irei repartir meus dados
View(indice)
dim(indice)

# Definimos os dados de treinamento como subconjunto do conjunto de dados original
# com números de indice de linha (conforme identificado acima) e todas as colunas
dados_treino <- dados_clientes[indice,]
dim(dados_treino)
table(dados_treino$inadimplente)

# Veja as porcentagens entre as classes
prop.table(table(dados_treino$inadimplente))

# Numero de registros no dataset de treinamento
dim(dados_treino)

# Comparamos as porcentagens entre as classes de treinamento e dados originais
compara_dados <- cbind(prop.table(table(dados_treino$inadimplente)), 
                       prop.table(table(dados_clientes$inadimplente))) # junta as duas tabelas
colnames(compara_dados) <- c("Treinamento", "Original")
compara_dados

# Melt Data - Converte colunas em linhas
?reshape2::melt
melt_compara_dados <- melt(compara_dados)
melt_compara_dados

# Plot para ver a distribuição do treinamento vs original
ggplot(melt_compara_dados, aes(x = X1, y = value)) + 
  geom_bar( aes(fill = X2), stat = "identity", position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Tudo o que nÃo esta¡ no dataset de treinamento esta no dataset de teste. Observe o sinal - (menos)
dados_teste <- dados_clientes[-indice,]
dim(dados_teste)
dim(dados_treino)

#################### Modelo de Machine Learning ####################

# Construindo a primeira versão do modelo
?randomForest # modelo de regressão e classificação
View(dados_treino)
modelo_v1 <- randomForest(inadimplente ~ ., data = dados_treino)
modelo_v1

# Avaliando o modelo
plot(modelo_v1)

# PrevisÃµes com dados de teste
previsoes_v1 <- predict(modelo_v1, dados_teste)

# Confusion Matrix
?caret::confusionMatrix
cm_v1 <- caret::confusionMatrix(previsoes_v1, dados_teste$inadimplente, positive = "1")
cm_v1

# Calculando Precision, Recall e F1-Score, mÃ©tricas de avaliação do modelo preditivo
y <- dados_teste$inadimplente
y_pred_v1 <- previsoes_v1

precision <- posPredValue(y_pred_v1, y)
precision

recall <- sensitivity(y_pred_v1, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1

# Balanceamento de classe
install.packages('xts')
install.packages('quantmod')
install.packages('ROCR')
install.packages('abind')
install.packages('zoo')
install.packages("DMwR") # deveremos baixar este pacote por fora do R e depois carregalo
library(DMwR)
?SMOTE

# Aplicando o SMOTE - SMOTE: Synthetic Minority Over-sampling Technique
# https://arxiv.org/pdf/1106.1813.pdf
table(dados_treino$inadimplente)
prop.table(table(dados_treino$inadimplente))
set.seed(9560)
dados_treino_bal <- SMOTE(inadimplente ~ ., data  = dados_treino)                         
table(dados_treino_bal$inadimplente)
prop.table(table(dados_treino_bal$inadimplente))

# Construindo a segunda versõa do modelo
modelo_v2 <- randomForest(inadimplente ~ ., data = dados_treino_bal)
modelo_v2

# Avaliando o modelo
plot(modelo_v2)

# PrevisÃµes com dados de teste
previsoes_v2 <- predict(modelo_v2, dados_teste) # não preciso fazer balanceamento dados teste

# Confusion Matrix
?caret::confusionMatrix
cm_v2 <- caret::confusionMatrix(previsoes_v2, dados_teste$inadimplente, positive = "1")
cm_v2 # ele tem um equilibrio melhor apesar ter uma acuracia menor que o modelo 1. Então o modelo 2 é melhor

# Calculando Precision, Recall e F1-Score, mÃ©tricas de avaliaÃ§Ã£o do modelo preditivo
y <- dados_teste$inadimplente
y_pred_v2 <- previsoes_v2

precision <- posPredValue(y_pred_v2, y)
precision

recall <- sensitivity(y_pred_v2, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1

# Importância das variÃ¡veis preditoras para as previsões
View(dados_treino_bal)
varImpPlot(modelo_v2)

# Obtendo as variaveis mais importantes
imp_var <- importance(modelo_v2)
varImportance <- data.frame(Variables = row.names(imp_var), 
                            Importance = round(imp_var[ ,'MeanDecreaseGini'],2))

# Criando o rank de variÃ¡veis baseado na importância
rankImportance <- varImportance %>% 
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

# Usando ggplot2 para visualizar a importÃ¢ncia relativa das variaveis
ggplot(rankImportance, 
       aes(x = reorder(Variables, Importance), 
           y = Importance, 
           fill = Importance)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank), 
            hjust = 0, 
            vjust = 0.55, 
            size = 4, 
            colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() 

# Construindo a terceira versão do modelo apenas com as variaveis mais importantes
colnames(dados_treino_bal)
modelo_v3 <- randomForest(inadimplente ~ PAY_0 + PAY_2 + PAY_3 + PAY_AMT1 + PAY_AMT2 + PAY_5 + BILL_AMT1, 
                          data = dados_treino_bal)
modelo_v3

# Avaliando o modelo
plot(modelo_v3)

# Previsões com dados de teste
previsoes_v3 <- predict(modelo_v3, dados_teste)

# Confusion Matrix
?caret::confusionMatrix
cm_v3 <- caret::confusionMatrix(previsoes_v3, dados_teste$inadimplente, positive = "1")
cm_v3

# Calculando Precision, Recall e F1-Score, metricas de avaliação do modelo preditivo
y <- dados_teste$inadimplente
y_pred_v3 <- previsoes_v3

precision <- posPredValue(y_pred_v3, y)
precision

recall <- sensitivity(y_pred_v3, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1

# Salvando o modelo em disco
saveRDS(modelo_v3, file = "modelo/modelo_v3.rds")

# Carregando o modelo
modelo_final <- readRDS("modelo/modelo_v3.rds")

# Previsões com novos dados de 3 clientes

# Dados dos clientes
PAY_0 <- c(0, 0, 0) 
PAY_2 <- c(0, 0, 0) 
PAY_3 <- c(1, 0, 0) 
PAY_AMT1 <- c(1100, 1000, 1200) 
PAY_AMT2 <- c(1500, 1300, 1150) 
PAY_5 <- c(0, 0, 0) 
BILL_AMT1 <- c(350, 420, 280) 

# Concatena em um dataframe
novos_clientes <- data.frame(PAY_0, PAY_2, PAY_3, PAY_AMT1, PAY_AMT2, PAY_5, BILL_AMT1)
View(novos_clientes)

# Previsões
previsoes_novos_clientes <- predict(modelo_final, novos_clientes)

# Checando os tipos de dados
str(dados_treino_bal)
str(novos_clientes)

# Convertendo os tipos de dados
novos_clientes$PAY_0 <- factor(novos_clientes$PAY_0, levels = levels(dados_treino_bal$PAY_0))
novos_clientes$PAY_2 <- factor(novos_clientes$PAY_2, levels = levels(dados_treino_bal$PAY_2))
novos_clientes$PAY_3 <- factor(novos_clientes$PAY_3, levels = levels(dados_treino_bal$PAY_3))
novos_clientes$PAY_5 <- factor(novos_clientes$PAY_5, levels = levels(dados_treino_bal$PAY_5))
# converter só tipo fator não funciona, devemos converter também no mesmo nível dos
# dados de nosso modelo
str(novos_clientes)

# Previsõµes
previsoes_novos_clientes <- predict(modelo_final, novos_clientes)
View(previsoes_novos_clientes)

# Fim

# O que vimos aqui foi apenas uma breve demonstraÃ§Ã£o do que Ã© Machine Learning.
# Para aprender como construir e aplicar modelos de Machine Learning em diferentes problemas de negÃ³cio confira os demais cursos em nosso portal:
# https://www.datascienceacademy.com.br/pages/todos-os-cursos-dsa





