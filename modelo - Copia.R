### PROJETO DE PREVISAO DE DISPAROS FALSOS ####



################################# SELECIONANDO OS PACOTES NECESSÁRIOS NO PROJETO ####################################


library(dplyr)
library(explore)
library(ggplot2)
library(lubridate)
library(randomForest)
library(caret)


################################# ABRINDO OS ARQUIVOS DE TREINO E TESTE #############################################

setwd("D:/ohbto/Documents/FormacaoCientistaDados/ProjetoSegware") #selecionando o diretorio de trabalho

#Lendo os arquivos de treino e de teste
train <- read.csv("train.csv", header = T, stringsAsFactors = F, sep = ";", col.names = 
                    c("CodCliente","NivelRisco","servidorCFTV","tipoPessoa","Estado",
                      "Cidade","Bairro","DataHora","CodigoEvento","Confirmado"))
test <- read.csv("test.csv", header = T, stringsAsFactors = F, sep = ";", col.names = 
                   c("CodCliente","NivelRisco","servidorCFTV","tipoPessoa","Estado",
                     "Cidade","Bairro","DataHora","CodigoEvento"))

################################# EXPLORANDO OS DADOS ###############################################################

describe(train)
#Pelo método describe, nota-se que as variáveis abaixo são do tipo fator. Portanto, vamos
#alterar. As variaveis CodCliente e Bairro possuem muitos valores unicos e os modelos em R podem nao conseguir suportar
#Além disso, nenhum campo é nulo, o que é um bom indicativo

dataset <- train #variavel dataset recebe dados de treino, apenas para não perdermos os dados carregados inicialmente

dataset$NivelRisco <- factor(dataset$NivelRisco,levels = c(1,2,3,4,5), ordered = T)
dataset$servidorCFTV <- as.factor(dataset$servidorCFTV)
dataset$tipoPessoa <- as.factor(dataset$tipoPessoa)
dataset$Cidade <- as.factor(dataset$Cidade)

#vamos garantir que na conversão de CodigoEvento para fator, os niveis dos dados de treino e de teste tenham o mesmo
#tamanho

CodigosEnvento <- unique(append(dataset$CodigoEvento,test$CodigoEvento))
dataset$CodigoEvento <- factor(dataset$CodigoEvento, levels = CodigosEnvento)

dataset$Confirmado <- as.factor(dataset$Confirmado)

#Já DataHora trata-se de um campo Date
#Transformando em formato de data
dataset$DataHora <- as.POSIXct(dataset$DataHora)

dataset$Estado <- NULL #como tempos apenas 1 estado, não faz sentido manter a informação para o modelo

#Para o caso da variável bairro, vamos desconsiderá-la, pois a variavel de área de risco já faz o papel dessa informação
dataset$Bairro <- NULL

describe(dataset)

#vamos verificar qual o volume dos dados do tipo Confirmado e Não Confirmado
dataset%>%group_by(Confirmado)%>%summarise(Qtde = n())%>%mutate(Percent = 100*Qtde/sum(Qtde))
#Portanto, apenas 10% dos casos são confirmado, dessa forma os dados estão desbalanceados

source("ToolsSegware.r") #carregando o arquivo de ferramentas com algumas funções auxiliares

#defindo quem são as variáveis do tipo fator para fazer uma análise exploratória em gráfico de barra
variaveis_fator <- c("NivelRisco","servidorCFTV","tipoPessoa","Cidade",
                     "CodigoEvento")

target <- "Confirmado"

#Agora, vamos plotar graficos de barra para cada variável do conjunto de dados e verificar como elas se comportam
#para cada tipo de classificação
lapply(variaveis_fator, function (x) barplotAll(dataset,target,x))
#Pelo análise dos gráficos de barra, os níveis 4 e 5 são aqueles que mais causam casos, tanto não confirmados quanto confirmados
#Pelo gráfico, que possui o servidor CFTV possui um equilíbrio maior entre alarmes falsos e alarmes verdadeiros, 
# diferente de quem nao tem

#Vamos verificar o comportamento da variavel Confirmado a partir de CodCliente
ggplot(dataset, aes(CodCliente)) +
  geom_histogram() + facet_wrap(. ~ Confirmado)

#Existem alguns códigos de clientes que geram muitos alarmes falsos, podendo isso identificar outliers, enquanto que os casos
# que clientes que tem alarmes de verdade são bem reduzidos. Além disso, nota-se que clientes com codigos maiores 
# (provavel que mais novos), tendam a gerar mais alarmes falsos




###################################### DATA MUNGING + EXPLORAÇÃO DE DADOS ##########################################


#Gerando novas colunas para a análise de dados

#Primeiro, vamos gerar colunas de dias da semana e horários
niveis_dia_semana <- c("domingo","segunda-feira","terça-feira","quarta-feira","quinta-feira","sexta-feira","sábado")
niveis_PeriodoDia <- c("Madrugada","Manha","Tarde","Noite")

#Temos apenas 3 dias do mês de janeiro, mas iremos pegar os dias da semana deste mês para deixar claro o dia
dataset$DiaSemana <- factor(weekdays(dataset$DataHora), levels = niveis_dia_semana)

#Também separaremos as horas dos eventos
dataset$hora <- as.factor(hour(dataset$DataHora))
dataset$PeriodoDia <- factor(ifelse(hour(dataset$DataHora)<6,"Madrugada",ifelse(hour(dataset$DataHora)>17, "Noite",
                                  ifelse(hour(dataset$DataHora)>11, "Tarde","Manha"))), levels = niveis_PeriodoDia)

#Vamos gerar uma coluna de DiaSemana + hora, pois o dia da semana junto a hora pode gerar boas informações
dataset$DiaSemanaHora <-factor(as.numeric(dataset$DiaSemana)*100 + as.numeric(dataset$hora)-1)

#Vamos gerar uma coluna de DiaSemana + hora + NivelCriticidade
dataset$DiaSemanaHoraNivelRisco <- factor(as.numeric(dataset$DiaSemana)*1000 + 
                                            as.numeric(dataset$hora)*10-10 + as.numeric(dataset$NivelRisco)+1)

variaveis_fator <- append(variaveis_fator, c("DiaSemana","hora", "DiaSemanaHora","DiaSemanaHoraNivelRisco","PeriodoDia"))

#explorando as novas variáveis
lapply(variaveis_fator, function (x) barplotAll(dataset,target,x))

dataset%>%group_by(CodCliente,DiaSemanaHora,Confirmado)%>%count()%>%arrange(desc(n))
#Por essa análise, percebe-se que o cliente 11687 interfere em 20% dos dados com casos não confirmados, a maioria
#acontecendo às terças-feiras, próximo às 19h. Vamos dar uma observada melhor no que acontece com esse e outros casos
#que o número de acontecimentos em sequencia é alto

View(dataset%>%filter(CodCliente == 11687)%>%arrange(DataHora))
View(dataset%>%filter(CodCliente == 11799)%>%arrange(DataHora))

#Nota-se que o intervalo de tempo entre um alarme e outro, muito curto, pode ser uma variável interessante
dataset <- dataset%>%arrange(CodCliente,DataHora)%>%group_by(CodCliente)%>%
    mutate(difTempo = (DataHora - lag(DataHora)))%>%mutate(difTempo = ifelse(is.na(difTempo),Inf,difTempo))

#Como as diferencas entre os tempos sao bem distintas para o caso que ocorre mais de uma vez nos 3 dias, vamos
#criar um fator entre tempos. A funcao quantize.num que está no pacote de ferramentas auxiliares irá gerar
#esses fatores dentro de limites de tempos

#A variavel breaks cria os ranges em segundos que serão considerados para a análise
breaks <- c(min(dataset$difTempo),10,100,1000,10000,max(dataset$difTempo)+1)
dataset$difTempo <- quantize.num(dataset$difTempo, breaks = breaks)

#A quantidade de fatores para as variaveis DiasemanaGora e DiamaSemanaHoraNivelRisco é elevada para rodar nos modelos em R
#Portanto, deveremos colocar em ranges, assim como foi feito para difTempo.

dataset$DiaSemanaHoraNivelRisco <- as.numeric(levels(dataset$DiaSemanaHoraNivelRisco))[dataset$DiaSemanaHoraNivelRisco]
dataset$DiaSemanaHora <- as.numeric(levels(dataset$DiaSemanaHora))[dataset$DiaSemanaHora]

dataset$DiaSemanaHora <- quantize.num(dataset$DiaSemanaHora, 
                                           minval = min(dataset$DiaSemanaHora),
                                           maxval = max(dataset$DiaSemanaHora),
                                           nlevs = 10)

dataset$DiaSemanaHoraNivelRisco <- quantize.num(dataset$DiaSemanaHoraNivelRisco, 
                                                minval = min(dataset$DiaSemanaHoraNivelRisco),
                                                maxval = max(dataset$DiaSemanaHoraNivelRisco),
                                                nlevs = 10)

# Para a variavel CodCliente, vamos normalizar os dados (deixando em média nula e desvio padrao 1)
media <- mean(dataset$CodCliente)
dp <- sd(dataset$CodCliente)
dataset <- dataset%>%ungroup%>%mutate(CodClienteNorm = (CodCliente-media)/dp)

#verificando os graficos de barras agora com a nova variavel difTempo
variaveis_fator <- append(variaveis_fator,"difTempo")
lapply(variaveis_fator, function (x) barplotAll(dataset,target,x))

# Pelos graficos de barra, a informação nova de DiaSemanaHora e DiaSemanaHoraNivelRisco é a mesma, portanto, vamos 
#desconsiderar uma delas

dataset$DiaSemanaHoraNivelRisco = NULL


###################################### FEATURE SELECTION ##########################################

#Vamos agora selecionar as melhores variáveis para o modelo de classificação a partir de uma randomForest
featureSelection <- randomForest(Confirmado ~ . - CodCliente, data = dataset, ntree = 5000, importance = T)
Importancia <- varImpPlot(featureSelection)
Importancia

# Pelos graus de importancia, vamos selecionar as variaveis mais relevantes para o modelo
variaveis_relevantes <- c("CodClienteNorm","tipoPessoa","CodigoEvento","difTempo","NivelRisco","hora","servidorCFTV")


###################################### SEPARAR DADOS DE TREINO E DE TESTE ########################

#Vamos separar o dataset em dados de treino (70%) e validação (30%), para podermos medir o grau de acerto do modelo

index_treino <- sample(1:nrow(dataset),round(0.7*nrow(dataset)))
dataset_treino <- dataset[index_treino,]
dataset_validacao <- dataset[-index_treino,]


##################################### RODANDO O MODELO ###########################################


modelo1 <- randomForest(as.formula(paste("Confirmado","~",paste(variaveis_relevantes, collapse = " + "))),data=dataset_treino)
confusionMatrix(predict(modelo1,dataset_validacao),dataset_validacao$Confirmado, mode = "everything")


##################################### NOVO DATA MUNGING #########################################


#O modelo acerta 100% dos casos de validação, o que é anormal. Isso pode estar ocorrendo por conta da utilização da 
#variável CodCliente. Vamos fazer um tratamento diferente: vamos remover os indivíduos que possam ser considerados
#como outliers pela quantidade de vezes que acionam o alarme, desconsiderando depois a informação do COdCliente

#Montando a tabela que possui os CodClientes e quantidade de aparições
CodClienteQtde <- dataset%>%group_by(CodCliente)%>%count()%>%ungroup

# definindo percentis de corte
min_quartil <- quantile(CodClienteQtde$n,0.05)
max_quartil <- quantile(CodClienteQtde$n,0.95)

#selecionando apenas os CodClientes dentro dos percentis de corte
CodClienteQtde <- CodClienteQtde%>%filter(n >= min_quartil & n <= max_quartil)

#filtrando os CodClientes selecionados pelo corte
dataset <- merge(dataset,CodClienteQtde)
dataset$n <- NULL

###################################### NOVO FEATURE SELECTION ##########################################

#Vamos agora selecionar as melhores variáveis para o modelo de classificação a partir de uma randomForest
featureSelection <- randomForest(Confirmado ~ . - CodCliente - CodClienteNorm, data = dataset, ntree = 5000, importance = T)
Importancia <- varImpPlot(featureSelection)
Importancia

# Pelos graus de importancia, vamos selecionar as variaveis mais relevantes para o modelo
variaveis_relevantes2 <- c("DataHora", "hora","tipoPessoa","CodigoEvento","difTempo","NivelRisco","hora","servidorCFTV")


###################################### NOVA SEPARAÇÃO DOS DADOS ################################


#Vamos separar o dataset em dados de treino (70%) e validação (30%), para podermos medir o grau de acerto do modelo

index_treino <- sample(1:nrow(dataset),round(0.7*nrow(dataset)))
dataset_treino <- dataset[index_treino,]
dataset_validacao <- dataset[-index_treino,]


##################################### RODANDO NOVO MODELO ###########################################


modelo2 <- randomForest(as.formula(paste("Confirmado","~",paste(variaveis_relevantes2, collapse = " + "))),
                        ntree = 3000,data=dataset_treino)
confusionMatrix(predict(modelo2,dataset_validacao),dataset_validacao$Confirmado, mode = "everything")
# Neste modelo, dos 141 casos de validação, 126 foram verdadeiros negativos, 8 verdadeiros positivos
# 4 falsos positivos e 3 falsos negativos

##################################### PREPARANDO OS DADOS DE TESTES #################################

#Vamos criar as variaveis necessárias criadas durante o processo de data munging para rodar os modelos
#ao conjunto de teste

#Predizendo os dados de teste com os dois modelos

teste <- test

teste$DataHora <- as.POSIXct(teste$DataHora)
teste$hora <- as.factor(hour(teste$DataHora))

teste$NivelRisco <- factor(teste$NivelRisco,levels = c(1,2,3,4,5), ordered = T)
teste$servidorCFTV <- as.factor(teste$servidorCFTV)
teste$tipoPessoa <- as.factor(teste$tipoPessoa)
teste$Cidade <- as.factor(teste$Cidade)
teste$CodigoEvento <- factor(teste$CodigoEvento, levels = CodigosEnvento)

teste <- teste%>%arrange(CodCliente,DataHora)%>%group_by(CodCliente)%>%
  mutate(difTempo = (DataHora - lag(DataHora)))%>%mutate(difTempo = ifelse(is.na(difTempo),Inf,difTempo))%>%ungroup%>%
  mutate(CodClienteNorm = (CodCliente-media)/dp)

teste$difTempo <- quantize.num(teste$difTempo, breaks = breaks)


####################################### PREVISÃO DOS DADOS DE TESTE ###############################

#usaremos a predicao do modelo 2 e escreveremos o resultado no arquivo csv resposta 
teste$PredicaoModelo2 <- predict(modelo2,teste)
write.csv(teste,file = "resposta.csv")
