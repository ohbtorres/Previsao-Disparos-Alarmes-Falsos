### PROJETO DE PREVISAO DE DISPAROS FALSOS ####



################################# SELECIONANDO OS PACOTES NECESSÁRIOS NO PROJETO ####################################


library(explore)
library(ggplot2)
library(lubridate)
library(randomForest)
library(caret)
library(data.table)

################################# ABRINDO OS ARQUIVOS DE TREINO E TESTE #############################################

setwd("D:/ohbto/Documents/FormacaoCientistaDados/ProjetoSegware") #selecionando o diretorio de trabalho

#Lendo os arquivos de treino e de teste
train <- fread("train.csv", header = T, stringsAsFactors = F, sep = ";", col.names = 
                    c("CodCliente","NivelRisco","servidorCFTV","tipoPessoa","Estado",
                      "Cidade","Bairro","DataHora","CodigoEvento","Confirmado"))
test <- fread("test.csv", header = T, stringsAsFactors = F, sep = ";", col.names = 
                   c("CodCliente","NivelRisco","servidorCFTV","tipoPessoa","Estado",
                     "Cidade","Bairro","DataHora","CodigoEvento"))

################################# EXPLORANDO OS DADOS ###############################################################

describe(train)
#Pelo método describe, nota-se que as variáveis abaixo são do tipo fator. Portanto, vamos
#alterar. As variaveis CodCliente e Bairro possuem muitos valores unicos e os modelos em R podem nao conseguir suportar
#Além disso, nenhum campo é nulo, o que é um bom indicativo

train[,Confirmado := as.factor(Confirmado)]
levels(train$Confirmado) <- c("não","sim")

dataset <- rbind(train,test,fill=T) #variavel dataset recebe dados de treino, apenas para não perdermos os dados carregados inicialmente

dataset[,NivelRisco := factor(NivelRisco,levels = c(1,2,3,4,5),ordered = T)]
dataset[,servidorCFTV := as.factor(servidorCFTV)]
dataset[,tipoPessoa := as.factor(tipoPessoa)]
dataset[,Cidade := as.factor(Cidade)]

#vamos garantir que na conversão de CodigoEvento para fator, os niveis dos dados de treino e de teste tenham o mesmo
#tamanho

CodigosEnvento <- unique((dataset$CodigoEvento))
dataset[,CodigoEvento := factor(CodigoEvento, levels = CodigosEnvento)]

#Já DataHora trata-se de um campo Date
#Transformando em formato de data
dataset[,DataHora := as.POSIXct(DataHora)]

dataset[,Estado := NULL] #como tempos apenas 1 estado, não faz sentido manter a informação para o modelo

#Para o caso da variável bairro, vamos desconsiderá-la, pois a variavel de área de risco já faz o papel dessa informação
dataset[,Bairro := NULL]

describe(dataset)

#vamos verificar qual o volume dos dados do tipo Confirmado e Não Confirmado
dataset[!is.na(Confirmado),.N,by=Confirmado][,.(Confirmado,N,Percent = 100*N/sum(N))]
#Portanto, apenas 10% dos casos são confirmado, dessa forma os dados estão desbalanceados

source("ToolsSegware.r") #carregando o arquivo de ferramentas com algumas funções auxiliares

#defindo quem são as variáveis do tipo fator para fazer uma análise exploratória em gráfico de barra
variaveis_fator <- c("NivelRisco","servidorCFTV","tipoPessoa","Cidade",
                     "CodigoEvento")

target <- "Confirmado"

#Agora, vamos plotar graficos de barra para cada variável do conjunto de dados e verificar como elas se comportam
#para cada tipo de classificação

lapply(variaveis_fator, function (x) barplotAll(dataset[!is.na(Confirmado)],target,x))
#Pelo análise dos gráficos de barra, os níveis 4 e 5 são aqueles que mais causam casos, tanto não confirmados quanto confirmados
#Pelo gráfico, que possui o servidor CFTV possui um equilíbrio maior entre alarmes falsos e alarmes verdadeiros, 
# diferente de quem nao tem

#Vamos verificar o comportamento da variavel Confirmado a partir de CodCliente
ggplot(dataset[!is.na(Confirmado)], aes(CodCliente)) +
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
dataset[,DiaSemana := factor(weekdays(DataHora), levels = niveis_dia_semana)]

#Também separaremos as horas dos eventos
dataset[,hora := as.factor(hour(DataHora))]
dataset[,PeriodoDia := factor(ifelse(hour(DataHora)<6,"Madrugada",ifelse(hour(DataHora)>17, "Noite",
                                  ifelse(hour(DataHora)>11, "Tarde","Manha"))), levels = niveis_PeriodoDia)]

#Vamos gerar uma coluna de DiaSemana + hora, pois o dia da semana junto a hora pode gerar boas informações
dataset[,DiaSemanaHora := as.numeric(DiaSemana)*100 + as.numeric(hora)-1]

#Vamos gerar uma coluna de DiaSemana + hora + NivelCriticidade
dataset[,DiaSemanaHoraNivelRisco := as.numeric(DiaSemana)*1000 + 
                                            as.numeric(hora)*10-10 + as.numeric(NivelRisco)+1]

variaveis_fator <- append(variaveis_fator, c("DiaSemana","hora", "DiaSemanaHora","DiaSemanaHoraNivelRisco","PeriodoDia"))

#explorando as novas variáveis
lapply(variaveis_fator, function (x) barplotAll(dataset[!is.na(Confirmado)],target,x))

dataset[!is.na(Confirmado),.N,by=.(CodCliente,DiaSemanaHora,Confirmado)][order(-N)]

#Por essa análise, percebe-se que o cliente 11687 interfere em 20% dos dados com casos não confirmados, a maioria
#acontecendo às terças-feiras, próximo às 19h. Vamos dar uma observada melhor no que acontece com esse e outros casos
#que o número de acontecimentos em sequencia é alto

View(dataset[CodCliente == 11687,][order(DataHora)])
View(dataset[CodCliente == 11799,][order(DataHora)])

#Nota-se que o intervalo de tempo entre um alarme e outro, muito curto, pode ser uma variável interessante
dataset[,.N,by = .(Confirmado,DiaSemana)] #verificando como estão os dias da semana dos dados de teste e treino
#Nota-se que eles estão em sequencia, portanto não o risco
dataset[order(CodCliente,DataHora),difTempo := 
          as.numeric(ifelse(is.na(DataHora - lag(DataHora)),Inf,DataHora - lag(DataHora))),
        by = CodCliente]

#Como as diferencas entre os tempos sao bem distintas para o caso que ocorre mais de uma vez nos 3 dias, vamos
#criar um fator entre tempos. A funcao quantize.num que está no pacote de ferramentas auxiliares irá gerar
#esses fatores dentro de limites de tempos

#A variavel breaks cria os ranges em segundos que serão considerados para a análise
breaks <- c(min(dataset$difTempo),10,100,1000,10000,max(dataset$difTempo))
dataset[,difTempo := quantize.num(difTempo, breaks = breaks)]

#A quantidade de fatores para as variaveis DiasemanaHora e DiamaSemanaHoraNivelRisco é elevada para rodar nos modelos em R
#Portanto, deveremos colocar em ranges, assim como foi feito para difTempo.


dataset[,DiaSemanaHora := quantize.num(DiaSemanaHora, 
                                           minval = min(DiaSemanaHora),
                                           maxval = max(DiaSemanaHora),
                                           nlevs = 10)]

dataset[,DiaSemanaHoraNivelRisco:=quantize.num(DiaSemanaHoraNivelRisco, 
                                                minval = min(DiaSemanaHoraNivelRisco),
                                                maxval = max(DiaSemanaHoraNivelRisco),
                                                nlevs = 10)]


#verificando os graficos de barras agora com a nova variavel difTempo
variaveis_fator <- append(variaveis_fator,"difTempo")
lapply(variaveis_fator, function (x) barplotAll(dataset[!is.na(Confirmado)],target,x))

# Pelos graficos de barra, a informação nova de DiaSemanaHora e DiaSemanaHoraNivelRisco é a mesma, portanto, vamos 
#desconsiderar uma delas

dataset$DiaSemanaHoraNivelRisco = NULL
variaveis_fator = variaveis_fator[variaveis_fator != "DiaSemanaHoraNivelRisco"]

#Por fim, vamos realizar a combinação de algumas variaveis e contar a sua ocorrência no dataset
dataset[,CodCliente_N := .N, by = CodCliente,
        ][,Nivel_Risco_N := .N, by = NivelRisco,
        ][,servidorCFTV_N := .N, by = servidorCFTV,
        ][,tipoPessoa_N := .N, by = tipoPessoa,
        ][,Cidade_N := .N, by = Cidade,
        ][,DataHora_N := .N, by = DataHora,
        ][,CodigoEvento_N := .N, by = CodigoEvento,
        ][,DiaSemana_N := .N, by = DiaSemana,
        ][,PeriodoDia_N := .N,by = PeriodoDia,
        ][,hora_H := .N, by = hora,
        ][,DiaSemanaHora_N := .N, by = DiaSemanaHora,
        ][,CodCliente_Nivel_Risco_N := .N, by = .(CodCliente, NivelRisco),
        ][,CodCliente_servidorCFTV_N := .N, by = .(CodCliente,servidorCFTV),
        ][,CodCliente_PeriodoDia_N := .N, by = .(CodCliente,PeriodoDia),
        ][,CodCliente_DiaSemana_N := .N, by= .(CodCliente,DiaSemana),
        ][,CodCliente_hora_N := .N, by = .(CodCliente,hora),
        ][,CodCliente_CodigoEvento_N := .N, by = .(CodCliente,CodigoEvento),
        ][,NivelRisco_servidorCFTV_N := .N, by = .(NivelRisco,servidorCFTV),
        ][,NivelRisco_tipoPessoa_N := .N, by = .(NivelRisco,tipoPessoa),
        ][,NivelRisco_DataHora_N := .N, by = .(NivelRisco,DataHora),
        ][,NivelRisco_CodigoEvento_N := .N , by = .(NivelRisco,CodigoEvento),
        ][,NivelRiscoDiaSemana_N := .N, by = .(NivelRisco,DiaSemana),
        ][,NivelRiscoPeriodoDia_N := .N, by = .(NivelRisco,PeriodoDia),
        ][,NivelRiscoDiaSemanaHora_N := .N, by = .(NivelRisco,DiaSemanaHora),
        ][,NivelRisco_Cidade_N := .N, by = .(NivelRisco,Cidade),
        ][,servidorCFTV_tipoPessoa_N := .N, by = .(servidorCFTV,tipoPessoa),
        ][,servidorCFTV_CodigoEvento_N := .N , by = .(servidorCFTV,CodigoEvento),
        ][,tipoPessoa_Cidade_N := .N, by = .(tipoPessoa,Cidade),
        ][,servidorCFTV_Cidade_N := .N, by = .(servidorCFTV,Cidade),
        ][,servidorCFT_hora_N := .N, by = .(servidorCFTV,hora),
        ][,tipoPessoa_hora_N := .N, by =.(tipoPessoa,hora),
        ][,tipoPessoa_CodigoEvento_N := .N,by= .(tipoPessoa,CodigoEvento),
        ][,tipoPessoa_PeriodoDia_N := .N, by = .(tipoPessoa,PeriodoDia),
        ][,CodigoEvento_PeriodoDia_N := .N, by = .(CodigoEvento,PeriodoDia),
        ][,CodigoEvento_hora_N := .N, by=.(CodigoEvento,hora),
        ][,CodigoEvento_DiaSemanaHora_H := .N, by=.(CodigoEvento,DiaSemanaHora),
        ][,tipoPessoa_servidorCFTV_PeriodoDia_N := .N, by = .(tipoPessoa,servidorCFTV,PeriodoDia),
        ][,CodCliente_NivelRisco_PeriodoDia_N := .N, by = .(CodCliente,NivelRisco,PeriodoDia),
        ][,CodCliente_NivelRisco_CodigoEvento_N := .N, by = .(CodCliente,NivelRisco,CodigoEvento)]

#verificando os graficos de barras agora com a nova variavel difTempo
classes <- lapply(dataset[0],class)
variaveis_numericas <- grep(pattern = "integer",classes)
lapply(names(dataset)[variaveis_numericas], function (x) histogramplotAll(dataset[!is.na(Confirmado)],target,x))


#Feitas as manipulações, vamos normalizar os dados das variaveis inteiras
dataset[,(variaveis_numericas) := lapply(.SD, function(x){(x-mean(x))/sd(x)}),
       .SDcols = variaveis_numericas]


###################################### FEATURE SELECTION ##########################################

#Vamos agora selecionar as melhores variáveis para o modelo de classificação a partir de uma xgbTree
featureSelection <- train(Confirmado ~ . - CodCliente,
                          data = dataset[!is.na(Confirmado)], 
                          method = "xgbTree",
                          metric = "ROC",
                          trControl = trainControl(classProbs = T, 
                                                   summaryFunction = twoClassSummary,
                                                   method = "cv",
                                                   number = 10),
                          tuneGrid = expand.grid(nrounds = 300,
                                                 max_depth = 5,
                                                 eta = 0.3,
                                                 gamma = 0,
                                                 colsample_bytree = 1,
                                                 min_child_weight = 1, 
                                                 subsample = 1),
                          nthread = 4)

varImp(featureSelection)
ggplot(data.frame(Variavel = row.names(varImp(featureSelection)$importance),
                  varImp(featureSelection)$importance),aes(x=reorder(Variavel,Overall),y=Overall))+
  geom_bar(stat="identity")+coord_flip()
# Pelos graus de importancia, vamos selecionar as variaveis mais relevantes para o modelo
variaveis_relevantes <- c("tipoPessoa","CodCliente_N^2","DataHora","CodCliente_CodigoEvento_N",
                          "NivelRisco_tipoPessoa_N","NivelRisco","CodCliente_hora_N","tipoPessoa_CodigoEvento_N",
                          "difTempo","CodigoEvento","CodigoEvento_hora_N","servidorCFT_hora_N","hora",
                          "tipoPessoa_PeriodoDia_N","CodCliente_NivelRisco_CodigoEvento_N")


###################################### SEPARAR DADOS DE TREINO E DE TESTE ########################

#Vamos separar o dataset em dados de treino (70%) e validação (30%), para podermos medir o grau de acerto do modelo

#Separando novamente os dados de treino e teste
treino_validacao <- dataset[!is.na(Confirmado)]
teste <- dataset[is.na(Confirmado)]
rm(dataset)

index_treino <- sample(1:nrow(treino_validacao),round(0.7*nrow(treino_validacao)))
dataset_treino <- treino_validacao[index_treino,]
dataset_validacao <- treino_validacao[-index_treino,]


##################################### RODANDO O MODELO ###########################################


#como a diferença entre classes é muito grande, iremos utilizar o método SMOTE dentro do treino
#para reduzir essa diferença
formula <- as.formula(paste("Confirmado ~", paste(variaveis_relevantes,collapse = " + ")))
modelo <- train(formula,
                 data = dataset_treino,
                 method = "xgbTree",
                 metric = "ROC",
                 trControl = trainControl(classProbs = T, 
                                          summaryFunction = twoClassSummary,
                                          method = "cv",
                                          number = 10,
                                          sampling = "smote"),
                 tuneGrid = expand.grid(nrounds = 300,
                                        max_depth = 5,
                                        eta = 0.3,
                                        gamma = 0,
                                        colsample_bytree = 1,
                                        min_child_weight = 1, 
                                        subsample = 1),
                 nthread = 4)

confusionMatrix(predict(modelo,dataset_validacao),dataset_validacao$Confirmado, mode = "everything")
#Temos uma acurácia de 94%, um recall de 95% e um F1-Score de 97%

####################################Classificando os casos de teste##############################

teste[,Predicao:=predict(modelo,teste)]
View(teste)
