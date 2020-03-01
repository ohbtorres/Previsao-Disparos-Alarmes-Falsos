# Função para criar gráficos de barras com o auxilio da função lapply
barplotAll <- function (dataset,target,variavel){
  ggplot(dataset, aes_string(variavel)) +
    geom_bar() + ggtitle(paste(target,"vs",variavel))+
    facet_wrap(as.formula(paste(". ~",target)))
}

histogramplotAll <- function (dataset,target,variavel){
  ggplot(dataset, aes_string(variavel)) +
    geom_histogram() + ggtitle(paste(target,"vs",variavel))+
    facet_wrap(as.formula(paste(". ~",target)))
}

#Função para dividir em ranges do tipo fator variaveis numéricas
quantize.num <- function(x, nlevs = 5, maxval = 1000, 
                         minval = 0, ordered = TRUE, breaks = NA){
  if(class(breaks) == "logical"){
    cuts <- seq(min(x), max(x), length.out = nlevs + 1)
    cuts[1] <- minval
    cuts[nlevs + 1] <- maxval
  }else{
    cuts <- breaks
  }
  x <- cut(x, breaks = cuts, order_result = ordered,include.lowest = T)
  return(x)
}