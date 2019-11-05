## FGV-Management - 2S2019
## Análise de Mídias Sociais e Mineração de Texto - Aula 1
## Prof. Eduardo de Rezende Francisco
## Data: 29/Outubro/2019                          

## Exemplo de SNA - Social Network Analysis - Rede TWO MODE

# Extensoes para Analise de Redes
#(devem ser previamente baixadas no CRAN do R)

setwd("Y:/Material Alunos/SNA_BrasiliaT1/Aula 1")

install.packages("network")
library(network)
install.packages("sna")
library(sna)

# Le o arquivo com as informa??es de compras
compras <- read.table("c:/temp/Exemplo Rede TwoMode.csv",header=TRUE,sep = ";", dec=",")

###########################
# NÃO RODAR
#compras <- as_tibble(compras)

# caso compras seja um objeto tibble, convertê-lo de volta para dataframe
#compras <- as.data.frame(compras)
##########################

# Adaptando o data.frame compras para que possa servir para a montagem da rede
gcompras <- compras[,2:6]
rownames(gcompras) <- compras[,1]

# Construindo a rede a partir da matriz de rela??es (0 e 1)
teste <- function(gcompras) {
  gplot(gcompras)
  gplot(gcompras,gmode="twomode",displaylabels = TRUE)
  gplot(gcompras,gmode="twomode",displaylabels = TRUE,
        edge.col="gray",label.cex = 0.7,usearrows=FALSE)
}

# Explorando a rede
degree(gcompras,gmode="twomode",cmode="indegree")
closeness(gcompras,gmode="twomode")
betweenness(gcompras,gmode="twomode")

# Aprimorando a representa??o da rede
gplot(gcompras,gmode="twomode",displaylabels = TRUE,
      edge.col="gray",label.cex = 0.7,usearrows=FALSE,
      vertex.cex = closeness(gcompras,gmode="twomode")*3)

# Analise:
# Voc? acha que as medidas de centralidade de proximidade e intermedia??o
# s?o ?teis no contexto da rede Two Mode? 


