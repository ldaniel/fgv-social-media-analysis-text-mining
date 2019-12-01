## FGV-Management - 2S2019
## Analise de Midias Sociais e Mineracao de Texto - Aula 1
## Prof. Eduardo de Rezende Francisco
## Data: 29/Outubro/2019                          

## Exemplo de SNA - Social Network Analysis - Rede TWO MODE

# Extensoes para Analise de Redes
# (devem ser previamente baixadas no CRAN do R)

#install.packages("network")
library(network)
#install.packages("sna")
library(sna)

# Le o arquivo com as informa??es de compras
compras <- read.table("data/raw/Exemplo Rede TwoMode.csv",
                      header = TRUE, sep = ";", dec=",")

###########################
# NAO RODAR
#compras <- as_tibble(compras)

# caso compras seja um objeto tibble, convertelo de volta para dataframe
#compras <- as.data.frame(compras)
##########################

# Adaptando o data.frame compras para que possa servir para a montagem da rede
gcompras <- compras[,2:6]
rownames(gcompras) <- compras[,1]

# Construindo a rede a partir da matriz de relacoes (0 e 1)
teste <- function(gcompras) {
  gplot(gcompras)
  gplot(gcompras, gmode = "twomode", displaylabels = TRUE)
  gplot(gcompras, gmode = "twomode", displaylabels = TRUE,
        edge.col = "gray", label.cex = 0.7, usearrows = FALSE)
}

# Explorando a rede
degree(gcompras, gmode = "twomode", cmode = "indegree")
closeness(gcompras, gmode = "twomode")
betweenness(gcompras, gmode = "twomode")

# Aprimorando a representacao da rede
gplot(gcompras, gmode = "twomode", displaylabels = TRUE,
      edge.col = "gray", label.cex = 0.7, usearrows = FALSE,
      vertex.cex = closeness(gcompras, gmode = "twomode") * 3)

# Analise:
# Voce acha que as medidas de centralidade de proximidade e intermediacao
# sao uteis no contexto da rede Two Mode?
