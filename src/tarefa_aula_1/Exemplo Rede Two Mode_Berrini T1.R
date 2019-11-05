## FGV-Management - 2S2019
## Análise de Mídias Sociais e Mineração de Texto - Aula 1
## Prof. Eduardo de Rezende Francisco
## Data: 29/Outubro/2019                            

## Exemplo de SNA - Social Network Analysis - Rede TWO MODE

# Extensões para Análise de Redes
#(devem ser previamente baixadas no CRAN do R)

install.packages("network")
library(network)
install.packages("sna")
library(sna)

# Lê o arquivo com as informações de compras
compras <- read.table("c:/temp/Exemplo Rede TwoMode.csv",header=TRUE,sep = ";", dec=",")

# Adaptando o data.frame compras para que possa servir para a montagem da rede
gcompras <- compras[,2:6]
rownames(gcompras) <- compras[,1]

# Construindo a rede a partir da matriz de relações (0 e 1)
gplot(gcompras)
gplot(gcompras,gmode="twomode",displaylabels = TRUE)
gplot(gcompras,gmode="twomode",displaylabels = TRUE,
      edge.col="gray",label.cex = 0.7,usearrows=FALSE)

# Explorando a rede
degree(gcompras,gmode="twomode",cmode="indegree")
closeness(gcompras,gmode="twomode")
betweenness(gcompras,gmode="twomode")

# Aprimorando a representação da rede
gplot(gcompras,gmode="twomode",displaylabels = TRUE,
      edge.col="gray",label.cex = 0.7,usearrows=FALSE,
      vertex.cex = closeness(gcompras,gmode="twomode")*3)

# Analise:
# Você acha que as medidas de centralidade de proximidade e intermediação
# são úteis no contexto da rede Two Mode? 


