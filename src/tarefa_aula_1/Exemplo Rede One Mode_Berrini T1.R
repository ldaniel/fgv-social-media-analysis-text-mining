## FGV-Management - 2S2019
## Analise de Midias Sociais e Mineracao de Texto - Aula 1
## Prof. Eduardo de Rezende Francisco
## Data: 29/Outubro/2019                          

## Exemplo de SNA - Social Network Analysis - Rede ONE MODE

# Extensoes para Analise de Redes
#(devem ser previamente baixadas no CRAN do R)

#install.packages("network")
library(network)

#install.packages("sna")
library(sna)

#install.packages("rgl")
library(rgl)

library(visNetwork)

# Trabalha a partir de uma rede aleat?ria
rede <- read.table("data/7-analise-de-midias-sociais-e-mineracao-de-texto/Exemplo Rede.csv",
                   header = TRUE,
                   sep = ";", dec = ",")

# Adaptando o data.frame rede para que possa servir para a montagem da rede
grede <- rede[,2:10]
rownames(grede) <- rede[,1]

# Construindo a rede a partir da matriz de relacoes (0 e 1)
gplot(grede)

gplot(grede, gmode = "graph", displaylabels = TRUE)

gplot(grede, gmode = "graph", displaylabels = TRUE, 
      edge.col = "gray", usearrows = FALSE)

# Explorando a rede
degree(grede, gmode = "graph", cmode = "indegree")
closeness(grede, gmode = "graph")
betweenness(grede, gmode = "graph")

# Aprimorando a representa??o da rede
gplot(grede,gmode = "grede", displaylabels = TRUE,
      edge.col = "gray", usearrows = FALSE, 
      vertex.cex = degree(grede, gmode = "graph", cmode = "indegree") / 3)

gplot(grede,gmode = "grede", displaylabels = TRUE,
      edge.col = "gray", usearrows = FALSE, 
      label = degree(grede, gmode = "graph", cmode = "indegree"))

gplot(grede, gmode = "grede", displaylabels = TRUE,
      edge.col = "gray", usearrows = FALSE, 
      vertex.cex = closeness(grede, gmode = "graph") * 2)

gplot(grede, gmode = "grede", displaylabels = TRUE,
      edge.col = "gray", usearrows = FALSE, 
      label = round(closeness(grede, gmode = "graph"), digits = 2))

gplot(grede,gmode = "grede",displaylabels = TRUE,
      edge.col = "gray", usearrows = FALSE , 
      vertex.cex=betweenness(grede, gmode = "graph") / 3 + 1)

gplot(grede,gmode = "grede", displaylabels = TRUE,
      edge.col = "gray", usearrows = FALSE, 
      label = betweenness(grede, gmode = "graph"))

# Grand finale... para impressionar
gplot3d(grede)

# Comando util para explorar redes
# Gera redes aleatorias
grede2 <- rgraph(10)
grede2


row.names(rede) <- rede$Label

rede <- dplyr::select(rede, -Label) %>% as.matrix

library(igraph)

g <- graph_from_adjacency_matrix(rede)

visIgraph(g)


# Interprete as metricas de centralidade de grau, proximidade e intermediacao
# para a rede da variavel grede
