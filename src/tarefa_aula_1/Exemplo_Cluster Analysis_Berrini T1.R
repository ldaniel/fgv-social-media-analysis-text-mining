## FGV-Management - 2S2019
## Análise de Mídias Sociais e Mineração de Texto - Aula 1
## Prof. Eduardo de Rezende Francisco
## Data: 29/Outubro/2019   

## Exemplo de Cluster Analysis

# Extensão para gráficos e para Análise de Agrupamentos

install.packages("ggplot2")
library(ggplot2)
install.packages("ggdendro")
library(ggdendro)

# Lê o arquivo com as informações dos Sanduíches
sanduiches <- read.table("c:/temp/Sanduiches.csv",header=TRUE,sep = ";", dec=",")
row.names(sanduiches) <- sanduiches$Sanduíches

# Implementa o algoritmo hierárquico e apresenta o dendrograma
hc <- hclust(dist(sanduiches), "average")  # explorar com outros métodos de distância
p <- ggdendrogram(hc, rotate=FALSE)
print(p)
ggdendrogram(hc, rotate=TRUE)

hcdata <- dendro_data(hc)
ggdendrogram(hcdata, rotate=TRUE, size=2) + labs(title="Dendrograma dos Sanduíches")

# "Cortando" a árvore em 3 grupos
grupos <- cutree(hc,k=3)
grupos

# Analisando as principais variáveis a partir dos grupos
boxplot(sanduiches$Sódio ~ grupos, col = "blue", main = 'Box Plot do Teor de Sódio')
boxplot(sanduiches$Valor.Energético ~ grupos, col = "blue", main = 'Box Plot do Valor Energético')


## =================================
## Refazendo os clusters, agora com as variáveis padronizadas
## =================================

sand_padr <- sanduiches
for (i in 2:12) sand_padr[,i] <- scale(sand_padr[,i])

# Implementa o algoritmo hierárquico e apresenta o dendrograma
hc2 <- hclust(dist(sand_padr), "average")  # explorar com outros métodos de distância
p <- ggdendrogram(hc2, rotate=FALSE)
print(p)
ggdendrogram(hc2, rotate=TRUE)

hcdata2 <- dendro_data(hc2)
ggdendrogram(hcdata2, rotate=TRUE, size=2) + labs(title="Dendrograma dos Sanduíches (padronizado)")

# "Cortando" a árvore em 3 grupos
grupos2 <- cutree(hc2,k=3)
grupos2

# Analisando as principais variáveis a partir dos grupos
boxplot(sand_padr$Sódio ~ grupos2, col = "green", main = 'Box Plot do Teor de Sódio')
boxplot(sand_padr$Valor.Energético ~ grupos2, col = "green", main = 'Box Plot do Valor Energético')
