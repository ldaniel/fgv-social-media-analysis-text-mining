## FGV-Management - 2S2019
## Analise de Midias Sociais e Mineracao de Texto - Aula 1
## Prof. Eduardo de Rezende Francisco
## Data: 29/Outubro/2019   

## Exemplo de Cluster Analysis

# Extensao para graficos e para Analise de Agrupamentos

#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggdendro")
library(ggdendro)

# Le o arquivo com as informacoes dos Sanduiches
sanduiches <- read.table("data/raw/Sanduiches.csv",
                         header = TRUE, sep = ";",
                         dec = ",")

row.names(sanduiches) <- sanduiches$Sanduiches

# Implementa o algoritmo hierarquico e apresenta o dendrograma
hc <- hclust(dist(sanduiches), "average")  # explorar com outros m?todos de dist?ncia
p <- ggdendrogram(hc, rotate=FALSE)
print(p)
ggdendrogram(hc, rotate=TRUE)

hcdata <- dendro_data(hc)
ggdendrogram(hcdata, rotate=TRUE, size=2) + labs(title="Dendrograma dos Sanduiches")

# "Cortando" a ?rvore em 3 grupos
grupos <- cutree(hc,k=3)
grupos

# Analisando as principais vari?veis a partir dos grupos
boxplot(sanduiches$Sódio ~ grupos, col = "blue", main = 'Box Plot do Teor de Sodio')
boxplot(sanduiches$Valor.Energético ~ grupos, col = "blue", main = 'Box Plot do Valor Energetico')


## =================================
## Refazendo os clusters, agora com as vari?veis padronizadas
## =================================

sand_padr <- sanduiches
for (i in 2:12) sand_padr[,i] <- scale(sand_padr[,i])

# Implementa o algoritmo hier?rquico e apresenta o dendrograma
hc2 <- hclust(dist(sand_padr), "average")  # explorar com outros m?todos de dist?ncia
p <- ggdendrogram(hc2, rotate = FALSE)
print(p)
ggdendrogram(hc2, rotate = TRUE)

hcdata2 <- dendro_data(hc2)

ggdendrogram(hcdata2, rotate = TRUE, size = 2) + labs(title = "Dendrograma dos Sanduiches (padronizado)")

# "Cortando" a ?rvore em 3 grupos
grupos2 <- cutree(hc2, k = 3)
grupos2

# Analisando as principais vari?veis a partir dos grupos
boxplot(sand_padr$Sódio ~ grupos2, col = "green", main = 'Box Plot do Teor de Sodio')
boxplot(sand_padr$Valor.Energético ~ grupos2, col = "green", main = 'Box Plot do Valor Energetico')
