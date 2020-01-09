## FGV-Management - 2S2019
## An?lise de M?dias Sociais e Minera??o de Texto - Aula 1
## Prof. Eduardo de Rezende Francisco
## Data: 29/Outubro/2019   

## Exemplo de Cluster Analysis

# Extens?o para gr?ficos e para An?lise de Agrupamentos

#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggdendro")
library(ggdendro)

# L? o arquivo com as informa??es dos Sandu?ches
sanduiches <- read.table("data/raw/Sanduiches.csv",header=TRUE,sep = ";", dec=",")
row.names(sanduiches) <- sanduiches$Sanduiches

# Implementa o algoritmo hier?rquico e apresenta o dendrograma
hc <- hclust(dist(sanduiches), "average")  # explorar com outros m?todos de dist?ncia
p <- ggdendrogram(hc, rotate=FALSE)
print(p)
ggdendrogram(hc, rotate=TRUE)

hcdata <- dendro_data(hc)
ggdendrogram(hcdata, rotate=TRUE, size=2) + labs(title="Dendrograma dos Sandu?ches")

# "Cortando" a ?rvore em 3 grupos
grupos <- cutree(hc,k=3)
grupos

# Analisando as principais vari?veis a partir dos grupos
boxplot(sanduiches$Sódio ~ grupos, col = "blue", main = 'Box Plot do Teor de S?dio')
boxplot(sanduiches$Valor.Energético ~ grupos, col = "blue", main = 'Box Plot do Valor Energ?tico')


## =================================
## Refazendo os clusters, agora com as vari?veis padronizadas
## =================================

sand_padr <- sanduiches
for (i in 2:12) sand_padr[,i] <- scale(sand_padr[,i])

# Implementa o algoritmo hier?rquico e apresenta o dendrograma
hc2 <- hclust(dist(sand_padr), "average")  # explorar com outros m?todos de dist?ncia
p <- ggdendrogram(hc2, rotate=FALSE)
print(p)
ggdendrogram(hc2, rotate=TRUE)

hcdata2 <- dendro_data(hc2)
ggdendrogram(hcdata2, rotate=TRUE, size=2) + labs(title="Dendrograma dos Sandu?ches (padronizado)")

# "Cortando" a ?rvore em 3 grupos
grupos2 <- cutree(hc2,k=3)
grupos2

# Analisando as principais vari?veis a partir dos grupos
boxplot(sand_padr$Sódio ~ grupos2, col = "green", main = 'Box Plot do Teor de S?dio')
boxplot(sand_padr$Valor.Energético ~ grupos2, col = "green", main = 'Box Plot do Valor Energ?tico')

## =============================================================================
## Kmeans, com vari?veis padronizadas, usando o mesmo k do algoritmo hier?rquico
## =============================================================================

res_k <- kmeans(sand_padr[2:12],centers=3)
gruposk <- res_k$cluster

# Analisando as principais vari?veis a partir dos grupos
boxplot(sand_padr$Sódio ~ gruposk, col = "green", main = 'Box Plot do Teor de S?dio')
boxplot(sand_padr$Valor.Energético ~ gruposk, col = "green", main = 'Box Plot do Valor Energ?tico')

## Testando outrs valores de k, atrav?s do WSS
## (baseado em c?digo de http://www.learnbymarketing.com/tutorials/k-means-clustering-in-r-example/)

totaisgrupos <- 2:19

tries <- 100 #Run the K Means algorithm 100 times

avg.totw.ss <-integer(length(totaisgrupos)) #Set up an empty vector to hold all of points

# Para cada n?mero de grupos
for(v in 1:length(totaisgrupos))
{
 v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
 k <- totaisgrupos[v]

 for(i in 1:tries)
 {
   k.temp <-kmeans(sand_padr[2:12],centers=k) #Run kmeans
   v.totw.ss[i] <-k.temp$tot.withinss  #Store the total withinss
 }
 avg.totw.ss[v] <- mean(v.totw.ss) #Average the 100 total withinss

}

## Apresenta gr?fico com os resultados

plot(totaisgrupos,avg.totw.ss,type="b", main="Total Within SS by Various K",
 ylab="Average Total Within Sum of Squares",
 xlab="Value of K")
