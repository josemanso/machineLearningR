# k-means and Hierarchical Clustering. 

# read data from https://cs.joensuu.fi/sipu/datasets/
#setwd('/home/josemo/Rprogram/dataset/')
#datase <- read.csv('Aggregation.csv')
dataset <- read.csv('/home/josemo/Rprogram/dataset/Aggregation.csv')
#dataset <- read.csv(setwd+file = 'Aggregation.csv')
str(dataset)
# 
#''data.frame':	788 obs. of  2 variables:
#  $ X: num  15.6 14.9 14.4 14.2 13.8 ...
#  $ Y: num  28.6 27.6 28.4 28.8 28.1 ...
summary(dataset)
##              X               Y         
##      Min.   : 3.35   Min.   : 1.950  
##      1st Qu.:11.15   1st Qu.: 7.037  
##      Median :18.23   Median :11.725  
##      Mean   :19.57   Mean   :14.172  
##      3rd Qu.:30.70   3rd Qu.:21.962  
##      Max.   :36.55   Max.   :29.150  

library(ggplot2)
ggplot() +
  geom_point(aes(x=X, y=Y), data=dataset,alpha=0.5) +
  ggtitle('Data Set')


# K-means
set.seed(1234)
wcss <- vector()  ## WCSS (Within Clusters Summed Squares).
for (i in 1:20) {
  wcss[i] <- sum(kmeans(dataset, i)$withinss)
}
# results
ggplot() +
  geom_point(aes(x=1:20, y = wcss), color ='blue') +
  geom_line(aes(x=1:20, y=wcss), color='blue') +
  ggtitle('Método del Codo') +
  xlab('Cantidad de centroides k') +
  ylab('WCSS')

# model behavior; k = 7,8,9
set.seed(1234)
kmeans <- kmeans(dataset, 7, iter.max = 1000, nstart= 10)

# Cluster results
dataset$cluster <- kmeans$cluster
ggplot() +
  geom_point(aes(x=X,y=Y, color=cluster),data=dataset,size=2) +
  scale_color_gradientn(colours=rainbow(4)) +
  geom_point(aes(x=kmeans$centers[, 1],y=kmeans$centers[,2]),color='blue',size=3) +
  ggtitle('Clusters de Datso con k = 8 / K -Means') +
  xlab('X') + ylab('Y')

set.seed(1234)
kmeans <- kmeans(dataset, 9, iter.max = 1000, nstart = 10)
dataset$cluster <- kmeans$cluster
ggplot() + geom_point(aes(x = X, y = Y, color = cluster), data = dataset, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  geom_point(aes(x = kmeans$centers[, 1], y = kmeans$centers[, 2]), color = 'black', size = 3) + 
  ggtitle('Clusters de Datos con k = 9 / K-Medios') + 
  xlab('X') + ylab('Y')

# Agrupamiento Jerárquico (Hierarchical Clustering)
library(ggdendro)
dendrogram <- hclust(dist(dataset, method = 'euclidean'), method = 'ward.D')
ggdendrogram(dendrogram,rotate=FALSE,labels=FALSE,theme_dendro=TRUE)+
  labs(title="Dendrogram")


# k = 3, 4, 6 , 7
agrupamientoJ <- hclust(dist(dataset, method = 'euclidean'), method = 'ward.D')
clases_aj <- cutree(agrupamientoJ, k = 3)
dataset$cluster <- clases_aj

ggplot() + geom_point(aes(x = X, y = Y, color = cluster), data = dataset, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  ggtitle('Clusters de Datos con k = 3 / Agrupamiento Jerárquico') + 
  xlab('X') + ylab('Y')
# k = 4
clases_aj <- cutree(agrupamientoJ, k = 4)
dataset$cluster <- clases_aj
ggplot() + geom_point(aes(x = X, y = Y, color = cluster), data = dataset, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  ggtitle('Clusters de Datos con k = 4 / Agrupamiento Jerárquico') + 
  xlab('X') + ylab('Y')
# k = 6
clases_aj <- cutree(agrupamientoJ, k = 6)
dataset$cluster <- clases_aj
ggplot() + geom_point(aes(x = X, y = Y, color = cluster), data = dataset, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  ggtitle('Clusters de Datos con k = 6 / Agrupamiento Jerárquico') + 
  xlab('X') + ylab('Y')
# k = 7
clases_aj <- cutree(agrupamientoJ, k = 7)
dataset$cluster <- clases_aj
ggplot() + geom_point(aes(x = X, y = Y, color = cluster), data = dataset, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  ggtitle('Clusters de Datos con k = 7 / Agrupamiento Jerárquico') + 
  xlab('X') + ylab('Y')
