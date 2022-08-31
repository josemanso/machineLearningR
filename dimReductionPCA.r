# Dimensionality Reduction
### PCA: Principal Component Analysis

library(ggplot2)
library(cowplot) # required to arrange multiple plots in a grid
theme_set(theme_bw(base_size=12)) # set default ggplot2 theme

library(dplyr)

library(grid)

data(iris)
head(iris, 5)

summary(iris)

cor(iris[,1:4])  # Correlation

plot(iris[,1:4], pch='*',col=c("red", "green", "blue")[unclass(iris[,5])])

pairs(iris[1:4], main = "Datos Iris", pch = 21, 
      bg = c("red", "green3", "blue")[unclass(iris$Species)],
      lower.panel=NULL, labels=c("SL","SW","PL","PW"), 
      font.labels=2, cex.labels=4.5) 

p1 <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point()
p2 <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point()
p3 <- ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Species)) + geom_point()
p4 <- ggplot(iris, aes(x=Sepal.Width, y=Petal.Width, color=Species)) + geom_point()
plot_grid(p1, p2, p3, p4, labels = "AUTO")

boxplot(iris$Sepal.Length ~ iris$Species, col=c("red", "green", "blue"),
        main= "Largo de sepalo por especie",xlab=" ",ylab=" ")

library(scatterplot3d)
scatterplot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width,
              color=c("red", "green", "blue")[unclass(iris[,5])])


#### PCA
iris.pca <- prcomp(iris[-5], scale=T)
iris.pca 

summary(iris.pca)

prop_varianza <- iris.pca$sdev^2 / sum(iris.pca$sdev^2)
prop_varianza_acum <- cumsum(prop_varianza)
prop_varianza_acum

ggplot(data = data.frame(prop_varianza, pc = 1:4),
       aes(x=pc, y= prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x="Componente principal", y = "Prop. de varianza")

ggplot(data = data.frame(prop_varianza_acum, pc = 1:4),
       aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +
  geom_label(aes(label = round(prop_varianza_acum,2))) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")

grupos <- iris$Species

plot(iris.pca$x[,1],iris.pca$x[,2],
     col=grupos,cex.axis=1,cex.lab=1)

library("FactoMineR")
res.pca <- PCA(iris[,-5], graph = F)
