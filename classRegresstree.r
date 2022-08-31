# CART: Classification And Regression Trees.

library(tidyverse)   # data processing
library(rpart) # Recursive Partitioning and Regression Trees RPART
library(rpart.plot) # plot rpart
library(caret) # classification and regression utilities

# Datos; data
#download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", "wine.data")

# Información; imformation
#download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.names", "wine.names")

readLines("/home/josemo/Rprogram/dataset/wine.data", 10)

vino <- read.table("/home/josemo/Rprogram/dataset/wine.data",
                   sep=",",header =FALSE)
vino

readLines("/home/josemo/Rprogram/dataset/wine.names", 10)

file.copy(from = "/home/josemo/Rprogram/dataset/wine.names",
          to = "/home/josemo/Rprogram/dataset/wine_names.txt")
file.show("/home/josemo/Rprogram/dataset/wine_names.txt")


summary(vino)
#  V1  min value 1; max value 3  -- 
nombres <- 
  readLines("/home/josemo/Rprogram/dataset/wine_names.txt")[58:70] %>%
  gsub("[[:cntrl:]].*\\)", "", .) %>% 
  trimws() %>%
  gsub(" |/", "_", .) %>% 
  # Agregamos el nombre "tipo", para nuestra primera columna con los tipos de vino
  c("tipo", .)

names(vino) <- nombres
vino <- vino %>%
  mutate_at("tipo", factor)

# Sets test and training
set.seed(1649)
vino_entrenamiento <- sample_frac(vino, .75)
vino_prueba <- setdiff(vino,vino_entrenamiento)

# training our model
#arbol_1 <- rpart(formula=tipo ~ ., data=vino_entrenamiento)
arbol_1 <- rpart(formula = tipo ~ ., data = vino_entrenamiento)
arbol_1

# plot
rpart.plot(arbol_1)

prediccion_1 <- predict(arbol_1, newdata = vino_prueba, type = "class")
#prediccion_1 <- predict(arbol_1, newdata=vino_prueba, tipe="class")
#confusionMatrix(prediccion_1,vino_prueba[["tipo"]])
confusionMatrix(prediccion_1, vino_prueba[["tipo"]])

# new tree
set.seed(7439)
vino_entrenamiento_2 <- sample_frac(vino, .75)
vino_prueba_2 <- setdiff(vino, vino_entrenamiento_2)

arbol_2 <- rpart(formula = tipo ~ ., data = vino_entrenamiento_2)

prediccion_2 <- predict(arbol_2, newdata = vino_prueba_2, type = "class")

rpart.plot(arbol_2)

confusionMatrix(prediccion_2, vino_prueba_2[["tipo"]])
