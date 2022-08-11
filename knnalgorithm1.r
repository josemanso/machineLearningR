library(dplyr)  # manipulación y operaciones con data frames.
library(class) # palgoritmo knn
library(caret)# algoritmos ML
library(ISLR) # datase ?Smarket
library(txtplot)


# Loading data
data(Smarket)

Smarket %>%
    head(10)

# Hacemos unas traducciones para facilitad del análisis
#Smarket <- Smarket %>% 
  #rename(Direccion = Direction) %>% 
  #mutate(Direccion = ifelse(Direccion == "Up", "Sube", "Baja")) %>% 
  #mutate_at(c("Direccion"), ~as.factor(.))

set.seed(300)
#Spliting data as training and test set. Using createDataPartition() function from caret
indxTrain <- createDataPartition(y = Smarket$Direction,p = 0.75,list = FALSE)
training <- Smarket[indxTrain,]
testing <- Smarket[-indxTrain,]

#Checking distibution in origanl data and partitioned data
prop.table(table(training$Direction)) * 100

# Setting parameters. Training and train control
SP_ctrl <- trainControl(method="cv", number = 5)

# caret library para hacer el algoritmo knn
SP_kNN <-  train(Direction ~ .,
                 data = training,
                 method = "knn",
                 tuneLength = 20, # k [1:20]
                 trControl = SP_ctrl,
                 preProcess = c("center","scale")
                 )
class(SP_kNN)

print("Classification kNN")
SP_kNN

plot(SP_kNN)

#txtplot(SP_kNN)

# Test the model
SP_knnPrediction <- predict(SP_kNN, newdata = testing)
SP_knnPrediction %>% 
  head(50)

# Show probability
prob_knnPrediction <- predict(SP_kNN, newdata = testing, type = "prob")

prob_knnPrediction %>% 
  head(10)

## Confusion Matrix and Statistics
confusionMatrix(SP_knnPrediction, testing$Direction)

mean(SP_knnPrediction == testing$Direction)
