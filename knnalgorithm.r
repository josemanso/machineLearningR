
library(dplyr)  # manipulación y operaciones con data frames.
library(class) # palgoritmo knn
library(caTools)  # varias funciones básicas, split


# Loading data
data(iris)
iris %>%
  head(10)

# Splitting data into train an test data
split <- sample.split(iris, SplitRatio = 0.7)
train_cl <- subset(iris, split == "TRUE")
test_cl <- subset(iris, split =="FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])

# Fifting kNN Model to train dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species, 
                      k = 1)

print("Classification kNN")
classifier_knn
#print()

# Confusion Matrix
cm <- table(test_cl$Species, classifier_knn)
print("Confusion matrix")
cm
#print()

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Species)
print(paste('Accuracy =', 1-misClassError))


k <- 1:50
result <- data.frame(k, accuary = 0)

for(n in k) {
  # Fifting kNN Model to train dataset
  classifier_knn <- knn(train = train_scale,
                        test = test_scale,
                        cl = train_cl$Species, 
                        k = n)
  result$accuary[n] <- mean(classifier_knn == test_cl$Species)
}
k
result %>%
  #ggplot() +
  ggplot() +
  aes(k, accuary) +
  geom_line()


