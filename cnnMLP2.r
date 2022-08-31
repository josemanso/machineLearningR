#  Convolution Neural Network, CNN
#  Multi-Layer Perceptron (MLP) for multi-class classification.

# Load in the keras package
library(keras)
#library(tensorflow)
library(tidyverse)
#library(datasets)

# # Read in MNIST data
# mnist <- dataset_mnist()
# 
# # Read in CIFAR10 data
# cifar10 <- dataset_cifar10()
# 
# # Read in IMDB data
# imdb <- dataset_imd

# Read in 'iris' data
data(iris)
summary(iris)

#  use the as.numeric() function to convert the data to numbers:
iris[,5] <- as.numeric(iris[,5]) -1

# Turn `iris` into a matrix
iris <- as.matrix(iris)

# Set iris `dimnames` to `NULL`
dimnames(iris) <- NULL


#  Training And Test Sets

set.seed(1234)

# Determine sample size
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

# Split the `iris` data
iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]

# Split the class attribute
iris.trainingtarget <- iris[ind==1, 5]
iris.testtarget <- iris[ind==2, 5]

# One hot encode training target values
iris.trainLabels <- to_categorical(iris.trainingtarget)

# One hot encode test target values
iris.testLabels <- to_categorical(iris.testtarget)

# Print out the iris.testLabels to double check the result
print(iris.testLabels)


#  Constructing the Model

# Initialize a sequential model
model <- keras_model_sequential() 

# Add layers to the model
model %>% 
  layer_dense(units = 8, activation = 'relu', input_shape = c(4)) %>% 
  layer_dense(units = 3, activation = 'softmax')


# Print a summary of a model
summary(model)

# Get model configuration
get_config(model)


# Get layer configuration
get_layer(model, index = 1)

# List the model's layers
model$layers

# List the input tensors
model$inputs


# List the output tensors
model$outputs

# Compile And Fit The Model
# Compile the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

# Store the fitting history in `history` 
history <- model %>% fit(
  iris.training, 
  iris.trainLabels, 
  epochs = 200,
  batch_size = 5, 
  validation_split = 0.2
)

# Plot the history
plot(history)


# Visualize The Model Training History
# Plot the model loss of the training data
plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")

# Plot the model loss of the test data
lines(history$metrics$val_loss, col="green")

# Add legend
legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))


# Plot the accuracy of the training data 
plot(history$metrics$acc, main="Model Accuracy", xlab = "epoch", ylab="accuracy", col="blue", type="l")

# Plot the accuracy of the validation data
lines(history$metrics$val_acc, col="green")

# Add Legend
legend("bottomright", c("train","test"), col=c("blue", "green"), lty=c(1,1))


# Predict Labels of New Data
# Predict the classes for the test data
#classes <- model %>% predict_classes(iris.test, batch_size = 128)
#classes <- model %>% predict(x) %>% k_argmax()
classes <- model %>% predict(iris.test, batch_size = 128) %>%
  k_argmax() %>% as.vector()

# Confusion matrix
table(iris.testtarget, classes)

# evaluation the model
# Evaluate on test data and labels
score <- model %>% evaluate(iris.test, iris.testLabels, batch_size = 128)

# Print the score
print(score)

# Fine-tuning the Model
# Adding layers
# Initialize the sequential model
model2 <- keras_model_sequential() 

# Add layers to model
model2 %>% 
  layer_dense(units = 8, activation = 'relu', input_shape = c(4)) %>% 
  layer_dense(units = 5, activation = 'relu') %>% 
  layer_dense(units = 3, activation = 'softmax')

# Compile the model
model2 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)


# Fit the model to the data
model2 %>% fit(
  iris.training, iris.trainLabels, 
  epochs = 200, batch_size = 5, 
  validation_split = 0.2
)

# Evaluate the model
score2 <- model2 %>% evaluate(iris.test, iris.testLabels, batch_size = 128)

# Print the score
print(score2)
