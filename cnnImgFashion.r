#  Convolution Neural Network, CNN


library(keras)
library(tidyverse)

####
# The Fashion MNIST dataset is then directly imported from Keras 
# 60,000 images to train the model, and 10,000 images to evaluate 
####
fashion_mnist <- dataset_fashion_mnist()
c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test

#  a single label is assigned to each image
class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat', 
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

# dimensions, which are 60,000 images with 28 × 28 pixels each.
dim(train_images)

dim(train_labels) # 60000

dim(test_images) #  10000  28  28
dim(test_labels)

# View a sample image from the dataset.
options(repr.plot.width=7, repr.plot.height=7) 
sample_image <- as.data.frame(train_images[7, , ])
colnames(sample_image) <- seq_len(ncol(sample_image))
sample_image$y <- seq_len(nrow(sample_image))
sample_image <- gather(sample_image, "x", "value", -y)
sample_image$x <- as.integer(sample_image$x)
ggplot(sample_image, aes(x = x, y = y, fill = value)) +
  geom_tile() + scale_fill_gradient(low="white", high="black",na.value=NA) +
  scale_y_reverse()+theme_minimal()+theme(panel.grid=element_blank()) +
  theme(aspect.ratio=1)+xlab("") + ylab("")


# Pre-process data
train_images <- train_images / 255
test_images <- test_images / 255

# display 
options(repr.plot.width=10, repr.plot.height=10)
par(mfcol=c(10,10))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:30) { 
  img <- train_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(class_names[train_labels[i] + 1]))}

# Building the model

model <- keras_model_sequential()
model %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), 
                activation = 'relu', input_shape = c(28, 28, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 10, activation = 'softmax')

model %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = 'adam', 
  metrics = c('accuracy')
)

summary(model)



# # Store the fitting history in `history'

history <- model %>% fit(
  train_images, 
  train_labels, 
  epochs = 20,
  verbose = 2
)


# evaluation the model

score <- model %>% evaluate(train_images, train_labels)
score_frame <-as.data.frame(t(score))
cat('Train loss:', score_frame$loss)
cat('Train accuracy:', score_frame$acc)


# Plot the history
plot(history)

# Evaluate on test data and labels
score <- model %>% evaluate(test_images, test_labels)
score_frame <-as.data.frame(t(score))
cat('Test loss:', score_frame$loss)
cat('Test accuracy:', score_frame$acc)

# Predict Labels of New Data
# Predict the classes for the test data

classes <- model %>% predict(test_images, batch_size = 128) %>%
  k_argmax() %>% as.vector()
predictions <- model %>% predict(test_images) %>%
  k_argmax() %>% as.matrix() 
#predictions % predict(x_test)

classes[1:25]

#predictions <- as.data.frame(t(predictions))
predictions[1,]

#  plot a few images with their predictions
# Correct predictions are in blue,
# whereas wrong predictions are in red.
options(repr.plot.width=7, repr.plot.height=7)
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- test_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  predicted_label <- which.max(predictions[i, ]) - 1
  true_label <- test_labels[i]
  if (predicted_label == true_label) { color <- 'blue' } 
  else 
  { color <- 'red' }
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], 
                      "(",class_names[true_label + 1], ")"),col.main = color)}
