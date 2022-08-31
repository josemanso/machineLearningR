# linear regression
library(dplyr)   # data manipulation
library(ggplot2) # visualization
library(caTools) # for train/test split
library(corrgram) # making neat correlation matrix plots

# read in the dataset
dataset <-read.csv('/home/josemo/Rprogram/dataset/Salary.csv')
# check data
head(dataset)
dim(dataset) # 35  2

# check missing values
any(is.na(dataset))

# Visualization
ggplot(data=dataset, aes(x=Salary, y=YearsExperience)) +
  geom_point(aes(color=YearsExperience, size=36, alpha=0.1))

# check for correlation
corrgram(df, lower.panel=panel.shade, upper.panel=panel.cor)

# Model training and evaluation
set.seed(123)
# split 80%
split <- sample.split(dataset$Salary,SplitRatio = 0.8)
train_set <-subset(dataset, split==TRUE)
test_set <- subset(dataset, split==FALSE)
cat(' Dimension of traing data:',
    dim(train_set), "\n",'Dimension of testing data:',
    dim(test_set))

# Linear regression R
model <- lm(formula=Salary ~ YearsExperience, data<-train_set)
summary(model)

# histogram
modelResiduals <- as.data.frame(residuals(model))

ggplot(modelResiduals, aes(residuals(model))) +
  geom_histogram(fill='deepskyblue',color='black', bins =30)

# predictions
preds <- predict(model, test_set)

# Visualize the training results
ggplot() +
  geom_point(aes(x <- train_set$YearsExperience, y <- train_set$Salary), colour = 'red') +
  geom_line(aes(x <- train_set$YearsExperience, y <- predict(model, train_set)), colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

# Evaluate
# Visualize the test set results
ggplot() +
  geom_point(aes(test_set$YearsExperience,test_set$Salary),color='red')+
  geom_line(aes(train_set$YearsExperience,
                 predict(model,train_set)),color='blue')+
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')


            
# Evaluate matrix to check the performance of the model
original <- test_set$Salary
predicted<- preds
d <-original - predicted



# MSE and RMSE
MAE <- mean(abs(d))
MSE <- mean((d)^2)
RMSE <- sqrt(MSE)

R2 <- 1 - (sum((d)^2) / sum((original - mean(original))^2))
cat(" Mean Absolute Error:", MAE, "\n", "Mean Square Error:",
    MSE, "\n", 
                                                               "Root Mean Square Error:", RMSE, "\n", "R-squared:", R2)


print("MSE: ")
mse
rmse <- sqrt(mse)
rmse

