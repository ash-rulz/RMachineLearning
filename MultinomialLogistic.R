####https://www.projectpro.io/article/example-on-how-to-do-logistic-regression-in-r/542####
data <- read.csv('./Data/BreastTissue.csv', header = T, sep = ';')

#Data pre-processing
data <- data[, -1]
data$Class <- as.factor(data$Class)
levels(data$Class)[levels(data$Class) %in% c('fad', 'gla', 'mas')] <- 'other'

#Data splitting
n <- dim(data)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.7))
train <- data[id,]
test <- data[-id,]

train$Class <- relevel(train$Class, ref = 'adi')
library(nnet)
mult_mod <-  multinom(Class ~ ., data = data, MaxNWts =10000000)
round(fitted(mult_mod),2)#Predictions

missclassError <- function(conf_mat){
  return(1-sum(diag(conf_mat))/sum(conf_mat))
}
#Predict training error
train_pred <- predict(mult_mod, type = 'class')
train_conf <-  table(train$Class, train_pred)
train_error <- missclassError(train_conf)

#Predict test error
test_pred <- predict(mult_mod, type = 'class', 
                     newdata = test)
test_conf <-  table(test$Class, test_pred)
test_error <- missclassError(test_conf)
