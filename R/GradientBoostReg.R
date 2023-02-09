####Gradient Boost for Regression - from scratch####
# https://www.youtube.com/watch?v=3CC4N4z3GJc - Theory
# https://www.youtube.com/watch?v=d6CzVXoVZCc - Code
#Objective: Predicting mpg using all the other fields
library(tree)#For predicting the residual
library(caret)#For RMSE

df <- mtcars
xvars <- names(df[2:ncol(df)])
xvars <- paste(xvars, collapse = ' + ') #Concatenate all the Xs together
lr <- 0.1 #Define the learning rate

#Round 1  
#Get the first prediction - mean of y
df$pred_1 <- mean(df$mpg)
#Calculate pseudo-residuals
df$resid_1 <- df$mpg - df$pred_1

#Round 2 - Go to line 38 instead
#Predict the residual
fml <- paste0('resid_1 ~ ', xvars)
mod <- tree(formula = fml, data = df)
df$pred_2 <- predict(mod, df)
#Get the aggregated result - Mean + lr*pred_residual
temp_mpg <- df$pred_1 + lr * df$pred_2
#Get the residual again
df$resid_2 <- df$mpg - temp_mpg

#Round 3
#Predict the residual
fml <- paste0('resid_2 ~ ', xvars)
mod <- tree(formula = fml, data = df)
df$pred_3 <- predict(mod, df)
#Get the aggregated result - Mean + lr*pred_residual
temp_mpg <- df$pred_1 + lr * df$pred_2 + lr * df$pred_3
#Get the residual again
df$resid_3 <- df$mpg - temp_mpg

#Instead of Round 2, 3... we have a for loop
nrounds <- 10
df$pred <- df$pred_1
#Track the performance by RMSE
rmse_df <- data.frame(Round = 1, RMSE = RMSE(df$mpg, df$pred))
for (i in 2:nrounds) {
  #Predict the residual
  fml <- paste0('resid_', i-1, ' ~ ', xvars)
  mod <- tree(formula = fml, data = df)
  df[[paste0('pred_', i)]] <- predict(mod, df)
  #Get the aggregated result - Mean + lr*pred_residual
  df$pred <- df$pred + lr * df[[paste0('pred_', i)]]
  rmse_df <- rbind(rmse_df, list(
    Round = i,
    RMSE = RMSE(df$mpg, df$pred)
  ))
  #Get the residual again
  df[[paste0('resid_', i)]] <- df$mpg - df$pred
}
#As seen in the below df, with each round the RMSE is going down
rmse_df


####Gradient Boost for Regression - using gbm####
# https://datascienceplus.com/gradient-boosting-in-r/
#Objective: Predicting medv using all the other fields

library(gbm)#Recquired for gradient boosting
library(MASS)
library(caret)#For RMSE function
library(ggplot2)

#Common functions
split_train_test <- function(data, ratio){
  n <- dim(data)[1]
  set.seed(12345)
  id <- sample(1:n, floor(n * ratio))
  train <- data[id,]
  test <- data[-id,]
  return(list(train = train, test = test))
}
get_RMSE_df <- function(model, test, train){
  #Calculate the test error
  trees <- seq(from=100 ,to=10000, by=100) #no of trees-a vector of 100 values 
  pred_mat <- predict(model, newdata = test, n.trees = trees)
  rmse_test <- as.vector(apply(pred_mat, 
                               MARGIN = 2, 
                               function(pred) RMSE(pred, test$medv)))
  rmse_df <- data.frame(
    Type = rep('Test', length(rmse_test)),
    Trees = trees, 
    Error = rmse_test
  )
  
  #Calculate the train error
  pred_mat <- predict(model, newdata = train, n.trees = trees)
  rmse_train <- as.vector(apply(pred_mat, 
                                MARGIN = 2, 
                                function(pred) RMSE(pred, train$medv)))
  rmse_df <- rbind(rmse_df, data.frame(
    Type = rep('Train', length(rmse_train)),
    Trees = trees, 
    Error = rmse_train
  ))
  return(rmse_df)
}


#Splitting the data
input_data <- MASS::Boston
split_data <- split_train_test(input_data, 0.7)
train <- split_data$train
test <- split_data$test

model <- gbm(medv ~ . ,
             data = train,
             distribution = "gaussian",
             n.trees = 10000,#Generates 10000 trees
             shrinkage = 0.01,#Learning rate in the gradient descent
             interaction.depth = 4)#Tree of depth 4

summary(model)#Gives the feature importance plot.
#Gradient boost also can be used for feature selection. 

#Calculate errors
rmse_df <- get_RMSE_df(model, train = train, test = test)

ggplot(data = rmse_df, aes(x = Trees, y = Error)) +
  geom_line(aes(color = Type, linetype = Type))
