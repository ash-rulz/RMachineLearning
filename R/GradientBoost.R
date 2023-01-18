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
