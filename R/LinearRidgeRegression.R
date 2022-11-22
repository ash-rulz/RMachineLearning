library(dplyr)
library(ggplot2)
library(tidyr)
set.seed(12345)
#Read csv
data <- read.csv(file = './Data/parkinsons.csv', header = T)
# View(data)
# colnames(data)
# [1] "subject."      "age"           "sex"           "test_time"     "motor_UPDRS"  
# [6] "total_UPDRS"   "Jitter..."     "Jitter.Abs."   "Jitter.RAP"    "Jitter.PPQ5"  
# [11] "Jitter.DDP"    "Shimmer"       "Shimmer.dB."   "Shimmer.APQ3"  "Shimmer.APQ5" 
# [16] "Shimmer.APQ11" "Shimmer.DDA"   "NHR"           "HNR"           "RPDE"         
# [21] "DFA"           "PPE"   

data <- data %>% 
  dplyr::select(motor_UPDRS,starts_with("Jitter"),starts_with("Shimmer"),
                NHR, HNR, RPDE, DFA, PPE)
# colnames(data)

#Divide training and test data in 60-40 ratio
n <- nrow(data)
train_index <- sample(1:n, size=floor(n*0.6))
train <- data[train_index,]
test <- data[-train_index,]
dim(test)[1] #2350
dim(train)[1] #3525

#Scaling the data
params <- caret::preProcess(train)
Stest <- predict(params, test)
Strain <- predict(params, train)

#Plotting motor_UPDRS
ggplot(Stest, aes(x=Stest$Jitter.Abs., y = Stest$motor_UPDRS)) +
  geom_point() +
  scale_x_continuous(
    breaks = seq(from=-1,to=2,by=0.1),
    limits = c(-1,3)
    )

#Computing linear Regression model from training data
# mod1 <- lm(formula = motor_UPDRS~., data = Stest)#model with intercept
mod <- lm(formula = motor_UPDRS~-1+., data = Strain)#model without intercept
# summary(mod1)#With intercept
summary(mod)#Without intercept
# test_predict1 <- predict(mod1)
test_predict <- predict(mod,newdata = Stest)
train_predict <- predict(mod,newdata = Strain)
length(test_predict)#2350
length(mod$fitted.values[1:5])#2350

#MSE - train
mse_train <- mean((train_predict-Strain$motor_UPDRS)^2) #0.9016564
cat('Training Error:', mse_train)

#MSE - test
# mse_test1 <- mean((test_predict1-Stest$motor_UPDRS)^2) #0.8863147 - With intercept
mse_test <- mean((test_predict-Stest$motor_UPDRS)^2) #0.8996531 - Without intercept
cat('Test Error: ',mse_test)
# The MSE is almost the same with and without intercept
summary(mod)$r.squared#0.1115984 - 11% of data explained by the features

#Question 2
# options(scipen=999)
p_df <- data.frame(p_val = summary(mod)$coefficients[,4]) %>%
  filter(p_val < 0.05) %>%
  arrange(p_val)
print(p_df)#These columns are considered to be statistically significant.

#TODO: Find the R2 to find the variance explained by the features. See if it matches.

Loglikelihood <- function(theta, sigma){
  n <- nrow(Strain)
  x <- as.matrix(Strain[,-1])
  return(-n*log(sigma*sqrt(2*pi))-
    (1/(2*sigma^2))*sum((Strain$motor_UPDRS-(x%*%(theta)))^2))
}
Ridge <- function(x, lambda){
  theta <- x[1:16]
  sigma <- x[17]
  # lambda <- x[18]
  return(-Loglikelihood(theta,sigma) + (lambda * sum(theta * theta)))
  # return(-Loglikelihood(theta,sigma))
}

RidgeOpt <- function(lambdas){
  # parameter <- c(rep(1,17),lambda)
  parameter <- c(rep(1,17))
  mse_df <- as.data.frame(matrix(nrow = 0, ncol = 4))
  colnames(mse_df) <- c('lambda', 'data_type', 'mse', 'rsqr')
  coef_df <- as.data.frame(matrix(nrow=0, ncol=3))
  colnames(coef_df) <- c('lambda','coef', 'val')
  for (i in lambdas) {
    res <- optim(parameter, 
                 fn=Ridge,
                 lambda = i, 
                 method = "BFGS") #Get the optimal theta & sigma 
    theta <- res$par[1:16]
    #MSE for training data
    train_x <- as.matrix(Strain[,-1])
    train_y_hat <- train_x %*% theta
    train_mse = mean((Strain$motor_UPDRS-train_y_hat)^2)
    train_rsq <- cor(Strain$motor_UPDRS, train_y_hat)^2
    
    #MSE for training data
    test_x <- as.matrix(Stest[,-1])
    test_y_hat <- test_x %*% theta
    test_mse <- mean((Stest$motor_UPDRS-test_y_hat)^2)
    test_rsq <- cor(Stest$motor_UPDRS, test_y_hat)^2
    
    mse_df <- rbind(mse_df, 
                    data.frame(lambda = i, 
                               data_type = 'train',
                               mse = train_mse,
                               rsqr = train_rsq
                    )
    )
    mse_df <- rbind(mse_df, 
                    data.frame(lambda = i, 
                               data_type = 'test',
                               mse = test_mse,
                               rsqr = test_rsq
                    ))
    coef_df <- rbind(
      coef_df,
      data.frame(lambda = i, coef = 'DFA', val = theta[15]),
      data.frame(lambda = i, coef = 'PPE', val = theta[16]),
      data.frame(lambda = i, coef = 'HNR', val = theta[13]),
      data.frame(lambda = i, coef = 'NHR', val = theta[12]),
      data.frame(lambda = i, coef = 'Shimmer.APQ11', val = theta[10]),
      data.frame(lambda = i, coef = 'Jitter.Abs.', val = theta[2]),
      data.frame(lambda = i, coef = 'Shimmer.APQ5', val = theta[9]),
      data.frame(lambda = i, coef = 'Shimmer', val = theta[6])
      )
  }
  mse_df <- mse_df %>% arrange(data_type, mse)
  cat('MSE for Training and Testing data using ridge reg:\n')
  print(mse_df)
  return(list(mse_df = mse_df, coef_df = coef_df))
}
lambdas = c(1, 100, 1000)
final_list <- RidgeOpt(lambdas)

#Plotting the statistically significant coefficients
ggplot(final_list$coef_df, aes(x=lambda, y = val)) +
  geom_line(aes(color = coef, linetype = coef))

#Plotting the mse's for different lambdas
ggplot(final_list$mse_df, aes(x=lambda, y = mse)) +
  geom_line(aes(color = data_type, linetype = data_type))

DF <- function(lambdas){
  #Slide 22 - has the hat matrix for Ridge Regression
  #To compute the df based on training data
  x <- as.matrix(Strain[-1])
  dim(x)
  degfree_df <- data.frame(matrix(ncol=2, nrow = 0))
  colnames(degfree_df) <- c('lambda', 'df')
  for (i in lambdas) {
    hat_mat <- x %*% solve( t(x)%*%x + i*diag(ncol(x)) ) %*% t(x)
    degfree_df <- rbind(degfree_df,
                        data.frame(lambda = i, df = sum(diag(hat_mat)))  
    )
  }
  return(degfree_df)
}
df <- DF(lambdas)
final_list$mse_df
df_final <- final_list$mse_df %>%
  filter(data_type == 'train') %>%
  inner_join(x=df, y=., by = 'lambda') %>%
  gather(.,key = "metric_type",
         value = "metric_value",
         -lambda, -df, -data_type) %>%
  arrange(metric_type, df)
ggplot(data = df_final, aes(x = df, y = metric_value)) +
  geom_line(aes(color = metric_type))
#As seen as degrees of freedom increases, the MSE decreases, because there are 
#more features to explain. Consequently, the R2 also increases. But at 9DF, we 
#see the graph stabilizing without much decrease/increase. Hence, this could be 
#a good point to select lambda, which in this case is 100.
#TODO: Which penalty term is appropriate

