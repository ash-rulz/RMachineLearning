####Adaboost from scratch####
#https://www.r-bloggers.com/2019/06/understanding-adaboost-or-how-to-turn-weakness-into-strength/
library(dplyr)
library(rpart)
library(OneR)#For eval_model
data <- iris %>% filter(Species == c('setosa', 'versicolor')) %>%
  select('Sepal.Length', 'Sepal.Width', 'Species')
colnames(data) <- c('Length', 'Width', 'Species')
data$Species <- ifelse(data$Species == 'setosa', 1, -1)

#Step 1: Assign initial weights
w <- rep(1/nrow(data), nrow(data))

#Step 2: Iterate
B <- 100
mod_lst <- list()
alpha <- c()
e_train <- c()
for (b in 1:B) {
  #Step 3: Fit weak learner and predict the y_hat
  #Weight w is learned sequentially and this is what corrects subsequent models.
  #This is how Adaboost learns the mistakes made by previous models.
  mod_lst[[b]] <- rpart(Species ~., data = data, 
                        weights = w, maxdepth = 1, method = "class")
  y_hat <- predict(mod_lst[[b]], 
                   newdata = data[,c('Length', 'Width')], type = 'class')
  #Step 4: Evaluate the error. If correctly classified, 0, else weight
  error <- ifelse(y_hat == data$Species, 0, w)
  e_train <- c(e_train, sum(error))
  
  #Step 5: Calculate confidence/alpha
  alpha <- c(alpha, 0.5 * log((1 - e_train[b])/e_train[b]))
  
  #Step 6: Calculate new weights
  for (i in 1:nrow(data)) {
    #If correctly classified, then lesser weight
    if (y_hat[i] == data$Species[i]) {
      w[i] <- w[i] * exp(-alpha[b])
    }else{
      #If in-correctly classified, then higher weight
      w[i] <- w[i] * exp(alpha[b])
    }
  }
  
  #Step 7: Normalize and update the weight
  w <- w/sum(w)
}
#Plot of Error vs Alpha: Low error for a stump means, it was a good stump and
#it should get a higher alpha/confidence
plot(x = e_train, y = alpha)

# Prediction
new_data <- data[,c('Length', 'Width')]
temp_pred <- sapply(mod_lst, function(x) as.numeric(
  as.character(predict(x, newdata =new_data, type = 'class')))
)
#Multiply each prediction with the corresponding confidence value
temp_pred <- t(alpha * t(temp_pred))
#Sum up the above found result
pred <- sign(rowSums(temp_pred))#We take the sign for classification

eval_model(pred, data$Species)

#Comparison with JOUSBoost
library(JOUSBoost)
## JOUSBoost 2.1.0
boost <- adaboost(as.matrix(data[,c('Length', 'Width')]), 
                  data$Species, tree_depth = 1, n_rounds = B)
pred <- predict(boost, new_data)
eval_model(pred, data$Species)


####Adaboost - exam - See 732A99January2023 under Documents####
#Data setup
set.seed(1234)

x1 <- runif(10000,0,10)
x2 <- runif(10000,-2,2)
y <- ifelse(x2 > sin(x1),1,-1)
plot(x1,x2,col = y + 2)
allData <- cbind(x1,x2,y)
D <- 2

stumpLearn <- function(w){
  
  o1 <- order(dat[,1])
  foo1 <- cumsum(w[o1] * dat[o1,3])
  m1 <- max(abs(foo1))
  i1 <- which.max(abs(foo1))
  
  o2 <- order(dat[,2])
  foo2 <- cumsum(w[o2] * dat[o2,3])
  m2 <- max(abs(foo2))
  i2 <- which.max(abs(foo2))
  
  if(m1>m2){
    i <- 1
    t <- dat[o1[i1],1]
    l <- ifelse(foo1[i1]>0,1,-1)
  }
  else{
    i <- 2
    t <- dat[o2[i2],2]
    l <- ifelse(foo2[i2]>0,1,-1)
  }
  #Return the feature for splitting, splitting value, prediction
  return(list(i=i, t=t, l=l))
}

stumpPredict <- function(x, pars){
  
  foo<-ifelse(x[,pars$i]<pars$t,pars$l,-(pars$l))
  
  return(foo)
}

AdaBoostLearn <- function(){
  
  n <- nrow(dat)
  w <- rep(1/n, times = n)
  alpha <- rep(0,times = B)
  allPars <- rep(list(list()),B)#Stores the models for each B
  for(b in 1:B){
    allPars[[b]] <- stumpLearn(w)#Get the model for the w
    yhat <- stumpPredict(dat[,1:D],allPars[[b]])#Get the prediction
    E <- sum(w * (dat[,D+1] != yhat))#Get the error
    alpha[b] <- 0.5 * log((1-E)/E)#Get the confidence
    w <- w * exp(- alpha[b] * dat[,D+1] * yhat)#Get weights
    w <- w / sum(w)#Normalize weights
  }
  #Return the models and the confidence for each model
  return(list(allPars=allPars, allAlpha=alpha))
}

AdaBoostPredict <- function(x, allPars, allAlpha){
  
  foo <- rep(0,times=nrow(x))
  for(b in 1:B){
    foo <- foo + allAlpha[b] * stumpPredict(x, allPars[[b]])
  }
  foo <- ifelse(foo>0,1,-1)
  
  return(foo)
}

res <- NULL
for(B in 1:50){
  res2 <- NULL
  for(i in 1:10){#For each B, get the average of result
    foo <- sample(1:10000,500)
    dat <- allData[foo,]#Sample of 500 used for training
    tes <- allData[-foo,]#Remaining used for test
    foo <- AdaBoostLearn()#Get the model and confidence for each B
    res2 <- c(res2,mean(#Get the miss-classification error
      AdaBoostPredict(tes[,1:D],foo$allPars,foo$allAlpha) != tes[,D+1]))
  }
  res <- c(res,mean(res2))
}
plot(res, type = "l")
