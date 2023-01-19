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
