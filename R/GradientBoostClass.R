####Gradient Boost for Classification - from scratch####
# https://www.youtube.com/watch?v=jxuNLH5dXCs&t=415s
library(tree)
#Data setup
data <- data.frame(
  likes_pop = c('Y', 'Y', 'N', 'Y', 'N' ,'N'),
  age = c(12, 87, 44, 19, 32, 14),
  fav_col = c('Blue', 'Green', 'Blue', 'Red', 'Green', 'Blue'),
  y = c('Y', 'Y', 'N', 'N', 'Y', 'Y')#Loves Troll 2
)
data$likes_pop <- as.factor(data$likes_pop)
data$fav_col <- as.factor(data$fav_col)
data$y <- as.factor(data$y)
head(data)

#Round 1
get_prob <- function(z){
  return(exp(z)/(1+exp(z)))
}
#Get the first prediction - 
n_y <- sum(data$y == 'Y')
n_n <- sum(data$y != 'Y')
#Get the log of the odds
l_odd <- log(n_y/n_n)
#Convert the log of odds to a probability - This is the 1st prediction
data$pred_1 <- get_prob(l_odd)
#Calculate the pseudo-residual
data$resid_1 <- ifelse(data$y == 'Y', 1-data$pred_1, 0-data$pred_1)

#Round 2
#Predict the residual
xvars <- names(data[1:3])
xvars <- paste(xvars, collapse = ' + ') #Concatenate all the Xs together
fml <- paste0('resid_1 ~ ', xvars)
mod <- tree(formula = fml, data = data)
data$pred_2 <- predict(mod, data)
