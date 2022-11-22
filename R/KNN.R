library(kknn)
library(ggplot2)
library(dplyr)
library(gridExtra)
set.seed(12345)
# options(digits = 3)
#Task 1: Split the data into train, test, validation
data <- read.csv(file = './Data/optdigits.csv',
                 header = F)
n <- nrow(data)
train_index <- sample(1:n,
                      size=floor(n*0.5))#Use floor
index_temp <- setdiff(1:n, train_index)
set.seed(12345)
validation_index <- sample(index_temp, 
                           floor(n*0.25))
test_index <- setdiff(index_temp, validation_index)

train <- data[train_index,]
nrow(train)#1911
head(validation,5)
validation <- data[validation_index, ]
nrow(validation)#955
test <- data[test_index, ]
nrow(test)#957

#Task 2: 30 nearest neighbor in training data
#Ratio of incorrect classification to total
missclass <- function(act, fitted){
  return(1-sum(diag(table(act,fitted)))/length(act))
}
mod_train_test <- kknn(as.factor(V65)~., 
                       train = train,#Model created based on training data
                       test = test,#Estimate the testing data
                       k = 30, kernel = 'rectangular')
test_conf <- table(test$V65, mod_train_test$fitted.values)
print(test_conf)
test_mis_err <- missclass(test$V65, mod_train_test$fitted.values)
print(test_mis_err)#0.0585

mod_train_train <- kknn(as.factor(V65)~.,
                        train = train,#Model created based on training data
                        test = train, #Estimate training data
                        k=30, kernel = 'rectangular')
train_conf <- table(train$V65, mod_train_train$fitted.values)
print(train_conf)
train_mis_err <- missclass(train$V65, mod_train_train$fitted.values)
print(train_mis_err)#0.045

#Quality of prediction on each digit
quality_df <- as.data.frame(matrix(nrow = 0, ncol = 3))
colnames(quality_df) <- c('digit', 'type', 'quality')
for (i in 1:nrow(train_conf)) {
  quality_df <- rbind(quality_df, data.frame(
    digit = colnames(train_conf)[i],
    type = 'train',
    quality = diag(train_conf)[i]/rowSums(train_conf)[i]
  ))
}
for (i in 1:nrow(test_conf)) {
  quality_df <- rbind(quality_df, data.frame(
    digit = colnames(test_conf)[i],
    type = 'test',
    quality = diag(test_conf)[i]/rowSums(test_conf)[i]
  ))
}
quality_df
ggplot(quality_df, aes(x=digit, y=quality, group = 1))+
  geom_point(aes(color=type))
#0,6 are classified almost correctly both in test and train
#1 in test is classified poorly, train is better. Same with 4.

#Task3
#Find 2 cases in training data where 8 was classified easily
final_df <- train #Include the training data
final_df <- cbind(final_df, 
                  data.frame(mod_train_train$prob))#Include the probability matrix
final_df$Pred <- mod_train_train$fitted.values#Include the fitted values
final_df <- final_df %>%
  filter(V65 == 8) %>% #Filter out where actual digit is 8
  # filter(Pred == 8) %>% #Filter out where prediction was 8
  arrange(X8) %>%#Order by the probability of 8
  select(V1:V65, X8, Pred)
digit_8_df <- final_df[1:3,]
digit_8_df <- rbind(digit_8_df, 
                    final_df[nrow(final_df),],
                    final_df[nrow(final_df)-1,]
                    )
mat_list <- list()
for (i in 1:5) {
  mat_list[i] <- i
  mat_list[[i]] <- matrix(data = as.integer(digit_8_df[i,1:64]), 
                 nrow = 8, ncol = 8, 
                 byrow = TRUE)
}
heatmap(mat_list[[1]], Colv = NA, Rowv = NA)
heatmap(mat_list[[2]], Colv = NA, Rowv = NA)
heatmap(mat_list[[3]], Colv = NA, Rowv = NA)
heatmap(mat_list[[4]], Colv = NA, Rowv = NA)
heatmap(mat_list[[5]], Colv = NA, Rowv = NA)

#Task 4:
misclass_df <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(misclass_df) <- c('k_val', 'data_type', 'error')
for (i in 1:30) {
  mod_train_train <- kknn(formula = as.factor(V65)~., 
                          train = train, 
                          test = train, 
                          k = i,
                          kernel = 'rectangular')
  mod_train_valid <- kknn(formula = as.factor(V65)~., 
                          train = train, 
                          test = validation, 
                          k = i,
                          kernel = 'rectangular')
  mod_train_test <- kknn(formula = as.factor(V65)~., 
                          train = train, 
                          test = test, 
                          k = i,
                          kernel = 'rectangular')
  train_mis_err <- missclass(train$V65, mod_train_train$fitted.values)
  valid_mis_err <- missclass(validation$V65, mod_train_valid$fitted.values)
  test_mis_err <- missclass(test$V65, mod_train_test$fitted.values)
  misclass_df <- rbind(misclass_df, 
                       data.frame(k_val = i,
                                  data_type = 'train',
                                  error = train_mis_err),
                       data.frame(k_val = i,
                                  data_type = 'validation',
                                  error = valid_mis_err),
                       data.frame(k_val = i,
                                  data_type = 'test',
                                  error = test_mis_err)) 
}
misclass_df <- arrange(misclass_df, k_val,data_type,error)
ggplot(misclass_df %>% filter(data_type == 'train' | data_type == 'validation'), 
       aes(x = k_val, y = error)) +
  geom_line(aes(color=data_type, linetype=data_type)) +
  labs(x = "K Value", y = "Miss-classfication Error")
#The model complexity is low when k is low as it is easier to classify.
#But as k increases the there are more elements and classification becomes
#more complicated.
#When the model complexity is increasing, the error is also increasing.
#The training error is lesser than the validation error
#Optimal k - 4. This is where the error dips and then grows again.

#Compare the test,validation, train error for the chose k
ggplot(misclass_df %>% filter(k_val == 4), 
       aes(x=k_val, y=error)) +
  geom_point(aes(color = data_type))


#5: Cross-entropy as loss to compute error
cross_entropy_df <- as.data.frame(matrix(nrow=0, ncol=2))
colnames(cross_entropy_df) <- c('KVal', 'Error')
for (kval in 1:30) {
  mod_train_valid <- kknn(formula = as.factor(V65)~., 
                          train = train, 
                          test = validation, 
                          k = kval,
                          kernel = 'rectangular')
  # Combine the probability matrix and the true value
  final_df <- mod_train_valid$prob
  final_df <- cbind(final_df,
                    data.frame(TrueVal = validation$V65))
  #Iterate through each class and find the error
  err <- 0L
  for (i in c(0:9)) {
    # Step 1: Filter by the true class
    filt_df <- final_df %>%
      filter(TrueVal == i)
    # Find the sum of the log(prob+1e-15)
    err <- err + sum(log(filt_df[,i+1]+1e-15))
  }
  cross_entropy_df <- rbind(cross_entropy_df,
        data.frame(KVal = kval,
                   Error = -err))
}
ggplot(cross_entropy_df, aes(x=KVal,y=Error))+
  geom_line()
# The optimal value for k is 5
#Comparing the Misclassification error vs cross-entropy for validation
error_df <- as.data.frame(matrix(nrow = 0, ncol = 3))
colnames(error_df) <- c('KVal', 'ErrType', 'ErrorVal')
error_df <- union(misclass_df %>%
                   filter(data_type == 'validation') %>%
                   mutate(ErrType = 'MissClass') %>%
                   select(KVal = k_val, ErrType, ErrorVal = error),
                 cross_entropy_df %>%
                   mutate(ErrType = 'CrossEntr') %>%
                   select(KVal , ErrType, ErrorVal = Error)
)
plot1 <- ggplot(cross_entropy_df, aes(x=KVal , y=Error)) +
  geom_line() + ggtitle('Cross-entropy error')
plot2 <- ggplot(misclass_df %>%
                  filter(data_type == 'validation'), aes(x=k_val , y=error)) +
  geom_line() + ggtitle('Miss-classification error')
grid.arrange(plot1, plot2)

