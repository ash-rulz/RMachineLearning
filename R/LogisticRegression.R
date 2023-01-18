####Excercise problem####
set.seed(12345)
library(ggplot2)
data <- read.csv(file = './data/pima-indians-diabetes.csv', header = F)
ggplot(data, aes(x = V2, y = V8)) +
  geom_point(aes(color=as.factor(V9))) +
  xlab('Plasma Glucose Concentration') + ylab('Age') +
  labs(colour = 'Diabetes')
# For the majority of the points in the graph we can intuitively categorize them
# based on their location. We can draw a line that will separate the 2 groups which
# will classify the majority of the points into 2 groups.
mod <- glm(as.factor(V9) ~ V2 + V8,
    data = data,
    family = 'binomial')
prob <- predict(mod,
                type = 'response')
pred_0.5 <- ifelse(prob>0.5, 1, 0)
summary(mod)
# Probabilistic Model: 
# P(Diabetes = Yes) = 1/(1+exp^(5.912449 - 0.035644Plasma - 0.024778Age))
missclassError <- function(conf_mat){
  return(1-sum(diag(conf_mat))/sum(conf_mat))
}
conf_mat <- table(data$V9, pred_0.5)
print(conf_mat)
print(missclassError(conf_mat))

#Scatter-plot of the prediction
data$Pred_0.5 <- pred_0.5
plot <- ggplot(data, aes(x = V2, y = V8)) +
  geom_point(aes(color=as.factor(Pred_0.5))) +
  xlab('Plasma Glucose Concentration') + ylab('Age') +
  labs(colour = 'Diabetes')
#The model is bad, as there is high rate of diabetic patients miss classified
#as healthy. 

#Formula for decision matrix. 1/2 = 1/(1+e^-z), z = theta^t * x
dec_bound <- as.data.frame(matrix(nrow=0, ncol = 2))
for (i in 50:200) {
  x <- i
  y <- -(mod$coefficients[1]/mod$coefficients[3]) -
    (mod$coefficients[2]/mod$coefficients[3])*x
  dec_bound <- rbind(dec_bound,
                     data.frame(x=x, y= y))
}
plot + geom_line(data = dec_bound, aes(x=x,  y= y))
#The decision boundary seems to capture all the predicted diabetic patients
#well, but some of the non-diabetic patients are wrongly classified as diabetic

pred_0.2 <- ifelse(prob>0.2, 1, 0)
pred_0.8 <- ifelse(prob>0.8, 1, 0)
data$Pred_0.2 <- pred_0.2
data$Pred_0.8 <- pred_0.8

ggplot(data, aes(x = V2, y = V8)) +
  geom_point(aes(color=as.factor(Pred_0.2))) +
  xlab('Plasma Glucose Concentration') + ylab('Age') +
  labs(colour = 'Diabetes') +
  geom_line(data = dec_bound, aes(x=x,  y= y)) +
  ggtitle('R = 0.2')
conf_mat_0.2 <- table(data$V9, pred_0.2)
print(conf_mat_0.2)
print(missclassError(conf_mat_0.2))
#Although more of diabetic patients are correctly classified, more of 
#non-diabetic patients are wrongly classified as diabetic.

ggplot(data, aes(x = V2, y = V8)) +
  geom_point(aes(color=as.factor(Pred_0.8))) +
  xlab('Plasma Glucose Concentration') + ylab('Age') +
  labs(colour = 'Diabetes') +
  geom_line(data = dec_bound, aes(x=x,  y= y)) +
  ggtitle('R = 0.8')
conf_mat_0.8 <- table(data$V9, pred_0.8)
print(conf_mat_0.8)
print(missclassError(conf_mat_0.8))
#In this case, more diabetic patients are wrongly classified. This is the worst model.

#Basis function expansion trick
data$z1 <- data$V2^4
data$z2 <- data$V2^3*data$V8
data$z3 <- data$V2^2*data$V8^2
data$z4 <- data$V2*data$V8^3
data$z5 <- data$V8^4
basis_mod <- glm(as.factor(V9) ~ V2 + V8 + z1 + z2 + z3 + z4 + z5,
                        data = data,
                        family = 'binomial')
basis_prob <- predict(basis_mod, type = 'response')
basis_pred <- ifelse(basis_prob > 0.5, 1, 0)
data$Pred_Basis <- basis_pred
ggplot(data, aes(x = V2, y = V8)) +
  geom_point(aes(color=as.factor(Pred_Basis))) +
  xlab('Plasma Glucose Concentration') + ylab('Age') +
  labs(colour = 'Diabetes')
conf_mat_basis <- table(data$V9, data$Pred_Basis)
print(conf_mat_basis)
print(missclassError(conf_mat_basis))
#The accuracy has improved a bit

####https://www.projectpro.io/article/example-on-how-to-do-logistic-regression-in-r/542####
train <- read.csv('./Data/TitanicTrain.csv', header = TRUE)
#Data pre-processing
data <- subset(train, select = c(2,3,5,6,7,8,10,12))
data <- data[!is.na(data$Embarked), ]
data$Age[is.na(data$Age)] <- round(mean(data$Age, na.rm = T), 0)
head(data)
data$Sex <- as.factor(data$Sex)
data$Embarked <- as.factor(data$Embarked)

#Split the data to train and test
n <- dim(data)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.7))
train <- data[id,]
test <- data[-id,]

mod <- glm(as.factor(Survived) ~ .,
           data = train,
           family = 'binomial')
summary(mod)

#Evaluate the performance on test data
prob <- predict(mod, newdata = test[,2:8], type = 'response')
pred <- ifelse(prob > 0.5, 1, 0)
test_conf_mat <- table(test$Survived, pred)
test_err <- missclassError(test_conf_mat)

#ROC and AUC curve
library(ROCR)
prob <- predict(mod, newdata = test[,2:8], type = 'response')
pred <- prediction(prob, test$Survived)
roc_curve <- performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(roc_curve)

auc <- performance(pred, measure = 'auc')
auc <- auc@y.values[[1]]
print(auc)#Gives AUC score, ideal score = 1


