
# Load data
tox<-read.csv("C:/Users/m48ha/OneDrive/Desktop/tox21.txt", header=TRUE, sep = ',',stringsAsFactors = FALSE )

#Scanning for NAs
colSums(is.na(tox))

# preprocessing
#removes the first variable(nama) and second variable(IC50) from the data set 
x<-tox[,-c(1,2)] 

# load IC50
IC50<-tox[,c(2)]

## x+IC50_org
tox2<-cbind(x,IC50)


selected_columns <- c(1:12)
tox2[selected_columns] <- lapply(tox2[selected_columns], 
                                 function(x) 
                                   replace(x,is.na(x), min(x, na.rm = T)
                                   ))

#Scanning for NAs again
colSums(is.na(tox2))

table(tox2$IC50)  

# install.packages('caTools')
library(caTools)
#The purpose of this classification exercise is to create a supervised training model that is able to predict FDA_APPROVED based on a CT_TOX
#split the data into three sets: 80% train, 10% valid and 10% test
set.seed(263)
train_ind <- sample(1:nrow(tox2), 0.8 * (nrow(tox2)))
training_set <- tox2[train_ind, ]

data2 <-tox2[-train_ind,]
test_ind <- sample(1:nrow(data2), 0.5 * (nrow(data2)))
test_set <- data2[test_ind, ]
valid_set <- data2[-test_ind, ]


#greate matrix for show "auc" result models
auc_result=matrix(0, nrow =8, ncol =1)

# It is also possible to change names
colnames(auc_result) <- c("auc")
rownames(auc_result) <- c("DT","LR","SVM","RF","NN","KNN","Bag.tree","Boost,reg")
###################################### Decision Tree#####################################

#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
fit <- rpart(IC50~., data = training_set, method = 'class') 
rpart.plot(fit)

predict_unseen <-predict(fit, test_set, type = 'class')

table_mat <- table(test_set$IC50, predict_unseen)
table_mat

#Accuracy = (TP + TN)/(TN + FP + FN + TP)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)*100
accuracy_Test

library("ROCR")
Pred.cart = predict(fit, newdata = valid_set, type = "prob")[,2] 
Pred2 = prediction(Pred.cart, valid_set$IC50) 

x.ct.perf <- (performance(Pred2, "tpr", "fpr"))
# add=TRUE draws on the existing chart 
plot(x.ct.perf, col=4, main="ROC curves of different machine learning classifier")

# Draw a legend.
legend(0.7, 0.7, c('DT', 'LR','svm','RF', 'NN'), 4:8)


library(pROC)
predict2<-predict(fit, valid_set, type = 'class')
roc <- roc(valid_set$IC50, as.numeric(predict2))
auc(roc)


auc_result[1,1]<- auc(roc)
############################logistic regression #####################
library(tidyverse)
library(caret)
#Implementation of Logistic Regression to predict the binary outcome
# Fit the model
logistic_model <- glm( IC50 ~., data = training_set, family = binomial)
# Summarize the model
summary(logistic_model)

# Predicting in the test dataset
pred_prob <- predict(logistic_model, test_set, type = "response")
A<- as.matrix(pred_prob)

##Evaluating Logistic Regression Model
# Converting from probability to actual output
pred_class <- ifelse(pred_prob >= 0.018, "1", "0")

# Generating the classification table
ctab_test <- table(test_set$IC50, pred_class)
ctab_test

#Accuracy = (TP + TN)/(TN + FP + FN + TP)
# Accuracy in Test dataset
accuracy_test <- sum(diag(ctab_test))/sum(ctab_test)*100
accuracy_test


p <- predict(logistic_model, newdata=valid_set, type="response")
pr <- prediction(p, valid_set$IC50)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,col=5, add=TRUE)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

auc_result[2,1]<- auc
################################ SVM ################################
require(e1071)
# svm requires tuning
set.seed(549)
x.svm.tune <- tune(svm, IC50~., data = training_set,
                   ranges = list(gamma = 2^(-8:1), cost = 2^(0:4)),
                   tunecontrol = tune.control(sampling = "fix"))
# display the tuning results (in text format)
x.svm.tune

# I manually copied the cost and gamma from console messages above to parameters below.
x.svm <- svm(IC50~., data = training_set, cost=8, gamma=0.0156, probability = TRUE)

# Predicting in the test dataset
pred_prob1 <- predict(x.svm, test_set, type = "response")
#A<- as.matrix(pred_prob)

##Evaluating Logistic Regression Model
# Converting from probability to actual output
pred_class1 <- ifelse(pred_prob1 >= 0.02, "1", "0")

# Generating the classification table
ctab_test1 <- table(test_set$IC50, pred_class1)
ctab_test1

#Accuracy = (TP + TN)/(TN + FP + FN + TP)
# Accuracy in Test dataset
accuracy_test1 <- sum(diag(ctab_test1))/sum(ctab_test1)*100
accuracy_test1


library(ROCR)
p1 <- predict(x.svm, newdata=valid_set, type="response")
pr1 <- prediction(p1, valid_set$IC50)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
plot(prf1,col=6, add=TRUE)

auc <- performance(pr1, measure = "auc")
auc <- auc@y.values[[1]]
auc

auc_result[3,1]<- auc
####################################### Random forest####################
##An implementation of the random forest and bagging ensemble algorithms utilizing conditional inference trees as base learners (from party package)
require(party)

x.cf <- cforest(IC50 ~ ., data=training_set, control = cforest_unbiased(mtry = 10))

# Predicting in the test dataset
x.cf.pred <- predict(x.cf, newdata=test_set)
#pred_prob2 <- predict(x.cf, test_set, type = "response")
#A<- as.matrix(x.cf.pred)

##Evaluating Logistic Regression Model
# Converting from probability to actual output
pred_class2 <- ifelse(x.cf.pred >= 0.05, "1", "0")

# Generating the classification table
ctab_test2 <- table(test_set$IC50, pred_class2)
ctab_test2

#Accuracy = (TP + TN)/(TN + FP + FN + TP)
# Accuracy in Test dataset
accuracy_test2 <- sum(diag(ctab_test2))/sum(ctab_test2)*100
accuracy_test2


library(ROCR)
p2 <- predict(x.cf , newdata=valid_set, type="response")
pr2 <- prediction(p2, valid_set$IC50)
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
plot(prf2,col=7, add=TRUE)

auc <- performance(pr2, measure = "auc")
auc <- auc@y.values[[1]]
auc

auc_result[4,1]<- auc
############################################ neural network #######################
library(caret)
library(nnet)

IC502 <- factor(training_set$IC50, levels = c(0,1), labels = c("YES", "NO"))
length(IC502 )
training_set_nn<-training_set[,-12]
training_set_nn$IC50<- IC502

model_nn <- train(
  IC50 ~ ., training_set_nn,
  method = "nnet",
  metric = "AUC",
  trControl = trainControl(
    method = "cv", 
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
)

model_nn

nnprediction <- predict(model_nn, test_set)
IC503 <- factor(test_set$IC50, levels = c(0,1), labels = c("YES", "NO"))


length(IC503 )
test_set$IC50<- IC503
cmnn <-confusionMatrix(nnprediction,test_set$IC50)
print(cmnn)

# Predicting in the test dataset
x.nn.pred <- predict(model_nn, newdata=test_set)
#pred_prob2 <- predict(x.cf, test_set, type = "response")

# Generating the classification table
ctab_test3 <- table(test_set$IC50, x.nn.pred)
ctab_test3

#Accuracy = (TP + TN)/(TN + FP + FN + TP)
# Accuracy in Test dataset
accuracy_test3 <- sum(diag(ctab_test3))/sum(ctab_test3)*100
accuracy_test3


library(ROCR)
p3 <- predict(model_nn , newdata=valid_set)
p3 <- ifelse(p3=="YES",1,0)
pr3 <- prediction(p3, valid_set$IC50)

prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
plot(prf3,col=8, add=TRUE)

auc <- performance(pr3, measure = "auc")
auc <- auc@y.values[[1]]
auc

auc_result[5,1]<- auc
############################################# KNN ############################
training_set_knn<-training_set[,-12]
random.stuff <- matrix(runif(prod(dim(training_set_knn)), min = -0.00001, max = 0.0001), nrow = nrow(training_set_knn))
random.stuff 

training_set_knn<-random.stuff + training_set_knn
training_set_knn<-cbind(training_set_knn,training_set$IC50)
names(training_set_knn)[12] <- 'IC50'

test_set_knn<-test_set[,-12]
random.stuff <- matrix(runif(prod(dim(test_set_knn)), min = -0.00001, max = 0.0001), nrow = nrow(test_set_knn))
random.stuff 

test_set_knn<-random.stuff + test_set_knn
test_set_knn<-cbind(test_set_knn,test_set$IC50)
names(test_set_knn)[12] <- 'IC50'


valid_set_knn<-valid_set[,-12]
random.stuff <- matrix(runif(prod(dim(valid_set_knn)), min = -0.00001, max = 0.0001), nrow = nrow(valid_set_knn))
random.stuff 

valid_set_knn<-random.stuff + valid_set_knn
valid_set_knn<-cbind(valid_set_knn,valid_set$IC50)
names(valid_set_knn)[12] <- 'IC50'

set.seed(123)
modelknn <- train(IC50~., data=training_set_knn,
                  method="knn",
                  tuneGrid=expand.grid(k=1:30))
modelknn

##$$$$$


x.knn.pred <- predict(modelknn, newdata=test_set_knn)
#pred_prob2 <- predict(x.cf, test_set, type = "response")
#A<- as.matrix(x.knn.pred)

##Evaluating Logistic Regression Model
# Converting from probability to actual output
pred_class4 <- ifelse(x.knn.pred >= 0.05, "1", "0")

# Generating the classification table
ctab_test4 <- table(test_set_knn$IC50, pred_class4)
ctab_test4

#Accuracy = (TP + TN)/(TN + FP + FN + TP)
# Accuracy in Test dataset
accuracy_test4 <- sum(diag(ctab_test4))/sum(ctab_test4)*100
accuracy_test4


library(ROCR)
p4 <- predict(modelknn , newdata=valid_set_knn)
pr4 <- prediction(p4, valid_set_knn$IC50)
prf4 <- performance(pr4, measure = "tpr", x.measure = "fpr")
plot(prf4)

auc <- performance(pr4, measure = "auc")
auc <- auc@y.values[[1]]
auc

auc_result[6,1]<- auc
#################################### Bagged #####################
control = trainControl(method = 'repeatedcv',
                       number = 10,
                       repeats = 3,
                       verbose=TRUE)

# Build a Bagged CART model
#--------------------------
set.seed(7)
mod_Treebag = train(IC50 ~., data=training_set, method='treebag',  trControl=control)
# Predicting in the test dataset
x.bag.pred <- predict(mod_Treebag, newdata=test_set)
#A<-as.matrix(x.bag.pred)
#pred_prob2 <- predict(x.cf, test_set, type = "response")

##Evaluating Logistic Regression Model
# Converting from probability to actual output
pred_class5 <- ifelse(x.bag.pred >= 0.03, "1", "0")

# Generating the classification table
ctab_test5 <- table(test_set$IC50, pred_class5)
ctab_test5

#Accuracy = (TP + TN)/(TN + FP + FN + TP)
# Accuracy in Test dataset
accuracy_test5 <- sum(diag(ctab_test5))/sum(ctab_test5)*100
accuracy_test5


library(ROCR)
p5 <- predict(mod_Treebag, newdata=valid_set_knn)
pr5 <- prediction(p5, valid_set$IC50)
prf5 <- performance(pr5, measure = "tpr", x.measure = "fpr")
plot(prf5)

auc <- performance(pr5, measure = "auc")
auc <- auc@y.values[[1]]
auc

auc_result[7,1]<- auc
####################################### Boost ###################

set.seed(89)
mod_GBM = train(IC50 ~., data=training_set, method='gbm', trControl=control)

# Predicting in the test dataset
x.boo.pred <- predict(mod_GBM, newdata=test_set)
f<-as.matrix(x.boo.pred)
#pred_prob2 <- predict(x.cf, test_set, type = "response")

##Evaluating Logistic Regression Model
# Converting from probability to actual output
pred_class6 <- ifelse(x.boo.pred >= 0.017, "1", "0")

# Generating the classification table
ctab_test6 <- table(test_set$IC50, pred_class6)
ctab_test6

#Accuracy = (TP + TN)/(TN + FP + FN + TP)
# Accuracy in Test dataset
accuracy_test6 <- sum(diag(ctab_test6))/sum(ctab_test6)*100
accuracy_test6


library(ROCR)
p6 <- predict(mod_GBM, newdata=valid_set)
pr6 <- prediction(p6, valid_set$IC50)
prf6 <- performance(pr6, measure = "tpr", x.measure = "fpr")
plot(prf6)

auc <- performance(pr6, measure = "auc")
auc <- auc@y.values[[1]]
auc

auc_result[8,1]<- auc

plot(auc_result)


