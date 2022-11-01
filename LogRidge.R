library(ridge)
library(tidyverse)
library(dplyr)
library(glmnet)
library(caret)
library(modelr)
library(broom)


df<-read.csv("C:/Users/HP/standartpankreas.csv", header = TRUE,sep=",")
str(df)

df$X<-NULL
df$diagnosis<-as.factor(df$diagnosis)
df$stage <- as.factor(df$stage)
str(df)

set.seed(1000)
train_index <- createDataPartition(df$diagnosis, p = .65,
                                   list = FALSE,
                                   times = 1)

train <- df[train_index,]
test <- df[-train_index,]

train_x <- train %>% dplyr::select(-c("diagnosis","sex","stage","benign_sample_diagnosis"))
train_y <- train$diagnosis

test_x <- test %>% dplyr::select(-c("diagnosis","sex","stage","benign_sample_diagnosis"))
test_y <- test$diagnosis

training <- data.frame(train_x, diagnosis = train_y)


#lambdanýn secilmesi

y_train_std <- as.factor(train_y)

x_train_std <- as.matrix(train_x)

set.seed(1000)
cv_train_std <- cv.glmnet(x_train_std, y_train_std, alpha=0,
                          type.measure = "class", nfolds=10,
                          family="multinomial")

coef(cv_train_std, s=cv_train_std$lambda.min)
names(cv_train_std)
cv_train_std$glmnet.fit
cv_train_std$lambda.min
lambda<-cv_train_std$lambda.min

plot(cv_train_std,ylab="Multinomial Deviance")


set.seed(1000)
std_ridge_logit <- glmnet(x_train_std, y_train_std, family="multinomial",type.measure = "class", alpha=0,lambda =lambda)

coef(std_ridge_logit, s=std_ridge_logit$lambda)


set.seed(1000)
SRL_pred_train <- predict(std_ridge_logit, x_train_std, type="class", s=lambda)

confusionMatrix(as.factor(y_train_std),as.factor(SRL_pred_train), positive="3")



#Test


y_test_std <- as.factor(test_y)
x_test_std <- as.matrix(test_x)

set.seed(1000)
SRL_pred_test <- predict(std_ridge_logit, x_test_std, type="class", s=lambda)
confusionMatrix(as.factor(y_test_std),as.factor(SRL_pred_test), positive="3")




#ROC curve for standardized data
library(ROCR)
library(ggplot2)
library(pROC)


multiclass.roc(y_test_std, as.numeric(SRL_pred_test))

#METRICS

std_ridge_logit$nulldev
names(std_ridge_logit)
#tLL <- -deviance(std_lasso_logit) # 2*log-likelihood
tLL <- (std_ridge_logit$nulldev) - (deviance(std_ridge_logit))
k <- std_ridge_logit$df
n <- std_ridge_logit$nobs
AICcr <- -tLL+2*k+2*k*(k+1)/(n-k-1)
AICcr
AIC_r <- -tLL+2*k
AIC_r
BICr<-log(n)*k - tLL
BICr






