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
str(df)

set.seed(1000)
train_index <- createDataPartition(df$diagnosis, p = .65,
                                   list = FALSE,
                                   times = 1)

train <- df[train_index,]
test <- df[-train_index,]

train_x <- train %>% dplyr::select(-c("diagnosis","stage","benign_sample_diagnosis","sex"))
train_y <- train$diagnosis

test_x <- test %>% dplyr::select(-c("diagnosis","stage","benign_sample_diagnosis","sex"))
test_y <- test$diagnosis

training <- data.frame(train_x, diagnosis = train_y)


#lambdanýn secilmesi

y_train_std <- as.factor(train_y)

x_train_std <- as.matrix(train_x)


set.seed(1000)
cv_train_std <- cv.glmnet(x_train_std, y_train_std, alpha=seq(0.1,0.9,0.01),
                          type.measure = "class", nfolds=10,
                          family="multinomial")
cv_train_std


alphalýst <-seq(0.1,0.9,0.01)
lambda<-cv_train_std$lambda[which.min(cv_train_std$cvm)]
alpha1<-alphalýst[which.min(cv_train_std$cvm)]


coef(cv_train_std, cv_train_std$lambda.min)

plot(cv_train_std, ylab="Multinomial Deviance")

set.seed(1000)
std_elastic_logit <- glmnet(x_train_std, y_train_std, family="multinomial",type.measure = "class",alpha=alpha1,lambda = lambda)

coef(std_elastic_logit)

set.seed(1000)
SRL_pred_train <- predict(std_elastic_logit, x_train_std, type="class", s=lambda)

confusionMatrix(as.factor(y_train_std),as.factor(SRL_pred_train), positive="3")


#Test

y_test_std <- as.factor(test_y)
x_test_std <- as.matrix(test_x)
set.seed(1000)
SRL_pred_test <- predict(std_elastic_logit, x_test_std, type="class", s=lambda)

confusionMatrix(as.factor(y_test_std),as.factor(SRL_pred_test), positive="3")

library(pROC)

multiclass.roc(y_test_std, as.numeric(SRL_pred_test))

#METRICS

names(std_elastic_logit)
#tLL <- -deviance(std_lasso_logit) # 2*log-likelihood
tLL <- std_elastic_logit$nulldev - deviance(std_elastic_logit)
k <- std_elastic_logit$df
n <- std_elastic_logit$nobs
AICce <- -tLL+2*k+2*k*(k+1)/(n-k-1)
AICce
AIC_e <- -tLL+2*k
BICe<-log(n)*k - tLL
BICe






