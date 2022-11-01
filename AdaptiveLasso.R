library(ridge)
library(tidyverse)
library(dplyr)
library(glmnet)
library(caret)
library(modelr)
library(broom)
library(tidyverse)
library(magrittr)
library(glmnet)
library(pROC)

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

names(cv_train_std)

cv_train_std$glmnet.fit


lambda<-cv_train_std$lambda.min

plot(cv_train_std)
coef(cv_train_std, s = lambda)

## The intercept estimate should be dropped.
best_ridge_coef <- do.call(cbind, coef(cv_train_std, s = lambda))

best_ridge_weights <- 1 / abs(as.matrix(best_ridge_coef)[-1,])

best_ridge_coef


set.seed(1000)
alasso <- cv.glmnet(x = x_train_std, y = y_train_std, weigths = best_ridge_weights,
                    family = "multinomial", nfolds = 10, type.measure ="class",intercept=FALSE,
                    alpha = 1)


coef(alasso,s="lambda.min")
plot(alasso, ylab="Multinomial Deviance")
attributes(alasso)
alasso$glmnet.fit
alambdalasso <- alasso$lambda.min

set.seed(1000)
std_lasso_logit <- glmnet(x_train_std, y_train_std,family="multinomial",type.measure = "class",weigths = best_ridge_weights,alpha=1, lambda = alambdalasso)

coef(std_lasso_logit, s=alambdalasso)

alasso_train <- predict(std_lasso_logit, x_train_std, type="class", s=alambdalasso)

confusionMatrix(as.factor(y_train_std),as.factor(alasso_train), positive="3")


#Test


y_test_std <- as.factor(test_y)
x_test_std <- as.matrix(test_x)
set.seed(1000)
SRL_pred_test <- predict(std_lasso_logit, x_test_std, type="class", s=alambdalasso)

confusionMatrix(as.factor(y_test_std),as.factor(SRL_pred_test), positive="3")



#ROC curve for standardized data
library(ROCR)
library(ggplot2)
library(pROC)


multiclass.roc(y_test_std, as.numeric(SRL_pred_test))

#METRICS
std_lasso_logit$nulldev
names(std_lasso_logit)
#tLL <- -deviance(std_lasso_logit) # 2*log-likelihood
tLL <- (std_lasso_logit$nulldev) - (deviance(std_lasso_logit))
k <- std_lasso_logit$df
n <- std_lasso_logit$nobs
AICcal <- -tLL+2*k+2*k*(k+1)/(n-k-1)
AICcal
AIC_al1 <- -tLL+2*k
AIC_al1
BICr<-log(n)*k - tLL
BICr

