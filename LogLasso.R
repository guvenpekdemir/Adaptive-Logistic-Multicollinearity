library(ridge)
library(tidyverse)
library(dplyr)
library(glmnet)
library(caret)
library(modelr)
library(broom)



df<-read.csv("C:/Users/HP/standartpankreas.csv", header = TRUE,sep=";")

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

#lambdanýn secýlmesi

y_train_std <- as.factor(train_y)

x_train_std <- as.matrix(train_x)

set.seed(1000)
cv_train_stdlasso <- cv.glmnet(x_train_std, y_train_std, alpha=1,
                          type.measure = "class", nfolds=10,
                          family="multinomial")


cv_train_stdlasso

lambdalasso<-cv_train_stdlasso$lambda.min

coef(cv_train_stdlasso, cv_train_stdlasso$lambda.min)

plot(cv_train_stdlasso,ylab="Multinomial Deviance")

set.seed(1000)
std_lasso_logit <- glmnet(x_train_std, y_train_std, family="multinomial",type.measure = "class",alpha=1, lambda = lambdalasso)

coef(std_lasso_logit,s=lambdalasso)

#train
set.seed(1000)
SRL_pred_trainlasso <- predict(std_lasso_logit, x_train_std, type="class", s=lambdalasso)
confusionMatrix(as.factor(y_train_std),as.factor(SRL_pred_trainlasso), positive="3")


#Test

y_test_std <- as.factor(test_y)
x_test_std <- as.matrix(test_x)



set.seed(1000)
SRL_pred_testlasso <- predict(std_lasso_logit, x_test_std, type="class", s=lambdalasso)
confusionMatrix(as.factor(y_test_std),as.factor(SRL_pred_testlasso), positive="3")



library(pROC)
set.seed(1000)
multiclass.roc(y_test_std, as.numeric(SRL_pred_testlasso))


#METRICS

names(std_lasso_logit)
#tLL <- -deviance(fit) # 2*log-likelihood
tLLlasso <- std_lasso_logit$nulldev - deviance(std_lasso_logit)
klasso <- std_lasso_logit$df
nlasso <- std_lasso_logit$nobs
AICclasso <- -tLLlasso+2*klasso+2*klasso*(klasso+1)/(nlasso-klasso-1)
AICclasso
AIC_lasso <- -tLLlasso+2*klasso
AIC_lasso

BIClasso<-log(nlasso)*klasso - tLLlasso
BIClasso





