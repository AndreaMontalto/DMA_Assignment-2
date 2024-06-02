## ASSIGNMENT 2 DATA MODELLING & ANALYTICS ##
library(dplyr)
library(mlbench)
library(corrplot)
library(car)
library(rpart)
library(rpart.plot)
training <- read.csv('training data set.csv')
testing <- read.csv('testing data set.csv')

# Question 1: Find the portion of observations where y= 1 in training set## 
print(prop.table(table(training$y)))

# Question 2: Find the portion of observations where y = 1 in testing set ## 
print(prop.table(table(testing$y)))

# Question 3: D

# Question 4: build a logistic regression model # 
mod1 <-glm(y ~ ., data = training, family = 'binomial')
summary(mod1)

# Question 5: Check for multicollinearity among varialbes ## 
vif(mod1)


#Question 6: There is no multicollinearity ##

#Question 7: Build predictions using training data set ## 
training$y <- as.factor(training$y)
fit_preds <-predict(mod1, newdata = training, type = "response")
cutoff <- 0.5
factor_levels <- levels(training$y)
training$pred <- factor_levels[(fit_preds > cutoff)+1]
conf_mat <- xtabs(~ y + pred, data=training)
conf_mat
#accuracy of 76.2%

#Question 8: recall computation
#0.698

#Question 9: Build predictions on testing dataset and create confusion matrix # 

testing$y <- as.factor(testing$y)
fit_preds_test <- predict(mod1, newdata = testing, type = "response")
testing$pred <- factor_levels[(fit_preds_test > cutoff)+1]
conf_mat_1 <- xtabs(~ y + pred, data=testing)
conf_mat_1
#accuracy: 0.725

#Question 10 
#recall = 0.637

#Question 11: It is better to test the model on the test data set because it does not make any sense
# to test the model on the same dataset on which it was trained 

#Question 12 
hist(mod1$residuals, breaks = 25, xlab = "Residuals", ylab = "Frequency",
     main = "Histogram of Residuals", col = "white", prob = TRUE)
plot(mod1$residuals, training$x1)
plot(mod1$residuals, training$x2)
plot(mod1$residuals, training$x3)
plot(mod1$residuals, training$x4)
plot(mod1$residuals, training$x5)
plot(training$x1, mod1$residual, xlab="X1", ylab="Residuals",main = "Plot residuals against X1") 

#Question 14:split dataset where x1 is more than 0 and less then 0 
df_train_under <- training %>% 
  filter(training$x1 < 0)

df_train_over <- training %>%
  filter(training$x1 > 0)

mod1a <- glm(y ~ x1+x2+x3+x4+x5, data = df_train_under, family = binomial)
summary(mod1a)
#p-value x1: 0.438

#Question 15: use mod1 and mod1a to make predictins on df_train_under

#USING mod1 ##
fit_preds_train1 <- predict(mod1, newdata = training, type = "response")
df_train_under$pred <- factor_levels[(fit_preds_train1 > cutoff)+1]
conf_mat_2 <- xtabs(~ y + pred, data=df_train_under)
conf_mat_2
#accuracy: 0.737

# USING mod1a ##
fit_preds_train2 <- predict(mod1a, newdata = df_train_under)
df_train_under$pred2 <- factor_levels[(fit_preds_train2 > cutoff)+1]
conf_mat_3 <- xtabs(~ y + pred2, data=df_train_under)
conf_mat_3
#accuracy:0,879 --> way higher 

#Question 17, we have removed non-linearity so we see an improvement in the accuracy of the model 

#Question 18:Build a logistic regression using df_train_over 
df_train_over$y <- as.factor(df_train_over$y)
mod1b <- glm(y ~ x1+x2+x3+x4+x5, data = df_train_over, family = binomial)
summary(mod1b)

#Question 19

predict_lower<-predict(mod1a, newdata = testing, type="response")
predict_high<-predict(mod1b, newdata = testing, type="response")

testing$pred_y<-ifelse(testing$x1<0, factor_levels[(predict_lower>cutoff)+1],factor_levels[(predict_high>cutoff)+1])
cm<-xtabs(~y + pred_y, data=testing)
cm
#accuracy: 0.855

#Question 21: Build a classification tree 
tree1 <- rpart(y ~ x1+ x2 + x3+ x4 + x5, data = training, method = "class", maxdepth = 3)
rpart.plot(tree1)

#Question 23: Out-of-sample accuracy 
testing$pred <- predict(tree1, newdata = testing, type = 'class')
cm1<-xtabs(~y + pred, data=testing)
cm1
#aacuracy: 0,785

#Question 24: tree with more depth
tree2 <- rpart(y ~ x1+ x2 + x3+ x4 + x5, data = training, method = "class", maxdepth = 6)
rpart.plot(tree2)
testing$pred_1 <- predict(tree2, newdata = testing, type = 'class')
cm2<-xtabs(~y + pred_1, data=testing)
cm2
#accuracy: 0.825

#Question 25: tree with more depth
tree3 <- rpart(y ~ x1+ x2 + x3+ x4 + x5, data = training, method = "class", maxdepth = 10)
rpart.plot(tree3)
testing$pred_2 <- predict(tree3, newdata = testing, type = 'class')
cm3<-xtabs(~y + pred_2, data=testing)
cm3
#accuracy: 0,825


#testing if it words