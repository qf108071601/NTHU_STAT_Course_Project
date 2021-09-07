# 4
library(tidyverse)

set.seed(123456)
X1 = rnorm(10000)
X2 = runif(10000)
X3 = rexp(10000)
X4 = rt(10000,10)

Y = 10*X1+15*X2+20*X3+25*X4

data= cbind(X1,X2,X3,X4,Y) %>% as.data.frame()
library(tree)

data = mtcars
controls = tree.control(nobs = 33,mincut = 5,minsize = 10)
tree.Y = tree(mpg~.,control = controls,data = data)
summary(tree.Y)
plot(tree.Y)

# 8
## (a)
library(ISLR)
library(tree)
data("Carseats")
Carseats$Sales_1 = as.factor(ifelse(Carseats$Sales <= 8, "Low", "High"))
set.seed(1234567)
index = sample(1:400,size = 320,replace = F)
train = Carseats[index,]
test = Carseats[-index,]

## (b)
model = tree(Sales~.,data = train[,-12])
plot(model)
text(model)
summary(model)
fit= predict(model,test[,-12])
sum((test$Sales - fit)^2)/nrow(test) #rmse

fit = ifelse(fit<=8,"Low","High")
table(predicted = fit, actual = test$Sales_1)

accuracy = function(actual, predicted) {
  mean(actual == predicted)
}
1-accuracy(predicted = fit, actual = test$Sales_1)



## (c)
set.seed(1234)
tree_cv = cv.tree(model, FUN = prune.tree,K = 10)
min_idx = which.min(tree_cv$dev)
tree_cv$size[min_idx]
tree_cv$dev/length(index)

# default plot
plot(tree_cv)
tree_prune = prune.tree(model,best = tree_cv$size[min_idx])
fit= predict(tree_prune,test)
sum((test$Sales - fit)^2)/nrow(test) #rmse

fit = ifelse(fit<=8,"Low","High")
table(predicted = fit, actual = test$Sales_1)
1-accuracy(predicted = fit, actual = test$Sales_1)

##(d)
library(randomForest)
bag = randomForest(Sales ~., data = train[,-12], mtry= 10,
                  importance = TRUE, ntrees = 500)
fit= predict(bag,test[,-12])
sum((test$Sales - fit)^2)/nrow(test) #rmse

fit = ifelse(fit<=8,"Low","High")
table(predicted = fit, actual = test$Sales_1)
1-accuracy(predicted = fit, actual = test$Sales_1)

varImpPlot(bag, type = 1)
## (e)
library(randomForest)
rf = randomForest(Sales ~., data = train[,-12],mtry=4,
                          importance = TRUE, ntrees = 500)
fit= predict(rf,test[,-12])
sum((test$Sales - fit)^2)/nrow(test) #rmse

fit = ifelse(fit<=8,"Low","High")
table(predicted = fit, actual = test$Sales_1)
1-accuracy(predicted = fit, actual = test$Sales_1)

varImpPlot(rf, type = 1)







# (9)
## (a)
data(OJ)
index = sample(1:1070,800,replace = F)
train = OJ[index,]
test = OJ[-index,]
## (b)
tree = tree(Purchase~.,data = train)
summary(tree)

#(c)
tree


#(d)
plot(tree)
text(tree)

#(e)
fit=predict(tree,test,type = "class")
table(predicted = fit, actual = test$Purchase)
1- accuracy(predicted = fit, actual = test$Purchase)

#(f)
cv_tree = cv.tree(tree, FUN = prune.misclass)


#(g)
plot(cv_tree)

#(h)
min_idx = which.min(cv_tree$dev)
cv_tree$size[min_idx]

#(i)
tree_prune = prune.tree(tree,best = cv_tree$size[min_idx])
fit= predict(tree_prune,test,type="class")
table(predicted = fit, actual = test$Purchase)
1- accuracy(predicted = fit, actual = test$Purchase)

#(j)
fit= predict(tree,train,type="class")
fit_1= predict(tree_prune,train,type="class")

cat("Train_Unpruned Error Rate: ",1- accuracy(predicted = fit, actual = train$Purchase))
cat("Train_Pruned Error Rate: ",1- accuracy(predicted = fit_1, actual = train$Purchase))

#(k)
fit= predict(tree,test,type="class")
fit_1= predict(tree_prune,test,type="class")

cat("Test_Unpruned Error Rate: ",1- accuracy(predicted = fit, actual = test$Purchase))
cat("Test_Pruned Error Rate: ",1- accuracy(predicted = fit_1, actual = test$Purchase))


#(10)
## (a)
data("Hitters")
Hitters = na.omit(Hitters)
Hitters$Salary = log(Hitters$Salary)

## (b)
train = Hitters[1:200,]
test = Hitters[201:263,]

## (c)
library(gbm)
s = seq(0.001,0.01,by = 0.001)

t = sapply(1:10, function(a){
  boost = gbm(Salary ~ ., data = train, distribution = "gaussian", 
                    n.trees = 1000, interaction.depth = 4, shrinkage = s[a])
  c(boost$shrinkage,tail(boost$train.error,1))
})
t = t(t) %>% as.data.frame()
names(t)= c("shrinkage","train_error")
plot(x=t$shrinkage,y=t$train_error,type="l",xlab = "shrinkage",ylab = "Train Error")

## (d)
s = seq(0.001,0.01,by = 0.001)
t = sapply(1:10, function(a){
  boost = gbm(Salary ~ ., data = train, distribution = "gaussian", 
              n.trees = 1000, interaction.depth = 4, shrinkage = s[a])
  fit = predict(boost,test)
  mse = mean((fit-test$Salary)^2)
  c(boost$shrinkage,mse)
})
t = t(t) %>% as.data.frame()
names(t)= c("shrinkage","test_error")
plot(x=t$shrinkage,y=t$test_error,type="l",xlab = "shrinkage",ylab = "Test Error")

#(e)
library(glmnet)
lm = lm(Salary ~ ., data = train)
fit = predict(lm,test)
mean((fit - test$Salary)^2)

train_1 = model.matrix(Salary ~ ., data = train)
test_1 = model.matrix(Salary ~ ., data = test)
y = train$Salary
lasso = glmnet(train_1, y, alpha = 0)
fit = predict(lasso, s = 0.01, newx = test_1)
mean((fit - test$Salary)^2)


#(f)
summary(boost)

#(g)
bag <- randomForest(Salary ~ ., data = train, mtry = 19, ntree = 500)
fit <- predict(bag, newdata = test)
mean((fit - test$Salary)^2)


