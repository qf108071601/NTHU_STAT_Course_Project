library(tidyverse)

data = read.csv("C:/Users/Lai/Desktop/bankruptcy.csv")
data$Bankruptcy = as.integer(data$Bankruptcy)

for (i in 2:ncol(data)) {
  data[,i] = as.numeric(data[,i])
}


library(reshape2)
pca<- prcomp(data[,-1], center = F, scale = T)
summary(pca)


ggplot(melt(pca$rotation[,1:9]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())



library(cluster)
data_Positive <- data[data$Bankruptcy==1,];dim(data_Positive)



k.max <- 10
asw <- rep(NA,10)
for(i in 2:k.max){
  asw[i] = clara(data_Positive,i)$silinfo$avg.width
}

k.best <- which.min(asw)
plot(2:10, asw[2:10],
     type="l", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

clustering <- clara(data_Positive,k.best)
data_Positive_cluster <- data.frame(data_Positive,clustering$cluster)

cluster_p = lapply(1:max(data_Positive_cluster$clustering.cluster), function(a){
  data_Positive_cluster[data_Positive_cluster$clustering.cluster==a,][,1:ncol(data)]
})

cluster_p_1 = lapply(1:max(data_Positive_cluster$clustering.cluster), function(a){
  set.seed(12345)
  x = cluster_p[[a]]
  x[sample(nrow(x),0.8*nrow(x),replace=F),]
})

only_1 = cluster_p_1[[1]]
for (i in 2:length(cluster_p_1)) {
  only_1 = rbind(only_1,cluster_p_1[[i]])
}

set.seed(12345)
data_Negative <- data[data$Bankruptcy==0,];dim(data_Negative)
k.max <- 10
asw <- rep(NA,10)
for(i in 2:k.max){
  asw[i] = clara(data_Negative,i)$silinfo$avg.width
}
k.best <- which.min(asw)
plot(2:10, asw[2:10],
     type="l", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

clustering <- clara(data_Negative,k.best)
data_Negative_cluster <- data.frame(data_Negative,clustering$cluster)

cluster_p = lapply(1:max(data_Negative_cluster$clustering.cluster), function(a){
  data_Negative_cluster[data_Negative_cluster$clustering.cluster==a,][,1:ncol(data)]
})

cluster_p_1 = lapply(1:max(data_Negative_cluster$clustering.cluster), function(a){
  set.seed(12345)
  x = cluster_p[[a]]
  x[sample(nrow(x),0.8*nrow(x),replace=F),]
})

only_0 = cluster_p_1[[1]]
for (i in 2:length(cluster_p_1)) {
  only_0 = rbind(only_0,cluster_p_1[[i]])
}

train = rbind(only_0,only_1)
train$Bankruptcy %>% table()

test = data[-as.numeric(rownames(train)),]



table(train$Bankruptcy)
data_Positive <- train[train$Bankruptcy==1,];dim(data_Positive)
k.max <- 10
asw <- rep(NA,10)
for(i in 2:k.max){
  asw[i] = clara(data_Positive,i)$silinfo$avg.width
}

k.best <- which.min(asw)
plot(2:10, asw[2:10],
     type="l", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

clustering <- clara(data_Positive,k.best)
data_Positive_cluster <- data.frame(data_Positive,clustering$cluster)

cluster_p = lapply(1:max(data_Positive_cluster$clustering.cluster), function(a){
  data_Positive_cluster[data_Positive_cluster$clustering.cluster==a,][,1:ncol(data)]
})

cluster_p_1 = lapply(1:max(data_Positive_cluster$clustering.cluster), function(a){
  set.seed(12345)
  x = cluster_p[[a]]
  x[sample(nrow(x),20*nrow(x),replace=T),]
})

only_1 = cluster_p_1[[1]]
for (i in 2:length(cluster_p_1)) {
  only_1 = rbind(only_1,cluster_p_1[[i]])
}

set.seed(12345)
data_Negative <- train[train$Bankruptcy==0,];dim(data_Negative)
k.max <- 10
asw <- rep(NA,10)
for(i in 2:k.max){
  asw[i] = clara(data_Negative,i)$silinfo$avg.width
}
k.best <- which.min(asw)
plot(2:10, asw[2:10],
     type="l", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

clustering <- clara(data_Negative,k.best)
data_Negative_cluster <- data.frame(data_Negative,clustering$cluster)

cluster_p = lapply(1:max(data_Negative_cluster$clustering.cluster), function(a){
  data_Negative_cluster[data_Negative_cluster$clustering.cluster==a,][,1:ncol(data)]
})

cluster_p_1 = lapply(1:max(data_Negative_cluster$clustering.cluster), function(a){
  set.seed(12345)
  x = cluster_p[[a]]
  x[sample(nrow(x),0.7*nrow(x),replace=F),]
})

only_0 = cluster_p_1[[1]]
for (i in 2:length(cluster_p_1)) {
  only_0 = rbind(only_0,cluster_p_1[[i]])
}

train_balance = rbind(only_0,only_1)
train_balance$Bankruptcy %>% table()

library(xgboost)
xgb.params = list(
  objective = "binary:logistic", 
  subsample = 0.4,
  booster="gbtree",
  colsample_bytree = 0.4,
  set.seed = 1234,
  max_depth = 5,
  eta = 0.01, 
  eval_metric = "rmse",
  gamma = 0
)

cv.model <- xgb.cv(
  data = data.matrix(subset(train_balance, select = -Bankruptcy)),
  label = train_balance$Bankruptcy,
  params = xgb.params,
  nrounds = 800,
  nfold = 5,
  print_every_n = 30,
  early_stopping_rounds = 30
)

tmp = cv.model$evaluation_log

plot(x=1:nrow(tmp), y= tmp$train_rmse_mean,
     col='red', xlab="nround", ylab="AUC",type="l", main="Avg.Performance in CV")
lines(x=1:nrow(tmp), y= tmp$test_rmse_mean, col='blue')

legend("topright", pch=1, col = c("red", "blue"),
       legend = c("Train", "Validation") )


best.nrounds = cv.model$best_iteration

xgb.model <- xgboost::xgboost(
  data.matrix(subset(train_balance,
                     select = -Bankruptcy)),
  label = train_balance$Bankruptcy,
  params = xgb.params,
  nrounds = best.nrounds)

dtest = data.matrix(subset(test, select = -Bankruptcy))
xgb.pred = predict(xgb.model,dtest,reshape=T,type="response")
fit = ifelse(xgb.pred>0.5,1,0)
library(precrec)
precrec_obj <- evalmod(scores = xgb.pred, labels = test$Bankruptcy)
autoplot(precrec_obj)

library(caret)
xtab <- table(fit,test$Bankruptcy)
print(confusionMatrix(xtab[2:1,2:1]))


library(vip)
vip(xgb.model, num_features = 20)

select = vip(xgb.model, num_features = 20)
col = select$data[1]

# CART
train_balance_select = train_balance[,c(col$Variable,"Bankruptcy")]

library(reshape2)
M = cor(train_balance_select)
melted_cormat <- melt(M)

ggplot(data = melted_cormat,
       aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

train_balance_select_1 = train_balance_select %>% 
  filter(Bankruptcy==1)

pca<- prcomp(train_balance_select_1[,-21], center = TRUE, scale = TRUE)
summary(pca)

ggplot(melt(pca$rotation[,1:10]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

pca

