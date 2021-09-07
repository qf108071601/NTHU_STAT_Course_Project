library(openxlsx)
library(tidyverse)
library(purrr)
library(tibble)
library(qs)


SheetList = qread("C:/Users/Lai/Desktop/data")



SheetList = lapply(1:283, function(x){
  data = SheetList[[x]] %>%
    purrr::map(~as.numeric(.)) %>%  
    as.data.frame()
})

firm = c()
firm_index = sapply(1:283, function(a){
  x = SheetList[[a]] %>% purrr::map(~as.numeric(.)) %>%
    as.data.frame()%>%  
    map(~sum(is.na(.))/length(.)) %>% 
    as.data.frame()
  
  if (median(as.numeric(x[1,-1])) >0.3){
    firm[a] = "Delete" #超過3成都是NA
    
  }else if (length(which(x[1,-1] < 0.3))< 16){
    firm[a] = "Specific" # 特定幾行超過是NA
  }else{
    firm[a] = "Normal" #極少部分是NA
  }
})
firm_index %>% table()

filename <-"C:/Users/Lai/Desktop/fuck4.xlsx"
sheets_name <- openxlsx::getSheetNames(filename)
names(SheetList) = sheets_name

Ticker = read.csv("C:/Users/Lai/Desktop/統計學習報告/sp500_ticker.csv")

sheets_name_1 = sapply(1:283, function(a){
  strsplit(sheets_name,split = " ")[[a]][1]
})

sheets_name_1 %in% Ticker$ticker %>%table()



EPS = qread("C:/Users/Lai/Desktop/data2")

E = EPS[c(2:8,10:285)]
names(E) = sheets_name

for (i in 1:283) {
  SheetList[[i]]$EPS = lag(E[[i]]$Diluted.EPS[-1])
}  

C = Ticker$category %>% table() %>% sort(.,decreasing = T) %>% as.data.frame()
C$. %>% View()

###########################
###########################
###########################

index1 = which(Ticker$category==C$.[4]) 

Consumer_Packaged_Goods = SheetList[[index1[1]]]
Consumer_Packaged_Goods$Firm = Ticker$company[index1[1]]

for (i in 2:length(index1)) {
  x = SheetList[[index1[i]]]
  x$Firm = Ticker$company[index1[i]]
  Consumer_Packaged_Goods =rbind(Consumer_Packaged_Goods,x)
}

library(VIM)
aggr_plot <- aggr(Consumer_Packaged_Goods, col=c('navyblue','yellow'), 
                  numbers=TRUE, sortVars=TRUE, labels=names(data), 
                  cex.axis=.7, gap=3, ylab=c("Hist of missing data","Pattern"))

miss = aggr_plot$missings
delete_col = miss[which(miss$Count>0.15*nrow(Consumer_Packaged_Goods)),]
Consumer_Packaged_Goods =select(Consumer_Packaged_Goods,-delete_col$Variable)



library(VIM)
aggr_plot <- aggr(Consumer_Packaged_Goods, col=c('navyblue','yellow'), 
                  numbers=TRUE, sortVars=TRUE, labels=names(data), 
                  cex.axis=.7, gap=3, ylab=c("Hist of missing data","Pattern"))

Consumer_Packaged_Goods = select(Consumer_Packaged_Goods,-Dates)


library(mice)
set.seed(1234)

imputed_Data = mice(Consumer_Packaged_Goods,
                    m=1,
                    maxit = 5,
                    method = 'rf',
                    seed = 123)

Consumer_Packaged_Goods_new = mice::complete(imputed_Data,1)

n = length(levels(as.factor(Consumer_Packaged_Goods_new$Firm)))
library(zoo)
Consumer_Packaged_Goods_new$Dates = 
  rep(paste(rep(2010:2020, each = 4), rep(c(3,6,9,12),10),31, sep = "-")[-44],n)

Consumer_Packaged_Goods_new$Dates = as.zoo(Consumer_Packaged_Goods_new$Dates)
Consumer_Packaged_Goods_new$Firm = as.factor(Consumer_Packaged_Goods_new$Firm)
Consumer_Packaged_Goods_new$EPS = as.numeric(Consumer_Packaged_Goods_new$EPS)
Consumer_Packaged_Goods_new = na.omit(Consumer_Packaged_Goods_new)
s = quantile(Consumer_Packaged_Goods_new$EPS,0.75)
Consumer_Packaged_Goods_new$Y = ifelse(Consumer_Packaged_Goods_new$EPS>s,"1","0")


## 
set.seed(1234)
sampid0 <- sample(0:1, dim(Consumer_Packaged_Goods_new)[1], replace=T, prob=c(0.7, 0.3))      
sampid <- which(sampid0 == 0)

train=Consumer_Packaged_Goods_new[sampid,
                                  which(!(colnames(Consumer_Packaged_Goods_new) %in% 
                                            c("EPS","Firm","Dates")))]

test=Consumer_Packaged_Goods_new[-sampid,which(!(colnames(Consumer_Packaged_Goods_new) %in% 
                                                   c("EPS","Firm","Dates")))]



library(cluster)
#平衡資料----
data_Positive <- train[train$Y==1,];dim(data_Positive)
k.max <- 10
asw <- rep(0,10)
for(i in 2:k.max){
  asw[i] = clara(data_Positive,i)$silinfo$avg.width
}

k.best <- which.max(asw)
plot(2:10, asw[2:10],
     type="l", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

clustering <- clara(data_Positive,k.best)
data_Positive_cluster <- data.frame(data_Positive,clustering$cluster)

cluster_p = lapply(1:max(data_Positive_cluster$clustering.cluster), function(a){
  data_Positive_cluster[data_Positive_cluster$clustering.cluster==a,][,1:ncol(train)]
})

cluster_p_1 = lapply(1:max(data_Positive_cluster$clustering.cluster), function(a){
  set.seed(12345)
  x = cluster_p[[a]]
  x[sample(nrow(x),2*nrow(x),replace=T),]
})

only_1 = cluster_p_1[[1]]
for (i in 2:length(cluster_p_1)) {
  only_1 = rbind(only_1,cluster_p_1[[i]])
}

set.seed(12345)
data_Negative <- train[train$Y==0,];dim(data_Negative)
k.max <- 10
asw <- rep(0,10)
for(i in 2:k.max){
  asw[i] = clara(data_Negative,i)$silinfo$avg.width
}
k.best <- which.max(asw)
plot(2:10, asw[2:10],
     type="l", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

clustering <- clara(data_Negative,k.best)
data_Negative_cluster <- data.frame(data_Negative,clustering$cluster)

cluster_p = lapply(1:max(data_Negative_cluster$clustering.cluster), function(a){
  data_Negative_cluster[data_Negative_cluster$clustering.cluster==a,][,1:ncol(train)]
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
train_balance$Y %>% table()




library(xgboost)
#測試決策樹數量
xgb.params = list(
  objective = "binary:logistic", 
  verbose = 0,
  subsample = 0.4,
  colsample_bytree = 0.3,
  seed = 1234,
  max_depth = 4,
  eta = 0.07, 
  gamma = 0,
  set.seed= 12345
)


cv.model <- xgb.cv(
  data = data.matrix(subset(train_balance, select = -Y),set.seed(12345)),
  label = train_balance$Y,
  params = xgb.params,
  nrounds = 1000,
  nfold = 10,
  print_every_n = 10,
  early_stopping_rounds = 30,
  eval_metric="error"
)

tmp = cv.model$evaluation_log

plot(x=1:nrow(tmp), y= tmp$train_error_mean,
     col='red', xlab="nround", ylab="Error",type="l", main="Avg.Performance in CV")
lines(x=1:nrow(tmp), y= tmp$test_error_mean, col='blue')
legend("topright", pch=1, col = c("red", "blue"),
       legend = c("Train", "Validation") )



# 獲得 best nround
best.nrounds = cv.model$best_iteration
best.nrounds

# 建構模型
xgb.model <- xgboost::xgboost(
  data.matrix(subset(train_balance, 
                     select = -Y)),
  label = train_balance$Y,
  params = xgb.params,
  nrounds = best.nrounds,
)


dtest = data.matrix(subset(test, select = -Y))

# 預測
xgb_y = predict(xgb.model,dtest,type="response")
xgb_y = round(xgb_y)

library(caret)
xtab <- table(xgb_y,test$Y)
print(confusionMatrix(xtab[2:1,2:1]))

library(vip)
vip(xgb.model, num_features = 10)

# library(pdp)
# # c-ICE curves and PDPs for Overall_Qual and Gr_Liv_Area
# x <- data.matrix(subset(train_balance, select = -Y))  # training features
# p1 <- partial(xgb.model,
#               pred.var = "ACCT_RCV_TURN",
#               ice = T, center = TRUE,
#               plot = TRUE, rug = T,type = "auto",
#               alpha = 0.1, plot.engine = "ggplot2",train = x)
# 
# p2 <- partial(xgb.model, pred.var = "INVENT_TURN",
#               ice = T, center = TRUE,
#               plot = TRUE, rug = T, type = "auto",
#               alpha = 0.1, plot.engine = "ggplot2",train = x)
# 
# grid.arrange(p1, p2, ncol =2)


select = vip(xgb.model, num_features = 10)
col = select$data[1]

# CART
train_balance_select = train_balance[,c(col$Variable,"Y")]
test_select = test[,c(col$Variable,"Y")]

library(rpart)
library(rpart.plot)
controls=rpart.control(maxdepth= 4)

cart.model<- rpart(Y ~. ,
                   method = "class",
                   xval= 15,
                   control = controls,
                   data=train_balance_select,
                   parms = list(split = "gini"))
p_y = predict(cart.model,test[,-15],type = "class")
xtab = table(p_y,test$Y)
print(confusionMatrix(xtab[2:1,2:1]))

rpart.plot(cart.model,
           tweak = 1,cex =0.8,
           extra = 104, # show fitted class, probs, percentages
           box.palette = "GnBu", # color scheme
           branch.lty = 2, # dotted branch lines
           nn = TRUE)


select_Company = Consumer_Packaged_Goods_new[
  which(Consumer_Packaged_Goods_new$ALTMAN_Z_SCORE<8.5&
          Consumer_Packaged_Goods_new$EBIT_TO_INT_EXP>5.7&
          Consumer_Packaged_Goods_new$DVD_PAYOUT_RATIO>43&
          Consumer_Packaged_Goods_new$QUICK_RATIO>0.46),]

Consumer_Packaged_Goods_new$Firm %>%table()
select_Company$Firm %>%table() %>%as.matrix()

