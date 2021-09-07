library(MASS)
library(tidyverse)
# 造二維常態分配
## X
mu <- c(1,2) # Mean
s1 = 1;s2=2;rho=0.4
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) # Covariance matrix

bvn_X <- mvrnorm(5000, mu = mu, Sigma = sigma ) # from MASS package
colnames(bvn_X) <- c("bvn1_X1","bvn1_X2")

## Z
mu <- c(2,1) # Mean
s1 = 2;s2=1;rho=0.6
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) # Covariance matrix

bvn_Z <- mvrnorm(5000, mu = mu, Sigma = sigma ) # from MASS package
colnames(bvn_Z) <- c("bvn1_Z1","bvn1_Z2")


plot(bvn_X)
points(bvn_Z,col=2)

set.seed(12345)
u = runif(n = 5000,min = 0,max = 1)

Y = matrix(NA, nrow = 100, ncol = 2)

set.seed(12345)
for (i in 1:100) {
  index = sample(1:5000,1)
  x = u[index]
  if (x<0.6) {
    Y[i,] = bvn_X[index,]
  }else{
    Y[i,] = bvn_Z[index,]
  }
}  

index_x = which(Y[,1] %in% bvn_X[,1])
index_z = which(Y[,1] %in% bvn_Z[,1])



plot(bvn_X[index_x,],pch=1,main = "Real Cluster",
     xlab = "",ylab = "",xlim = c(-3,6),ylim = c(-3,6))
points(bvn_Z[index_z,],pch=2,col=2)
legend("topleft", legend=c("X", "Z"),
       col=c("black", "red"), pch=1:2, cex=1)


d <- dist(Y, method = "euclidean")
hc1 <- hclust(d, method = "ward.D2" )
plot(hc1, cex = 0.6)

rect.hclust(hc1, k = 2)
x = rect.hclust(hc1, k = 2)
 
h_1 = Y[x[[1]],]
h_2 = Y[x[[2]],]

plot(h_1,pch=1,main = "Hierarchical Cluster",
     xlab = "",ylab = "",xlim = c(-3,6),ylim = c(-3,6))
points(h_2,col=2,pch=2)
legend("topleft", legend=c("h_1", "h_2"),
       col=c("black", "red"), pch=1:2, cex=1)



# 3
X1 = rnorm(n = 5000,mean = 0,sd = 1)
X2 = rnorm(n = 5000,mean = 3,sd = 1)

Output = lapply(1:200, function(a){
  Y = matrix(NA, nrow = 100, ncol = 1)
  for (i in 1:100) {
    index = sample(1:5000,1)
    x = u[index]
    if (x<0.6) {
      Y[i] = X1[index]
    }else{
      Y[i] = X2[index]
    }
  }
  
  Y = Y %>% as.data.frame()
  Y$K = ifelse(Y[,1] %in% X1,"1","2") %>% as.matrix()
  
  kmeans <- kmeans(Y[,1], centers=2) 
  
  kk = table(prob = kmeans$cluster,True = Y$K) 
  
  # 如果分群顛倒的話，轉回來
  if (kk[1,1]<kk[1,2]) {
    Y$Kmeans = ifelse(kmeans$cluster=="1","2","1")
  } else{
    Y$Kmeans = ifelse(kmeans$cluster=="1","1","2")
  }
  return(Y)
})

Accuracy_Kmeans = sapply(1:200, function(a){
  x = Output[[a]][,c("K","Kmeans")]
  xx = which(x$K==x$Kmeans) %>% length()
  xx/nrow(x)
})
hist(Accuracy_Kmeans)
 
source("EM.R")

EM = lapply(1:200, function(a){
  t = Output[[a]]
  
  t.summary.df <- t %>%
    group_by(Kmeans) %>%
    summarize(mu = mean(V1), variance = var(V1), std = sd(V1), size = n())
  
  t.summary.df <- t.summary.df %>%
    mutate(alpha = size / sum(size))
  
  
  for (i in 1:50) {
    if (i == 1) {
      # Initialization
      e.step <- e_step(t$V1, t.summary.df[["mu"]], t.summary.df[["std"]],
                       t.summary.df[["alpha"]])
      m.step <- m_step(t$V1, e.step[["posterior.df"]])
      cur.loglik <- e.step[["loglik"]]
      loglik.vector <- e.step[["loglik"]]
    } else {
      # Repeat E and M steps till convergence
      e.step <- e_step(t$V1, m.step[["mu"]], sqrt(m.step[["var"]]), 
                       m.step[["alpha"]])
      m.step <- m_step(t$V1, e.step[["posterior.df"]])
      
      # 紀錄log-Likelihood
      loglik.vector <- c(loglik.vector, e.step[["loglik"]])
      
      loglik.diff <- abs((cur.loglik - e.step[["loglik"]]))
      if(loglik.diff < 1e-4) {
        break
      } else {
        cur.loglik <- e.step[["loglik"]]
      }
    }
  }
  return(list(e.step = e.step,
              m.step = m.step,
              loglik.vector = loglik.vector))
})


for (i in 1:200) {
  Output[[i]]$EM = ifelse(EM[[i]]$e.step$prob>0.5,"1","2")
}

Accuracy_EM = sapply(1:200, function(a){
  x = Output[[a]][,c("K","EM")]
  xx = which(x$K==x$EM) %>% length()
  xx/nrow(x)
})
hist(Accuracy_EM)










