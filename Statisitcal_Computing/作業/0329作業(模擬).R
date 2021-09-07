library(tidyverse)
lcg <- function(a,c,m,run.length,seed) {
  x <- rep(0,run.length)
  x[1] <- seed
  for (i in 1:(run.length-1)) {
    x[i+1] <- (a*x[i] + c) %% m
  }
  U <- x/m 
  return(list(x=x,U=U))
}

Uni = lcg(a = 7^5,c = 0,m = 2^31-1,run.length = 10000,seed = 5)
Uni$U %>% as.numeric() %>% hist()

# Problem 1 
## (a)
fnc = function(U1,U2){
  sqrt(-2*log(U1))*cos(2*pi*U2)
}

x = fnc(sample(Uni$U,10000),sample(Uni$U,10000))
y = fnc(sample(Uni$U,10000),sample(Uni$U,10000))
par(mfrow=c(1,2))
hist(x,main = "By Box-Muller");hist(rnorm(10000,0,1),main = "By rnorm")

par(mfrow=c(1,1))

## (b)
# 需要用到指數分配，放在第二題結束的地方

# Problem 2
## (a)
Poisson = function(mu,n){
  X = sapply(1:n, function(a){
    t = 0 ; X = 0 ; lambda = mu
    while (t<1) {
      U = sample(Uni$U,1)
      t = t - (1/lambda)*log(U)
      X = X+1
    }
    X = X-1
    X
  })  
  
  return(X)
}

Poi = Poisson(10,10000)

mean(Poi)
var(Poi)

hist(Poi)
boxplot(Poi)

## (b)
EXP = function(mu,n){
  lambda =mu
  N = sapply(1:n, function(a){
    U = sample(Uni$U,1)
    X = -(1/lambda)*log(U)
  })
  N
}

E = EXP(3,10000)
mean(E)

GAM = function(a,b,n){
  i = 1
  x = matrix(0,nrow = n)
  repeat{
    if(i>a) break
    y = EXP(b,n) %>% as.matrix()
    x = x+y 
    i = i+1
  }
  return(x)
}


G = GAM(5,1/3,10000)
mean(G)
var(G)

## Problem 1
### (b)

E = EXP(1,10000)

standard_normal_accept.reject = function(n) { 
  n.accepts = 0
  result.sample = rep(NA, n)
  while (n.accepts < n) {
    y = sample(E,1)
    u1 = sample(Uni$U,1)       # step 2
    u2 = sample(Uni$U,1)
    if (u1 <= exp((-(y-1)/2))) { # step 3 (accept)
      if (u2<=0.5) {
        y = -y
      }
      n.accepts = n.accepts+1
      result.sample[n.accepts] = y
    }
  }
  result.sample
}
vals <- standard_normal_accept.reject(n = 10000) 
hist(vals, breaks=30, freq=FALSE, main="Sample vs true Density")
xs <- seq(-4,4, len=100)
lines(xs, dnorm(xs,0,1), col="red", lwd=2)
mean(vals);var(vals)


# Problem 3
## (a)

## (b)



## (c)

G = GAM(a = 5,b = 1/3,n = 10000)
x = matrix(0,nrow=10000)
for (i in 1:10000) {
  x[i,] = Poisson(mu = G[i],n=1)
}
mean(x);var(x)
hist(x);
boxplot(x)
   

# Problem 4
## (b)
mix_normal = function(x,p1){
  (p1/sqrt(2*pi))*exp(-x^2/2)+
    ((1-p1)/sqrt(2*pi))*exp((-(x-3)^2)/2)
}




#The number of samples from the mixture distribution
N = 100000                 
#Sample N random uniforms U
U =runif(N)
#Variable to store the samples from the mixture distribution                                             
rand.samples = rep(NA,N)
#Sampling from the mixture
for(i in 1:N){
  if(U[i]<0.75){
    rand.samples[i] = rnorm(n = 1,mean = 0,sd = 1)
  }else{
    rand.samples[i] = rnorm(n = 1,mean = 3,sd = 1)
  }
}

hist(rand.samples)
plot(density(rand.samples),main="Density Estimate of the Mixture Model")



