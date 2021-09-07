library(tidyverse)
library(magrittr)
options(scipen = 100) 

# (1)
set.seed(36)
n = 100; sigma = 5; beta0 = c(2,-2,0.5,1,-3)
cormat = diag(1,nrow=5,ncol=5) ; cormat[cormat==0] = 0.5
cholmat = chol(cormat) #Choleskey¤À¸Ñ
x= matrix(rnorm(5*n,0,1), ncol = 5) %*% cholmat
err = rnorm(n,0,sigma)
y = x %*% beta0 + err

# (2a)
library(glmnet)

x_center = sapply(1:5,function(a)
  {
  mean(x[,a])
       }
  )

x_sd = sapply(1:5, function(a){
  sd(x[,a])
  }
  )

z = sapply(1:5, function(a){
  (x[,a] - x_center[a])/x_sd[a]
})

y_1 = y - mean(y)


fit_ridge = glmnet(x = x,y = y,alpha = 0)
par1 = fit_ridge %>% coef(s=0.01) %>% as.numeric()

fit1_ridge = glmnet(x = z,y = y_1,alpha = 0)
par2 = fit1_ridge %>% coef(s=0.01) %>% as.numeric()

library(MASS)
ginv(z) %*% x %*% par1[2:6]

paste0("Standardize: ",par2[2:6]) %>% as.matrix()


ginv(x) %*% z %*% par2[2:6]
paste0("Normal: ",par1[2:6]) %>% as.matrix()


# (2b)
num = 2^c(-10:5)%>% sort(decreasing = T)

par_own = sapply(1:16, function(a){
  a = solve(t(x)%*%x + num[a]*diag(1,nrow=5,ncol=5))
  a %*% t(x) %*% y
})

par_own = sapply(1:16, function(a){
  par_own[,a] = par_own[,17-a]
})


plot(par_own[1,],type = "b",ylim = c(-5,5),pch=2)

for (i in 2:5) {
  par(new=T)
  plot(par_own[i,],type = "b",ylim = c(-5,5),pch=i+1)
}

# (2c)
num = 2^c(-10:5)%>% sort(decreasing = T)
fit2_ridge = glmnet(x = z,
                    y = y_1,
                    alpha = 0,
                    
                    lambda = num,
                    family = "gaussian")
plot(fit2_ridge, xvar = "lambda",label = T)

par_glment =  fit2_ridge$beta %>% as.matrix()

par_glment = sapply(1:16, function(a){
  par_glment[,a] = par_glment[,17-a]
})

vignette("glmnet")





