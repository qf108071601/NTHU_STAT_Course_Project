#Demo: multivariate analysis for inflow data

library(vars)
library(astsa)
#library(MTS)

### EDA (focusing on inflow and precipitation)

data(climhyd)
head(climhyd)
colnames(climhyd)

pairs(climhyd[,6:1], pch=16, cex=0.3, col=4)
#par(mfcol=c(4,1))
#for (i in c(6,1,3,5)){ts.plot(climhyd[,i]);title(colnames(climhyd)[i])}

par(mfcol=c(2,1))
ts.plot(climhyd[,6], ylab="", lwd=2); title(colnames(climhyd)[6])
ts.plot(climhyd[,5], ylab="", lwd=2); title(colnames(climhyd)[5])

# Data transformation
x = climhyd[,6:5]

x[,1] = log(x[,1])   # log(inflow)
x[,2] = sqrt(x[,2])  # sqrt(precipitation)

par(mfcol=c(2,1))
ts.plot(x[,1])
ts.plot(x[,2])

summary(x)
acf(x)
pacf(x)

ccf(x[,1], x[,2], main="Inflow & Precip")


### Vector AR model fitting

fit1 = VAR(x,p=2)
summary(fit1)
par(mfcol=c(1,1))
plot(fit1,names="Inflow")
plot(fit1,names="Precip")

#VAR(2) with seasonal mean effects (period = 12) 

fit2 = VAR(x, p=2, season=12)
summary(fit2)

coef(fit2)    #call partial output
resid(fit2)
fitted(fit2)

plot(fit2,names="Inflow")  #diagnostics plots
plot(fit2,names="Precip")    

normality.test(fit2)  #normality checking
serial.test(fit2,lags.pt=12) #Portmanteau test 
arch.test(fit2)       #ARCH effect test

predict(fit2,12)
plot(predict(fit2,12), lwd=2)


#AIC-selected VAR(p) (p.max = 10) with seasonal mean effects (period = 12) 

fit.aic = VARselect(x,10,season=12)

par(mfcol=c(1,1))
ts.plot(t(fit.aic$crit[1:3,]), col=1:3, lwd=2, xlab="AR Order")
abline(v=fit.aic$sel[1:3],lty=2,col=1:3,lwd=2)
legend("topleft",legend=rownames(fit.aic$crit[1:3,]),col=1:3,lty=1, bty="n")
title("Information Criteria")


fit.aic = VARselect(x,10) #no seasonality
par(mfcol=c(1,1))
ts.plot(t(fit.aic$crit[1:3,]), col=1:3, lwd=2, xlab="AR Order")
abline(v=fit.aic$sel[1:3],lty=2,col=1:3,lwd=2)
legend("topleft",legend=rownames(fit.aic$crit[1:3,]),col=1:3,lty=1, bty="n")
title("Information Criteria")

#Summary for AIC-selected model 

fit3 = VAR(x, p=4, season=12)
summary(fit3)

plot(fit3,names="Inflow")  #diagnostics plots
plot(fit3,names="Precip")    

serial.test(fit3,lags.pt=12) #Portmanteau test 
arch.test(fit3)              #ARCH effect test
par(mfcol=c(1,2))
apply(resid(fit3),2,hist)    #check normality


fit3.pred = predict(fit3, n.ahead=24, ci=0.95)
plot(fit3.pred, lwd=2)


### Test for Granger Causality 

causality(fit3, cause= "Precip") #specify cause and target variables
causality(fit3, cause= "Inflow")


### Impulse response function analysis for VAR model 

a = irf(fit3, response = "Inflow", n.ahead = 24, ortho=FALSE, boot = TRUE)
plot(a)
plot(a, names="Inflow")
plot(a, names="Precip")

a = irf(fit3, response = "Precip", n.ahead = 24, ortho=FALSE, boot = TRUE)

a = irf(fit3, response = "Inflow", n.ahead = 24, ortho=TRUE, boot = TRUE) #orthogonal IRF
plot(a)


### Parameter-constrained fitting (set zero constraints on parameters)

summary(fit3)
fit3.constrained = restrict(fit3, method="ser", thresh=2.0)
summary(fit3.constrained)  

round(Bcoef(fit3.constrained),digit=3)

#setup the constrained matrix:
R1 = (Bcoef(fit3.constrained)!=0)
R1 = matrix(as.numeric(R1), nrow=2)

fit3.constrained = restrict(fit3, method="manual", resmat=R1) 
round(Bcoef(fit3.constrained),digit=3)


