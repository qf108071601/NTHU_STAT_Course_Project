###Linear Model and plot and table
###2019/09/23

#data
library(MASS)
Boston
head(Boston)
View(Boston)
help(Boston)#F1

#feature
head(Boston$crim,10)
###########plot##############
#scatter plot 
plot(Boston$crim,Boston$medv)
plot(Boston$crim,Boston$medv,pch = 12, cex = 0.5, col = 2,
     xlab = "per capita crime rate by town(crim)", ylab = "price(medv)", main = "Scatter Plot")
#more about plot
plot(1:5, c(2,3,1,5,4))
plot(1:5, c(2,3,1,5,4), type = "p")
plot(1:5, c(2,3,1,5,4), type = "l")
plot(1:5, c(2,3,1,5,4), type = "b")
plot(1:5, c(2,3,1,5,4), type = "b", lwd = 2)

lines(1:5, c(3,4,5,1,2), lty = 2)
points(3,3,col=4,pch=15,cex=4)
abline(a = 1, b = 2, col = "red")
abline(h = 3, col = 4, lty = 3)
abline(v = 3, col = 3, lty = 4)
###########Linear model###########

#one covariate
lm(medv~crim,data=Boston)
#y=Bx
#two covariates
lm(medv~crim+zn,data=Boston)

#covariates and interaction
lm(medv~crim*zn,data=Boston)

#only interaction
lm(medv~I(crim*zn),data=Boston)

#All
lm(medv~.,data=Boston)

#remove one
lm(medv~.-crim,data=Boston)

#remove Intercept
lm(medv~crim+zn-1,data=Boston)

#square
lm(medv~I(crim^2),data=Boston)

#more detail
fit=lm(medv~crim,data=Boston)
summary(fit)

#fit.value
head(fit$fitted.values)
head(fit$residuals)
head(Boston$medv-fit$fitted.values)


sum(fit$residuals^2)
# add regression line
plot(Boston$crim, Boston$medv, pch = 12, cex = 0.5, col = 2,
     xlab = "per capita crime rate by town(crim)", ylab = "price(medv)", main = "Scatter Plot")
abline(fit, col = 4, lwd = 2)

#prediction
predict(fit, newdata = data.frame(crim=c(10,20,30,40,50)))
predict(fit, newdata = data.frame(crim=c(10,20,30,40,50)), interval = "confidence")


############table#########
V1=sample(c("a","b","c"),50,replace = TRUE)
V2=sample(c("d","e","f"),50,replace = TRUE)
V3=sample(c("g","h"),50,replace = TRUE)
test_data=as.data.frame(cbind(V1,V2,V3))
colnames(test_data)=c("abc","def","gh")
#one covariate
table(test_data$abc)
table(test_data$def)
table(test_data$gh)
#two covariate
table(test_data$abc,test_data$def)
table(test_data$abc,test_data$gh)
#three covariate
table(test_data$abc,test_data$def,test_data$gh)




