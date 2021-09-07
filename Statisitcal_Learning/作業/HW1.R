# (8)
######

#(a)
college = read.csv("C:/Users/nthuqf/Desktop/²Î­p¾Ç²ß/College.csv",header=T)

#(b)
rownames (college )=college [,1]
fix(college)

rownames (college )=college [,1]
fix(college)

college =college [,-1]
fix(college)

#(c)
summary(college)
pairs(college[,2:11])
plot(as.factor(college$Private),college$Outstate)

Elite=rep("No",nrow(college ))
Elite[college$Top10perc >50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college , Elite)
summary(college$Elite)
plot(as.factor(college$Elite),college$Outstate)

par(mfrow=c(2,2))

hist(college$Apps)
hist(college$Accept)
hist(college$Enroll)


hist(college$Top10perc)
hist(college$Top25perc)

hist(college$F.Undergrad)
hist(college$P.Undergrad)

hist(college$Outstate)
hist(college$Room.Board)
hist(college$Books)

hist(college$Personal)
hist(college$PhD)
hist(college$Grad.Rate)


# (d)
library(dplyr)
library(magrittr)

barplot(table(college$Elite))

par(mfrow=c(2,1))

E1 = college %>%
  filter(Elite =="Yes")

E2 = college %>%
  filter(Elite =="No")

hist(E1$Top10perc)
hist(E2$Top10perc)

hist(E1$Grad.Rate)
hist(E2$Grad.Rate)


hist(E1$Outstate)
hist(E2$Outstate)



#(10)
###
library(MASS)


#(b)
data(Boston)
pairs(Boston)
plot(Boston$crim,Boston$age)

#(c)
par(mfrow=c(1,1))
plot(Boston$crim,Boston$tax)

#(d)
plot(Boston$crim,Boston$black)
plot(Boston$crim,Boston$medv)

#(e)
table(Boston$chas)
barplot(table(Boston$chas))

#(f)
median(Boston$ptratio)

#(g)
which(Boston$medv==min(Boston$med))

#(h)
which(Boston$rm > min(Boston$rm)) %>% length()

which(Boston$rm > 8) %>% length()

morethan8 = Boston[which(Boston$rm > 8),]
notmorethan8 = Boston[which(Boston$rm < 8),]

par(mfrow=c(1,2))
hist(morethan8$medv)
hist(notmorethan8$medv)

hist(morethan8$crim,breaks = 20)
hist(notmorethan8$crim)


summary(morethan8)
summary(notmorethan8)
