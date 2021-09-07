require(ISLR); require(tidyverse); require(ggthemes);
require(GGally);

set.seed(1)
data('Weekly')

glimpse(Weekly)

Weekly %>% 
  gather(Variable, value, starts_with('Lag'), Today) %>%
  group_by(Variable, Direction) %>%
  summarise(Q25 = quantile(value, 0.25), 
            median = median(value),  
            mean = mean(value),
            Q75 = quantile(value, 0.75))

require(corrplot)
Weekly %>%
  select(-Volume, -Year) %>%
  ggpairs(aes(shape = Direction,col= Direction,  fill = Direction))


Weekly %>%
  gather(value_type, value, starts_with('Lag')) %>%
  ggplot(aes(value_type, value, fill = Direction)) +
  geom_boxplot(notch = F) + 
  labs(x = '', y = '') +
  ylim(c(-6, 6)) + 
  geom_hline(yintercept = 0, linetype = 2)

Weekly %>%
  gather(value_type, value, starts_with('Lag')) %>%
  ggplot(aes(as.factor(Year), value, fill = Direction)) +
  geom_boxplot(notch = F) + 
  labs(x = '', y = '') +
  ylim(c(-6,6)) + 
  geom_hline(yintercept = 0, linetype = 2)


t.test(Lag1 ~ Direction, data = Weekly)
t.test(Lag2 ~ Direction, data = Weekly)

# (b)
Log_ful <- glm(Direction ~ . - Year - Today, data = Weekly, family = 'binomial')
summary(Log_ful)


pred <- predict(Log_ful, type = 'response')
pred_values <- ifelse(pred >= 0.5, 'Up', 'Down')

library(caret)
xtab <- table(pred_values,Weekly$Direction)
print(confusionMatrix(xtab[2:1,2:1]))

## 跟用猜的差不多
mean(Weekly$Direction == 'Up')

#(d)
train <- Weekly[Weekly$Year <= 2008,]
test <- Weekly[Weekly$Year > 2008,]


lag2_logreg <- glm(Direction ~ Lag2, data = train, family = 'binomial')
pred <- predict(lag2_logreg, newdata = test, type = 'response')
pred_values <- ifelse(pred >= 0.5, 'Up', 'Down')

xtab <- table(pred_values,test$Direction)
print(confusionMatrix(xtab[2:1,2:1]))

mean(test$Direction == 'Up')


## (e)
require(MASS)

lda_model <- lda(Direction ~ Lag2, data = train)

pred <- predict(lda_model, newdata = test)
pred_values <- pred$class

xtab <- table(pred_values,test$Direction)
print(confusionMatrix(xtab[2:1,2:1]))



## (g)
qda_model <- qda(Direction ~ Lag2, data = train)

pred <- predict(qda_model, newdata = test)
pred_values <- pred$class

xtab <- table(pred_values,test$Direction)
print(confusionMatrix(xtab[2:1,2:1]))

require(class)

knn_pred <- knn(train = data.frame(train$Lag2), 
                test = data.frame(test$Lag2), 
                cl = train$Direction, k = 1)

xtab <- table(knn_pred,test$Direction)
print(confusionMatrix(xtab[2:1,2:1]))


## (h)


## (i)

acc <- list('1' = 0.5)

for (i in 1:20) {
  knn_pred <- knn(train = data.frame(train$Lag2),
                  test = data.frame(test$Lag2), 
                  cl = train$Direction, k = i)
  acc[as.character(i)] = mean(knn_pred == test$Direction)
}



acc <- unlist(acc)
data_frame(acc = acc) %>%
  mutate(k = row_number()) %>%
  ggplot(aes(k, acc)) +
  geom_col(aes(fill = k == which.max(acc))) +
  labs(x = 'K',
       y = 'Accuracy',
       title = 'KNN Accuracy for different values of K') +
  scale_x_continuous(breaks = 1:20) +
  coord_cartesian(ylim = c(min(acc), max(acc))) +
  guides(fill = FALSE)













