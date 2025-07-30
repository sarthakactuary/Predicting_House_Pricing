HousePrice <- read.csv("original_data.csv")
head(HousePrice)
colnames(HousePrice)
colSums(is.na(HousePrice))
sapply(HousePrice, is.numeric)

colSums(HousePrice==0)
HousePrice <- subset(HousePrice, bedrooms!=0 & bathrooms!=0)
HousePrice$price[HousePrice$price==0] <- median(HousePrice$price)
colSums(HousePrice==0)

sum(HousePrice$sqft_living)==sum(HousePrice$sqft_above+HousePrice$sqft_basement)
summary(HousePrice)

install.packages("dplyr")

library(dplyr)

hist(HousePrice$price,breaks = 50, main = "Price Distribution", xlab="Price",prob = TRUE)
lines(density(HousePrice$price),col="blue",lwd=2)
install.packages("e1071")

library(e1071)
skewness(HousePrice$price)
boxplot(HousePrice$price, horizontal = TRUE, main="Boxplot of Price")

library(ggplot2)
ggplot(HousePrice, aes(x=price))+
  geom_histogram(bins=50,fill="darkgreen")+
  labs(title="Distribution of price")+
  theme_minimal()
ggplot(HousePrice, aes(x=price))+
  geom_boxplot(fill="skyblue")+
  labs(title="price",ylab="Price")+
  theme_minimal()

install.packages("tidyr")
library(tidyr)

data_long <- pivot_longer(
  HousePrice,
  cols = c(price, sqft_living, sqft_lot),
  names_to = "Variable",
  values_to = "Value"
)

ggplot(data_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "orchid") +
  labs(title = "Boxplots of Multiple Variables",
       y = "Value")+
  theme_minimal()

ggplot(HousePrice, aes(x = factor(floors), y = price)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Price by Number of Bedrooms",
       x = "Bedrooms",
       y = "Price") +
  theme_minimal()

ggplot(HousePrice, aes(x=log(price)))+
  geom_histogram(bins=50,fill="darkgreen")+
  labs(title="Distribution of price")+
  theme_minimal()

install.packages("moments")
library(moments)
skewness(HousePrice$price)
skewness(log(HousePrice$price))
ggplot(HousePrice, aes(x=log(price)))+
  geom_histogram()
mean(HousePrice$price)>median(HousePrice$price)

#that mean's right skewed
HousePrice$view <- factor(HousePrice$view, ordered = TRUE)
HousePrice$condition <- factor(HousePrice$condition, ordered = TRUE)
HousePrice$city <- factor(HousePrice$city, ordered = TRUE)
HousePrice$statezip <- factor(HousePrice$statezip, ordered = TRUE)


cor(HousePrice[,sapply(HousePrice,is.numeric)])
sapply(HousePrice, function(x) length(unique(x)))

install.packages("caret")
library(caret)
nearZeroVar(HousePrice, saveMetrics = TRUE)
sapply(HousePrice,n_distinct)

HousePrice$price_bins <- cut(HousePrice$price,
                             breaks=quantile(HousePrice$price, probs = seq(0,1,0.1)),
                             include.lowest=TRUE)
set.seed(123)
split_index <- createDataPartition(HousePrice$price_bins,p=0.7,list = FALSE)

train_data <- HousePrice[split_index,]
test_data <- HousePrice[-split_index,]
summary(train_data)
summary(test_data)





train_data <- train_data %>% select(-c(country,price_bins))
test_data <- test_data %>% select(-c(country,price_bins))

str(train_data)

#start with numeric variable
model_lm1 <- lm(price~bedrooms+bathrooms+floors+sqft_lot+sqft_above+sqft_basement+yr_built,data=train_data)
summary(model_lm1)

#replace basement and above to living
model_lm2 <- lm(price~bedrooms+bathrooms+floors+sqft_lot+yr_built,data=train_data)
summary(model_lm2)

#apply log into model_lm2
model_lm2_log <- lm(log(price)~bedrooms+bathrooms+floors+sqft_lot+sqft_living+yr_built,data=train_data)
summary(model_lm2_log)

#removing sq ft_lot terms
model_lm3 <- lm(price~bedrooms+bathrooms+floors+sqft_living+yr_built,data=train_data)
summary(model_lm3)

#adding interaction term
model_lm4 <- lm(price~bedrooms*bathrooms*sqft_living+bathrooms:floors+floors+yr_built+bathrooms:yr_built+floors+sqft_lot+yr_built,data=train_data)
summary(model_lm4)

#log(price)
model_lm5 <- lm(log(price)~bedrooms*bathrooms*sqft_living+bathrooms:floors+floors+yr_built+bathrooms:yr_built+floors+sqft_lot+yr_built,data=train_data)
summary(model_lm5)

#removing sq ft_lot in log model
model_lm6 <- lm(log(price)~bedrooms*bathrooms*sqft_living+bathrooms:floors+floors+yr_built+bathrooms:yr_built+floors+yr_built,data=train_data)
summary(model_lm6)

#changing some factors
model_lm7 <- lm(price~bedrooms*bathrooms*sqft_living+floors+bathrooms:yr_built+sqft_lot,data=train_data)
summary(model_lm7)
predict_lm7 <- predict(model_lm7,newdata = test_data)
rmse_lm7 <- sqrt(mean((test_data$price-predict_lm7)^2))
print(rmse_lm7)
AIC(model_lm7)

#before go with category variable just check there relation with price
summary(aov(price~condition,data=HousePrice))
cor(as.numeric(HousePrice$condition),HousePrice$price,method = "spearman")
cor(as.numeric(HousePrice$condition),HousePrice$price,method = "kendall")
kruskal.test(price~condition,data=HousePrice)
library(dplyr)
HousePrice %>%
  group_by(condition) %>%
  summarise(median_price=median(price))
HousePrice %>%
  group_by(view) %>%
  summarise(median_price = median(price))
summary(aov(price~city,data=train_data))
kruskal.test(price~city, data=train_data)


#more changes
model_lm8 <- lm(log(price)~bedrooms*bathrooms*sqft_living+floors+bathrooms:yr_built+sqft_lot+waterfront+factor(view)+factor(condition)+factor(statezip),data=train_data)
summary(model_lm8)
install.packages("MASS")
library(MASS)
stepAIC(model_lm8)

predict_lm8 <- exp(predict(model_lm8,test_data))
rmse_lm8 <- sqrt(mean((test_data$price-predict_lm8)^2))
print(rmse_lm8)
plot(model_lm8,which = 1)
plot(model_lm8,which = 2)

#model_lm8 good but it's over fit as you see there was so many predictors.

#now jump to random-forest
library(dplyr)
HousePrice %>%
  group_by(floors) %>%
  summarise(median_per_view <- median(HousePrice$price))

ggplot(HousePrice, aes(x=factor(floors),y=price))+
  geom_boxplot(fill="skyblue")+
  theme_minimal()



install.packages("randomForest")
library(caret)
library(randomForest)
library(doParallel)
library(caret)
cl <- makeCluster(4)
registerDoParallel(cl)
names(train_data)
train_data$view <- factor(train_data$view)
train_data$condition <- factor(train_data$condition)
train_data$city <- factor(train_data$city)
train_data$statezip <- factor(train_data$statezip)


model_rf1 <- randomForest(price~bathrooms+sqft_living+floors+yr_built+sqft_lot+waterfront+view+condition+yr_renovated+statezip+city,data=train_data,ntree=600,importance=TRUE,do.trace=100,mtry=4)
varImp(model_rf1)
ctrl <- trainControl(method="cv",number=10,savePredictions = "final")
library(doParallel)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
model_rf1 <- train(price~bathrooms+sqft_living+floors+yr_built+sqft_lot+factor(condition)+factor(view)+waterfront+yr_renovated+factor(city)+factor(statezip),data=train_data,ntree=200,importance=TRUE,)



importance(model_rf1)
predict_rf1 <- predict(model_rf1,test_data)
rmse_rf1 <- sqrt(mean((test_data$price-predict_rf1)^2))
print(rmse_rf1)
tuneRF(
  train_data[, c("bathrooms", "sqft_living", "floors", "yr_built", "sqft_lot", "waterfront", "view", "condition", "yr_renovated", "statezip","city")],
  train_data$price,
  ntreeTry = 500,
  stepFactor = 1.5,
  improve = 0.01
)

#time to use train function
library(dplyr)
names(train_data)
sapply(train_data, function(x) if (is.factor(x)) nlevels(x))
train_data_clean <- train_data %>%
  select(-country, - Check.sqft_living, - price_bins, - date, - street, -city)


model_train1_log <- train(log(price) ~ .,
                      data=train_data_clean,
                      method="rf",
                      tuneGrid=data.frame(mtry=c(2,4,6)),
                      ntree=500,
                      trControl=trainControl(method="cv",number=5))

model_train1 <- train(price ~ .,
                      data=train_data_clean,
                      method="rf",
                      tuneGrid=data.frame(mtry=c(2,4,6)),
                      ntree=500,
                      trControl=trainControl(method="cv",number=5))

pred_train1 <- predict(model_train1,test_data)
pred_train1_log <- exp(predict(model_train1_log,test_data))
rmse_train1_log <- sqrt(mean((test_data$price-pred_train1_log)^2))
rmse_train1 <- sqrt(mean((test_data$price-pred_train1)^2))
print(rmse_train1_log)
print(rmse_train1)
postResample(pred_train1,test_data$price)

model_glm <- glm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+yr_built,
                 data=train_data,
                 family = tweedie(var.power=1.5,link.power=0))
summary(model_glm)

pred_glm <- predict(model_glm,test_data,type = "response")
rmse_glm <- sqrt(mean((test_data$price-pred_glm)^2))
print(rmse_glm)


install.packages("car")
library(car)

#making some changes
train_data$log_price <- log(train_data$price)
upper_limit <- quantile(train_data$price,0.99)
train_data$price <- pmin(train_data$price,upper_limit)
train_data$log_sqft_living <- log(train_data$sqft_living)
train_data$log_sqft_lot <- log(train_data$sqft_lot)
train_data$sqft_living <- pmin(train_data$sqft_living,quantile(train_data$sqft_living,0.99))
train_data$sqft_lot <- pmin(train_data$sqft_lot,quantile(train_data$sqft_lot,0.99))

library(MASS)
model_glm2 <- glm(price~bedrooms*bathrooms*log_sqft_living+log_sqft_lot+yr_built+factor(condition)+factor(view)+waterfront+floors,
                  data = train_data,
                  family = Gamma(link = "log"))
summary(model_glm2)

model_lm9 <- lm(log_price~bedrooms+bathrooms+log_sqft_living+waterfront+sqft_lot+view+condition+floors+yr_built+factor(city)+factor(statezip),
                data=train_data)
summary(model_lm9)
plot(model_lm9,which=2)
predict(model_lm9,test_data)
library(moments)
skewness(train_data$price)




model_lm2 <- lm(log(price)~bedrooms+bathrooms+floors+sqft_lot+sqft_above+sqft_basement+yr_built+view+condition+waterfront,data=train_data)
summary(model_lm2)
table(train_data$city)
table(test_data$city)
model_glm2 <- glm(price~bedrooms+bathrooms+floors+sqft_lot+sqft_above+sqft_basement+yr_built+view+condition+waterfront,data=train_data, family = Gamma(link="log"))
summary(model_glm2)
AIC(model_lm1,model_lm2)
AIC(model_glm2)
plot(model_glm2,which = 1)

pred_lm2_log <- predict(model_lm2,newdata = test_data)
pred_lm2 <- exp(pred_lm2_log)

library(car)
vif(model_lm2)
rmsemodel_lm2rmse_lm2 <- sqrt(mean((test_data$price-pred_lm2)^2))
rmse_lm2/median(test_data$price)

pred_glm2 <- predict(model_glm2,newdata = test_data)
rmse_glm2 <- sqrt(mean((test_data$price-pred_glm2)^2))
rmse_glm2

#using train function predict model random forest
library(caret)

ctrl <- trainControl(
  method = "repeatedcv",
  number=10,
  repeats = 3,
  savePredictions = "final")

train_data$city <- factor(train_data$city)
test_data$city <- factor(test_data$city)
install.packages("randomForest")
library(randomForest)
model_rf <- train(price~bedrooms+bathrooms+floors+sqft_lot+sqft_above+sqft_basement+yr_built+view+condition+waterfront+city,
                  data=train_data,
                  method="rf",
                  trControl=ctrl,
                  tuneGrid=data.frame(mtry=c(4,5,6)),
                  ntree=500,
                  importance=TRUE)
model_rf
model_rf_vars <- names(model_rf$trainingData)[-1]
test_data_subset <- test_data[,model_rf_vars]
colSums(is.na(test_data_subset))
names(table(train_data$city))==names(table(test_data$city))
test_data_subset$city <- factor(
  test_data_subset$city,
  levels = levels(train_data$city)
)

pred_rf <- predict(model_rf,newdata = test_data_subset)
rmse_rf <- sqrt(mean((test_data$price-pred_rf)^2))
rmse_rf
rmse_lm2
sum(is.na(pred_rf))
varImp(model_rf)

install.packages("doParallel")
library(doParallel)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

ctrl2 <- trainControl(
  method = "cv",
  number=10,
  savePredictions = "final",
  allowParallel = TRUE)

model_rf2 <- train(log(price)~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+yr_built,
                   data=train_data,
                   method  = "rf",
                   ntree=1000,
                   tuneGrid=expand.grid(mtry=c(2,4,6,8,10)),
                   importance=TRUE)
library(doParallel)
model_rf2
stopCluster(cl)





