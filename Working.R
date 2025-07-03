
HousePrice <- read.csv("data.csv")
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
HousePrice %>%
  group_by(view) %>%
  summarise(median_price = median(price))
HousePrice %>%
  group_by(condition) %>%
  summarise(median_price=median(price))

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
       y = "Value") +
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
mean(HousePrice$price)>median(HousePrice$price)
#that mean's right skewed
HousePrice$view <- factor(HousePrice$view, ordered = TRUE)
HousePrice$condition <- factor(HousePrice$condition, ordered = TRUE)
HousePrice$waterfront <- factor(HousePrice$waterfront, ordered = TRUE)


cor(HousePrice[,sapply(HousePrice,is.numeric)])
sapply(HousePrice, function(x) length(unique(x)))
nearZeroVar(HousePrice, saveMetrics = TRUE)
sapply(HousePrice,n_distinct)


install.packages("caret")
library(caret)
HousePrice$price_bins <- cut(HousePrice$price,
                             breaks=quantile(HousePrice$price, probs = seq(0,1,0.1)),
                             include.lowest=TRUE)
set.seed(200)
split_index <- createDataPartition(HousePrice$price_bins,p=0.7,list = FALSE)
train_data <- HousePrice[split_index,]
test_data <- HousePrice[-split_index,]

summary(train_data)
summary(test_data)

model_lm <- lm(price~.,data=train_data)









