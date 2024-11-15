# Load necessary libraries
if(!require(heplots)) install.packages("heplots")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(caret)) install.packages("caret")
if(!require(dplyr)) install.packages("dplyr")

library(heplots)
library(ggplot2)
library(caret)
library(dplyr)

## Q1

# Load and clean data
ss_data <- read.csv("C:/Users/leiker-s/Desktop/ss_data.csv")
ss_data <- ss_data %>% select(-Firm_id)
summary(ss_data)
dim(ss_data)

# Q1a
ggplot(ss_data, aes(x = sponsored, y = revenue)) + geom_point() + ggtitle("Revenue vs Sponsored")
ggplot(ss_data, aes(x = organic, y = revenue)) + geom_point() + ggtitle("Revenue vs Organic")
ggplot(ss_data, aes(x = social, y = revenue)) + geom_point() + ggtitle("Revenue vs Social")

# Q1b
cor_matrix <- cor(ss_data)
print(cor_matrix)
cor_test <- cor.test(ss_data$revenue, ss_data$sponsored)
print(cor_test)

# Q1c
set.seed(123)
trainIndex <- createDataPartition(ss_data$revenue, p = 0.7, list = FALSE)
train_data <- ss_data[trainIndex, ]
test_data <- ss_data[-trainIndex, ]

model <- lm(revenue ~ sponsored + organic + social, data = train_data)
summary(model)

# Q1d
predictions <- predict(model, test_data)
rmse <- sqrt(mean((predictions - test_data$revenue)^2))
print(paste("RMSE:", rmse))


## Q2

# Load data
marketing_spend <- read.csv("C:/Users/leiker-s/Desktop/Marketing_spend.csv")

# Q2a
model_q2a <- lm(sales ~ spend + Month, data = marketing_spend)
summary(model_q2a)

# Q2b
marketing_spend$spend_sq <- marketing_spend$spend^2
model_q2b <- lm(sales ~ spend + spend_sq + Month, data = marketing_spend)
summary(model_q2b)

# Q2c
marketing_spend$interaction <- marketing_spend$social_media * marketing_spend$digital_ads
model_q2c <- lm(sales ~ spend + social_media + digital_ads + interaction, data = marketing_spend)
summary(model_q2c)


## Q3

# Load data
health_data <- read.csv("C:/Users/leiker-s/Desktop/Health Indicator.csv")

# Q3a
pca <- prcomp(health_data[, sapply(health_data, is.numeric)], scale. = TRUE)
summary(pca)
plot(pca)
biplot(pca)


## Q4

# Load RootStock data
data(RootStock)
rootstock_data <- RootStock

# Q4a
manova_q4a <- manova(cbind(girth4, ext4, weight15) ~ rootstock, data = rootstock_data)
summary(manova_q4a, test = "Pillai")

# Q4b
manova_q4b <- manova(cbind(girth4, ext4, girth15, weight15) ~ rootstock, data = rootstock_data)
summary(manova_q4b, test = "Pillai")

# Q4c
anova_q4c <- aov(weight15 ~ rootstock, data = rootstock_data)
summary(anova_q4c)
TukeyHSD(anova_q4c)


## Q5
# PCA 1
cat("1: PCA example - for reducing variables in health metrics.\n")
# Linear Regression 2
cat("2: Linear regression example - for predicting revenue based on advertising and traffic data.\n")

