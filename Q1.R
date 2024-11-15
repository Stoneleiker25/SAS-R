# Load libraries
library(ggplot2)

# Load the data
ss_data <- read.csv("C:/Users/leiker-s/Desktop/ss_data.csv")

# Drop Firm_id
ss_data <- ss_data[, -which(names(ss_data) == "Firm_id")]

# Summary stats and dimensions
summary(ss_data)
dim(ss_data)

# Plot revenue against other variables
ggplot(ss_data, aes(x = sponsored, y = revenue)) + geom_point()
ggplot(ss_data, aes(x = organic, y = revenue)) + geom_point()
ggplot(ss_data, aes(x = social, y = revenue)) + geom_point()

# Load psych library for correlation and p-values
library(psych)

# Correlation between revenue and other variables
corr <- corr.test(ss_data$revenue, ss_data[, c("sponsored", "organic", "social")])
corr

# Load caret library for data splitting
library(caret)

# Split data 70:30 into training and test sets
set.seed(123)
trainIndex <- createDataPartition(ss_data$revenue, p = 0.7, list = FALSE)
trainData <- ss_data[trainIndex, ]
testData <- ss_data[-trainIndex, ]

# Fit a regression model
model <- lm(revenue ~ sponsored + organic + social, data = trainData)
summary(model)

# Predict on test data and calculate RMSE
predictions <- predict(model, newdata = testData)
rmse_value <- rmse(testData$revenue, predictions)
rmse_value

# Load the data
marketing_spend <- read.csv("C:/Users/leiker-s/Desktop/Marketing_spend.csv")

# Summary and model for sales ~ spend + month
summary(marketing_spend)

# Fit a regression model
model2 <- lm(sales ~ spend + month, data = marketing_spend)
summary(model2)
