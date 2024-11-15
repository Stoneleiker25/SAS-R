# Load the data
marketing_spend <- read.csv("C:/Users/leiker-s/Desktop/Marketing_spend.csv")

# Summary and model for sales ~ spend + month
summary(marketing_spend)

# Fit a regression model
model2 <- lm(sales ~ spend + month, data = marketing_spend)
summary(model2)

# Add quadratic term for spend to check for nonlinearity
marketing_spend$spend_sq <- marketing_spend$spend^2

# Fit a model with spend squared
model3 <- lm(sales ~ spend + spend_sq + month, data = marketing_spend)
summary(model3)