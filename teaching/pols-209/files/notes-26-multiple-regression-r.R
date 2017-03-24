
# delete all objects from workspace
rm(list = ls())

# set working directory
setwd("/Users/carlislerainey/Dropbox/classes/pols-209")  # <--- CHANGE THIS

# load packages

# load training and prediction sets
training_data <- readRDS("data/taxes-training.rds")
prediction_data <- readRDS("data/taxes-prediction.rds")


# model with only an intercept
fit1 <- lm(tax_change ~ 1, data = training_data)
coef(fit1)

# model with a single predictor
fit2 <- lm(tax_change ~ gov_request, data = training_data)
coef(fit2)

# model with multiple predictors
fit3 <- lm(tax_change ~ gov_request + estimated_imbalance, data = training_data)
coef(fit3)

# model with a qualitative predictor
fit4 <- lm(tax_change ~ gov_party, data = training_data)
coef(fit4)

# model with a 3rd-degree polynomial
fit5 <- lm(tax_change ~ poly(estimated_imbalance, degree = 3), data = training_data)
coef(fit5)

# model with an interaction
fit6 <- lm(tax_change ~ gov_request*estimated_imbalance, data = training_data)
coef(fit6)

# use BIC to evaluate model fit
BIC(fit1, fit2, fit3, fit4, fit5, fit6)

