

# delete all objects from workspace
rm(list = ls())

# set working directory
setwd("/Users/carlislerainey/Dropbox/classes/pols-209")  # <--- CHANGE THIS

# load packages
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)

# load training and prediction sets
training_data <- readRDS("data/taxes-training.rds")
prediction_data <- readRDS("data/taxes-prediction.rds")

# model 1
fit1 <- lm(tax_change ~ gov_request, data = training_data)

# model 2
fit2 <- lm(tax_change ~ gov_request + estimated_imbalance, data = training_data)

# model 3
fit3 <- lm(tax_change ~ gov_request + poly(estimated_imbalance, 3), data = training_data)

# model 4 ('.' is a quick way to include all other variables in the df on the righthand side of the formula)
fit4 <- lm(tax_change ~ ., data = training_data)

# store models in a list and give them names
fits <- list(fit1, fit2, fit3, fit4)  # <- if you want more than four models, add them here
model_names <- c( "One-Variable Model", 
                 "Two-Variable Model",
                 "Polynomial Model",
                 "Kitchen Sink")

###########################################
## Ignore the code below this point. ######
## it just calculates the fit statistics ##
## for each model and creates the plot ####
###########################################

# build a data frame of BICs
bic_df <- NULL
for (i in 1:length(fits)) {
  df0 <- data.frame(model = model_names[i], bic = BIC(fits[[i]]))
  bic_df <- rbind(bic_df, df0)
}
bic_df <- mutate(bic_df, model = reorder(model, bic))


# build a data frame of RMSEs
rmse_df <- NULL
for (i in 1:length(fits)) {
  e <- residuals(fits[[i]])
  df0 <- data.frame(model = model_names[i], rmse = sqrt(mean(e^2)))
  rmse_df <- rbind(rmse_df, df0)
}
rmse_df <- mutate(rmse_df, model = reorder(model, rmse))


# build a data frame of out-of-sample predictions
years <- sort(unique(training_data$year))
oos_df <- df0 <- NULL
for (i in 1:length(fits)) {
  f <- fits[[i]]$call[[2]]
  for (j in 1:length(years)) {
    train00 <- filter(training_data, year != years[j])
    pred00 <- filter(training_data, year == years[j])
    fit00 <- lm(f, data = train00)
    df00 <- data.frame(model = model_names[i], 
                       year = years[j], 
                      oos_predicted = predict(fit00, newdata = pred00),
                      actual = pred00$tax_change)
    df0 <- rbind(df0, df00)
    }
  oos_df <- rbind(oos_df, df0)
}
oos_df <- mutate(oos_df, model = reorder(model, oos_predicted - actual, sd))

# a single plot to summarize all metrics
oos_df_agg <- summarize(group_by(oos_df, model), oos_rmse = sqrt(mean((oos_predicted - actual)^2)))
comb_df <- left_join(bic_df, rmse_df) %>%
  left_join(oos_df_agg)
tall_comb_df <- gather(comb_df, metric, value, bic:oos_rmse)
tall_comb_df$metric <- ifelse(tall_comb_df$metric == "bic", "BIC", 
                              ifelse(tall_comb_df$metric == "rmse", "In-Sample R.M.S. Error", "Out-of-Sample R.M.S. Error"))
tall_comb_df$model <- factor(tall_comb_df$model, levels = levels(oos_df$model))
ggplot(tall_comb_df, aes(x = value, y = model)) + 
  facet_wrap(~ metric, scales = "free") + 
  geom_point() + 
  theme_bw() + 
  labs(title = "Which Model Fits Best?",
       subtitle = "Three Measures to Help Us Decide... But Don't Forget About Theory!",
       x = "Value", 
       y = "Model", 
       caption = "This figure plots three fit statistics for each model. In each case, small values indicate a better fit.") 
