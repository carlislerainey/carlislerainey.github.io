##############################################################################
# code to evaluate the models, don't change anything below, just look at the 
# output.
# evaluate model fit by predicting years, one-by-one, out-of-sample
evaluate_models <- function(..., data, group, model_names) {
  require(dplyr, warn.conflicts = FALSE, quietly = TRUE)
  formulas <- list(...)
  # in-sample rmse
  eval1 <- NULL
  for (i in 1:length(formulas)) {
    f <- formulas[[i]]
    fit0 <- lm(f, data = data)
    df0 <- data.frame(model = model_names[i], 
                      BIC = BIC(fit0),
                      rms_error = sqrt(mean(residuals(fit0)^2)))
    eval1 <- rbind(eval1, df0)
  }
  # leave-groups-out rmse
  groups <- sort(unique(as.matrix(data[, group])))
  oos_df <- df0 <- NULL
  for (i in 1:length(formulas)) {
    f <- formulas[[i]]
    df0 <- NULL
    for (j in 1:length(groups)) {
      train00 <- filter(data, data[, group] != groups[j])
      pred00 <- filter(data, data[, group] == groups[j])
      fit00 <- lm(f, data = train00)
      df00 <- data.frame(model = model_names[i], 
                         group = groups[j], 
                         oos_predicted = predict(fit00, newdata = pred00),
                         actual = pred00$inc1)
      df0 <- rbind(df0, df00)
    }
    oos_df <- rbind(oos_df, df0)
  }
  eval2 <- summarize(group_by(oos_df, model), 
                     oos_rms_error = sqrt(mean((oos_predicted - actual)^2)))
  eval <- left_join(eval1, eval2)
  return(eval)
}

compute_measures_of_fit <- function() {
  model_names <- c("Too Simple", 
                   "Just Right",
                   "Too Complicated")
  ev <- evaluate_models(m1, m2, m3,
                        data = train, 
                        group = "Year",
                        model_names = model_names)
  ev_tall <- gather(ev, measure_of_fit, fit, BIC:oos_rms_error) 
  ev_tall <- mutate(ev_tall, measure_of_fit = factor(measure_of_fit, 
                                                     levels = c("rms_error", 
                                                                "BIC",
                                                                "oos_rms_error")))
  ev_tall <- mutate(ev_tall, model = factor(model, c("Too Simple", 
                                                     "Just Right",
                                                     "Too Complicated")))
  gg <- ggplot(subset(ev_tall, measure_of_fit != "oos_rms_error"), aes(x = model, y = fit)) + 
    geom_col() + 
    facet_wrap(~ measure_of_fit, scale = "free_y")
  print(select(ev, -oos_rms_error), digits = 3)
  print(gg)
}

##############################################################################
# code to create the data-frame to submit
# note: I recommend NOT changing this block
write_predictions_to_file <- function() {
  model_names <- c("Too Simple", 
                   "Just Right",
                   "Too Complicated")
  name_stub <- str_to_lower(my_name) %>%
    str_replace(" ", "-") %>%
    str_replace("\\.", "")
  where_to_save_predictions <- paste0("data/",
                                      "election-predictions-", 
                                      name_stub, 
                                      ".csv")
  fits <- list(m1, m2, m3)
  submit_df <- NULL
  for (i in 1:length(fits)) {
    df0 <- select(pred, Year)
    df0$modeler <- my_name
    df0$model_name <- model_names[i]
    df0$prediction <- predict(fits[[i]], newdata = pred)
    df0 <- select(df0, modeler, model_name, Year, prediction)
    submit_df <- rbind(submit_df, df0)
  }
  write_csv(submit_df, where_to_save_predictions)
}

