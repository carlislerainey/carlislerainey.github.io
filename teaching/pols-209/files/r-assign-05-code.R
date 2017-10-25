
# don't forget to set your working directory as part of your code **CHANGE**
setwd("~/Dropbox/classes/pols-209")

# clear workspace
rm(list = ls())

# load packages
library(tidyverse)
library(stringr)

# your name  **CHANGE**
my_name <- "Carlisle R."

# load training and prediction sets from the web
train <- read_csv(url("http://www.carlislerainey.com/teaching/pols-209/files/r-assign-05-train.csv"))
pred <- read_csv(url("http://www.carlislerainey.com/teaching/pols-209/files/r-assign-05-pred.csv"))

# fit models  **CHANGE**
## note: the formulas below are simply placeholders, change all of them
## - make model 1 too simple (~1 expl. var.)
## - make model 2 about right (~3 expl. var.)
## - make model 3 much too complicated (~8 expl. var.)
m1 <- lm(inc1 ~ G, data = train)  # too simple
m2 <- lm(inc1 ~ G + P, data = train)  # just right
m3 <- lm(inc1 ~ G + P + Z, data = train)  # too complicated

# I wrote some custom code that does two things:
# 1. prints and plots the rms error, BIC, and oos rms error for each model
# 2. saves a .csv file of each models predictions to your data folder.
source(url("http://www.carlislerainey.com/teaching/pols-209/files/r-assign-05-fns.R"))
compute_measures_of_fit()
write_predictions_to_file()
