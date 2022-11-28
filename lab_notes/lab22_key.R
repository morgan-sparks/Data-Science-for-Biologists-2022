# Header ------------------------------------------------------------------
# Assignment name: lab 22 key
# Author name: Morgan Sparks
# Date: 11/25/2022
# Notes:
#1. explain models and reiduals

# File setup --------------------------------------------------------------

# set working directory below

# load libraries below
library(tidyverse); library(modelr)

# Exercise I --------------------------------------------------------------

### Question 1

# load in data and check it out
data(sim1)
sim1

# build model and see structure

sim1_mod <- loess(y~x, data = sim1)
sim1_mod

# build grid using predictions of x and then look at grid

grid <- sim1 %>% 
  data_grid(x) %>% 
  add_predictions(sim1_mod)

grid

# plot data and predictions from model

ggplot(sim1) +
  geom_point(aes(x = x, y =y)) + # raw data
  geom_line(data = grid, aes(x =x ,y = pred), color = "red") + # predictionns from loess model
  theme_classic()


ggplot(sim1) +
  geom_point(aes(x = x, y =y)) + # raw data
  geom_line(data = grid, aes(x =x ,y = pred), color = "red", size = 2) + # predictionns from loess model
  geom_smooth(aes(x = x, y =y), se = FALSE) + # line plots right on top of the model line
  theme_classic()


### Question 2

# add residuals and plot

sim1 <- sim1 %>% add_residuals(sim1_mod)
sim1

ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0,col = "blue") +
  geom_point() 

# geom_ref_line adds a horizontal line at 0 to assess your residuals, the residuals allow you to assess how well
# your model is preforming, e.g., is it doing well to minimize the differentiaion between your data and prediction.

