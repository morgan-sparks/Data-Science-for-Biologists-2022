# Header ------------------------------------------------------------------
# Assignment name: Lab 13 Key
# Author name: Morgan Sparks
# Date: 10/20/2022
# Notes:
# 1. In R, you can call objects by just running the object rather than print() or show()

# File setup --------------------------------------------------------------

# set working directory below
setwd("~/Downloads/lab11/FauchaldEtAl2017")

# load libraries below
library(tidyverse)

# Exercise 1 --------------------------------------------------------------

### Question 1
snow <- read_tsv("./snow.csv")

# this is enough
ggplot(data = snow) +
    geom_point(aes(x = Perc_snowcover, y = Week_snowmelt))

#but this separates the data more (it also might make sense to break for herd)
ggplot(data = snow) +
    geom_point(aes(x = Perc_snowcover, y = Week_snowmelt, color = Year))


### Question 2

avg_snow <- snow %>%
                group_by(Year)  %>%
                summarise(avg_perc_snow = mean(Perc_snowcover),
                          avg_wk_snowmelt = mean(Week_snowmelt))

ggplot(data = avg_snow) +
    geom_point(aes(x= avg_perc_snow, y = avg_wk_snowmelt))

# you could also just pipe the data into ggplot without creating an object
snow %>%
    group_by(Year)  %>%
    summarise(avg_perc_snow = mean(Perc_snowcover),
              avg_wk_snowmelt = mean(Week_snowmelt)) %>%
    ggplot() +
    geom_point(aes(x= avg_perc_snow, y = avg_wk_snowmelt))

### Question 3
pop_size <- read_tsv("./pop_size.csv")

ggplot(data = pop_size) +
    geom_boxplot(aes(x = Herd, y = Pop_Size))

### Question 4

pop_size_sum <- pop_size  %>%
                    filter(Year >= 2008, Year <= 2014) %>%
                    group_by(Year) %>%
                    summarise(avg_size =  mean(Pop_Size),
                    sd_size = sd(Pop_Size))

ggplot(data = pop_size_sum) +
    geom_point(aes(x = Year, y = avg_size)) 

# we can also add error_bars with our SD easily and clean it up (not part of assingment)

ggplot(data = pop_size_sum) +
    geom_point(aes(x = Year, y = avg_size)) +
    geom_errorbar(aes( x = Year, ymin= avg_size-sd_size, ymax = avg_size+sd_size )) +
    labs(x = "Year", y= "Average population size") +
    theme_classic(base_size = 14)

