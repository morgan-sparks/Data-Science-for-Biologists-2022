# Header ------------------------------------------------------------------
# Assignment name: lab 15 key
# Author name: Morgan Sparks
# Date: 11/1/22
# Notes:
# 1. A number of folks were using geom_col() instead of geom_hist()
# File setup --------------------------------------------------------------

# set working directory below
setwd("~/Downloads/lab11/FauchaldEtAl2017/")

# load libraries below
library(tidyverse)

# Exercise II --------------------------------------------------------------

### Question 1
snow <- read_tsv("./snow.csv")

ggplot() +
  geom_histogram(data = snow, aes(x = Week_snowmelt)) +
  facet_wrap(~Herd) +
  theme_classic()

### Question 2

ggplot(data = snow, aes(x = Week_snowmelt, y = Perc_snowcover)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Herd) +
  theme_classic()

### Question 3

ggplot() +
  geom_tile(data = snow, aes(x = Year, y = Herd, fill = Week_snowmelt)) +
  theme_classic()

# Exercise III --------------------------------------------------------------
setwd("~/Downloads/lab15/")

### Question 1
bolstad <- read_csv("Bolstad2015_data.csv")

# make a summarized version of the data

bolstad_sum <- bolstad %>% 
  group_by(Species, Sex) %>% 
  summarise(avg.WS = mean(WingSize),
            avg.L2L = mean(L2Length))

# you can plot multiple data types in a ggplot, I will use the sum data for geom_point and the full data for geom_smooth
# notice how the order here matters (points are laid on top of lines when it comes after in order)

ggplot() +
  geom_smooth(data = bolstad, aes(x = WingSize, y = L2Length, group = Species), method = "lm", se = FALSE, size = .75) +
  geom_point(data = bolstad_sum, aes(x = avg.WS, y = avg.L2L)) +
  facet_wrap(~Sex) +
  labs(x = "Wing size (In mm)", y =  "Vein L2 lenght (In mm)") +
  theme_bw()

### Question 2

urban <- read_tsv("Urban2015_data.csv")

## notice how I make a new column called cut with the breaks I want and then group and tally on that column
urban_tally <- urban %>%  
  group_by(Author, Year) %>% 
  summarise(avg.Percent =  mean(Percent)) %>% 
  mutate(cut = cut(avg.Percent, breaks = c( -.1, 0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 1)
                                           , labels= c("0-0", "0-0.05", "0.05-0.1", "0.1-0.15", "0.15-0.2", "0.2-0.25",
                                                       "0.25-0.3", "0.3-0.35", "0.35-0.4", "0.4-0.45", "0.45-0.5", "0.5-1"))) %>% 
  group_by(cut) %>% 
  tally() 

### this is very close, but maybe not exact, hard to know without seeing author's exact binning
ggplot() +
  geom_col(data = urban_tally, aes(x= cut, y = n)) +
  labs(x = "Proportion species at risk", y = "Number of studies") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # this rotates axis labels

# you could also make the same plot by using geom_hist with the breaks I used above and then providing 
# those breaks a custom set of names, but I fiigured that would be more annoying that this way, so I tried
# what I figured was the easier option
