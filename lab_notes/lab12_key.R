# Header ------------------------------------------------------------------
# Assignment name: Lab 12 Key
# Author name: Morgan Sparks
# Date: 10/20/2022
# Notes:

# File setup --------------------------------------------------------------

# set working directory below
setwd("~/Downloads/lab11/FauchaldEtAl2017")

# load libraries below
library(tidyverse)

# Exercise III --------------------------------------------------------------

### Question 1

sea_ice <-  read_tsv("./sea_ice.csv")

sea_ice_long <- sea_ice %>%
                    pivot_longer(cols = 3:14, names_to = "Month", values_to = "ice_cover")

# check to see if it looks correct (looks good!)
head(sea_ice_long)

### Question 2
table2 %>% 
    pivot_wider(names_from = "type", values_from = "count")

table4a %>%
    pivot_longer(cols = 2:3, names_to = "Year", values_to = "value")

### Question 3
pop_size <- read_tsv("./pop_size.csv")

# get summary from long version of sea ice
avg_sea_ice <- sea_ice_long %>%
    group_by(Herd, Year) %>%
    summarise(avg_sea_ice = mean(ice_cover))

# join pop size with avg_sea_ice (notice NAs get intoduced)

avg_Perc_seaicecover <- avg_sea_ice %>%
                            left_join(pop_size)

head(avg_Perc_seaicecover)

### Question 4
gberg_dat <- read_csv("~/Downloads/lab8/Q1/Goldberg2010_data.csv")

gberg_dat %>%
    separate( col ="Species", into = c("Genus", "species_epithet"), sep = "_")  %>%
    group_by(Genus, Status)  %>%
    summarise(count = n())

### Question 5

### look at join documentation
help(join)

# see section about filter joins

# See Also
# Other joins: filter-joins, nest_join()
# Description
# Filtering joins filter rows from x based on the presence or absence of matches in y:

# semi_join() return all rows from x with a match in y.

# anti_join() return all rows from x without a match in y.

# anti_join() is what we want (look, output is 17 rows)

pop_size %>% 
    anti_join(avg_Perc_seaicecover)

### Question 6
march_sea_ice <- sea_ice_long %>%
                    filter(Month == "Mar")

snow <- read_tsv("./snow.csv")

sea_ice_and_snow <- march_sea_ice %>%
                        inner_join(snow)

# compute kendall's rank correlation
sea_ice_and_snow %>%
    group_by(Herd) %>%
    summarise(ice_snow_cor = cor(ice_cover, Week_snowmelt, method = "kendall"))
