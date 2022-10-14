# Header ------------------------------------------------------------------
# Assignment name: Lab 11 Key
# Author name: Morgan Sparks
# Date: 10/13/2022
# Notes:

# File setup --------------------------------------------------------------

# set working directory below

# load libraries below
library(tidyverse)


# Exercise 1 --------------------------------------------------------------

### Question 1
pop_size <- read_tsv("~/Downloads/lab11/FauchaldEtAl2017/pop_size.csv")

pop_size %>%
    select(Year) %>%
    distinct() %>%
    arrange()

### Question 2

ndvi <- read_tsv("~/Downloads/lab11/FauchaldEtAl2017/ndvi.csv")

# part 1
ndvi %>%
arrange(desc(NDVI_May))

# Exercise Y --------------------------------------------------------------

### Question 1

### Question 2
