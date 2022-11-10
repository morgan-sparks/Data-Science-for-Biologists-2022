# Header ------------------------------------------------------------------
# Assignment name:
# Author name:
# Date:
# Notes:

# File setup --------------------------------------------------------------

# set working directory below

# load libraries below

library(tidyverse); library(lubridate); library(nycflights13)

# Exercise II Ch9 --------------------------------------------------------------

beelist <- read_tsv("~/Downloads/lab17/bee_list.txt")

separators<-"[&,]"
beelist <- beelist %>% mutate(authorNum = str_count(`Taxon Author`, separators))

nameYearRegEx <- "\\(?([^0-9]+)(\\d+)\\)?"

matchedResults<- str_match(beelist$`Taxon Author`, nameYearRegEx)

### Question 1

names <- unlist(str_split(matchedResults[,2], pattern = "[&,]"))

names_good<- names %>%
  str_replace(pattern = " ", replacement = "")

names_good <-data.frame(names_good)

names_good %>% 
  group_by(names_good) %>% 
  tally() %>% 
  arrange(desc(n))

# Cockerell has 3394 instances

### Question 2

years <- unlist(str_split(matchedResults[,3], pattern = "[&,]"))
data.frame(years) %>% 
  group_by(years) %>% 
  tally() %>% 
  arrange(desc(n))

# 1903 has 406 instances


# Exercise I Ch10 --------------------------------------------------------------

### Question 1
# set objects

d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014

#change objects (not setting them as objects so you can easily see their results)

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)


### Question 2

#string of first days of each month in 2015 and 2022
yr_2015 <- seq(mdy("1-1-2015"), mdy("12-1-2015"), by = "months")

yr_2022 <- seq(mdy("1-1-2022"), mdy("12-1-2022"), by = "months")

### Question 3

new_delays <- flights %>% 
  mutate(dept_delay = dep_time - sched_dep_time) %>% # create your new metric
  mutate(delay_diff = dep_delay - dept_delay) # subtract your metric vs the old one to find differencces

# plot delay diff

ggplot(data = new_delays) +
  geom_histogram(aes(x = delay_diff)) +
  theme_classic()

summary(new_delays$delay_diff) # mostly 0s
# looks like some really big differences but most stack around zero

### Question 4

wday_delays <- new_delays %>% 
  mutate(day.of.week = wday(time_hour)) %>% 
  group_by(day.of.week) %>% 
  summarise(mean.delay = mean(dep_delay, na.rm =TRUE))

ggplot(data = wday_delays) +
  geom_line(aes(x = day.of.week, y = mean.delay)) +
  lims(y = c(0,20)) +
  theme_classic()

# I would leave on Saturday, but the difference in negligible

  
  


