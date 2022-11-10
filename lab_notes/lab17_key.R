# Header ------------------------------------------------------------------
# Assignment name: Lab 17 Key
# Author name: Morgan Sparks
# Date: 11/08/22
# Notes:
# 1. For the word length matches, using ... won't just return letters.
# 2.

# File setup --------------------------------------------------------------

# set working directory below

# load libraries below
library(stringr)

# Exercise 1--------------------------------------------------------------

### Question 1

# collapse will take multiple strings into one and delimit them with the object provided,
# whereas sep will maintain the number of strings and will separate those strings with the
# provided delimiter

str_c(c("x", "y", "z"), collapse = ", ")
str_c(c("x", "y", "z"), sep = ", ")

### Question 2

str_view_all(words, "^y", match = TRUE)
str_view_all(words, "x$", match = TRUE)
str_view_all(words, "^[a-zA-Z][a-zA-Z][a-zA-Z]$", match = TRUE)
str_view_all(words, "[a-zA-Z]{7}+", match = TRUE)

### Question 3

str_view_all(words, "^[^aeiou]{3}", match = TRUE)
str_view_all(words, "[aeiou]{3}", match = TRUE)
str_view_all(words, "([aeiou][^aeiou]){2,}", match = TRUE)

