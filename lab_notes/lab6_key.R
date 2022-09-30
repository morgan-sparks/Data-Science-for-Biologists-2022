# Header ------------------------------------------------------------------
# Assignment name: Lab 6
# Author name:
# Date:
# Notes:

# 1. some of you are using paste() incorrectly
# 2. some of your answers are very similar, I understand you may work together in class
#    but make sure you are providing different answers.
# 3. Make sure your loops and logical statements open their brackets on the same line as statement
#     e.g., if(condition){}


# File setup --------------------------------------------------------------

# set working directory below

# load libraries below

# Exercise 1 --------------------------------------------------------------
z <- readline(prompt = "Enter a number: ")
z <- as.numeric(z)

### Question 1

if (z > 100) {
  print(z ^ 3)
} else {
  print(paste(z, "is less than 100"))
}

### Question 2

if (z %% 17 == 0) {
  print(sqrt(z))
} else{
  print(paste(z, "is not divisible by 17"))
}

### Question 3

z <- readline(prompt = "Enter a positive integer: ")
z <- as.numeric(z)
  if (z < 100) {
  print(seq(1, z))
  }else{
  print(seq(z, 1))
  }


# Exercise 2 --------------------------------------------------------------

### Question 1

z <- seq(1, 1000, by = 3) # create a vector of 1 to 3000 using every third number
for (k in z) { # for each element k in the vector z...
  if (k %% 4 == 0) { # if element k is divisible by 4...
    print(k) # print that element k
  }
}

### Question 2

z <- readline(prompt = "Enter a number: ") # make prompt for z
z <- as.numeric(z) # make sure z is of class numeric
isthisspecial <- TRUE # set object isthisspecial to TRUE
i <- 2 # set object i to 2
while (i < z) { # while i is less than z
  if (z %% i == 0) { # if element z is divisible by i...
    isthisspecial <- FALSE # change the object isthispsecial to FALSE
    break # and break the while loop
  }
  i <- i + 1 # add one to element i
}
if (isthisspecial == TRUE) {
  print(z) # if the while did not break print the number (aka this number is not divisible by any numbers smaller than it)
}

# this loop is essentially a test for prime numbers