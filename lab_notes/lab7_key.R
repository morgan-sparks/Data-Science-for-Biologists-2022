# Header ------------------------------------------------------------------
# Assignment name: :Lab 7 Key
# Author name:
# Date:
# Notes:
# 1. Difference between naming and calling functions
# 2. some of you aren't understanding functions you don't have to provide objects for
# 3. Definitely do not initialize matrix data using loops. It makes R very slow.
# 4. View() can be a very useful function, but head() and tail() used in the consul are
#    often better options

# File setup --------------------------------------------------------------

# set working directory below

# load libraries below

# Exercise III --------------------------------------------------------------

### Question 1
tell_fortune <- function() { # notice there is no object interpreted in this function
  # you can use runif(1) 
  if (runif(1)> 0.3) { #runif(1) hardcodes the random draw inside the function
    print("Yes")
  } else {
    print("No")
  }
}

tell_fortune() # as such, you just have to run it as so, 
              # because you generate the random number inside the function

### Question 2

order_three <- function(x, y, z){
  return(sort( c(x,y,z))) #note you need to make a vector here inside the sort function
}

order_three(x = 5, y = 7,z = 3)

### Question 3
order_three <- function(x, y, z){
  my_ord <- sort(c(x,y,z))
  return(list("first" = my_ord[1], # notice how you call each element of the vector for the named list items
              "second" = my_ord[2],
              "third" = my_ord[3]
  ))
}

order_three(x = 5, y = 7,z = 3) # returns the list with names for each element

### Question 4

data(trees)

rand_nums <- sample(c(1,2), #vector or 1 or 2 to sample from
                    size = nrow(trees), # sample the number times there are rows in trees
                    replace = TRUE #sample with replacement
) 

trees <- cbind(trees, rand_nums)

### Question 5
data(trees) # reload trees so you have a new column in the dataframe

pvals <- NULL # empty object to store pvals

for (i in 1:100){
# make your vector of random 1s and 2s
rand_nums <- sample(c(1,2), #vector or 1 or 2 to sample from
                    size = nrow(trees), # sample the number times there are rows in trees
                    replace = TRUE #sample with replacement
) 

# append to trees (different way than shown in previous question)
trees[,"rand_nums"] <- rand_nums # there are only 4 columns in the original tress so this makes new one and rewrites it for every i

trees_one <- trees[trees$rand_nums == 1,] # subset df when new column = 1
trees_two <- trees[trees$rand_nums == 2,] # subset df when new column = 2

pval <- t.test(trees_one$Volume, trees_two$Volume)

pvals[i] <- pval$p.value #pval has many substructure elements, you can specifically extract the pval (see them with str(pval))

}

#number of pvals elements less than or equal to 0.05 divided by total number of pvals
length(pvals[pvals >= 0.05])/length(pvals)

# Exercise IV --------------------------------------------------------------

### Question 1

# initialize matrix that is 10K rows by 10K columns
M <- matrix(rnorm(10000*10000, mean = 1:10000),  #rnorm repeats a value with mean 1:10000 for each row
            nrow = 10000, 
            ncol = 10000,
            byrow=FALSE) #inputs values in according to column


### Question 2

# calculate medians as a for loop
medians_loop <- matrix(NA, nrow = nrow(M),ncol = 2) #initialize matrix to store medians

colnames(medians_loop) <- c("row_med", "col_med") # give it column names

for (n in 1:nrow(M)){ # for n in number of rows of M matrix (this works because nrows==ncols)
medians_loop[n, "row_med"] <- median(M[n,]) # take the median of the nth row  and put in the row_med column for n position
medians_loop[n, "col_med"] <- median(M[,n]) # do the same for column
}
   
# calculate medians using apply

medians_apply <- matrix(NA, nrow = nrow(M),ncol = 2)

colnames(medians_apply) <- c("row_med", "col_med")

medians_apply[, "col_med"] <- apply(M, MARGIN = 2, FUN = median) # apply median function across columns
medians_apply[, "row_med"] <- apply(M, MARGIN = 1, FUN = median) # apply median function across rows

#### check if each method is the same
medians_loop == medians_apply