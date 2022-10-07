# Header ------------------------------------------------------------------
# Assignment name: Lab 8 Key
# Author name: Morgan Sparks
# Date:
# Notes:
# Can't change rank and then rewrite over that column (breaks data connections)

# File setup --------------------------------------------------------------

# set working directory below

# load libraries below
library(tidyverse); library(EBImage)


# Exercise 5 --------------------------------------------------------------

### Question 5.12.1 Self-Incompatibility in Plants

# 1.
gberg_dat <- read_csv("~/Downloads/lab8/Q1/Goldberg2010_data.csv")

gberg_df <- data.frame(table(gberg_dat$Status))

# 2.
gberg_dat[c("Genus", "newSpecies")] <- str_split_fixed(gberg_dat$Species, 
                                                            pattern = "_",
                                                            n =2)

# new df with Genus and Status counts
sum_table <- data.frame(table(gberg_dat$Genus, gberg_dat$Status))

# give it more meaninful names
colnames(sum_table) <- c("Genus", "Status", "count")


### Question 5.12.2  Body Mass fo Mammals

# 1.
source("~/Downloads/lab8/Q2/read_mass.R")

### read_mass.R looks like

# smith_dat <- read.table("~/Downloads/lab8/Q2/Smith2003_data.txt", sep = "\t", header = FALSE)

# smith_dat <- data.frame(smith_dat)
# colnames(smith_dat) <- c("Continent", "Status", "Order", "Family", "Genus", "Species", "Log_Mass_g", "Combined_Mass_g", "Reference")
# smith_dat[smith_dat== -999 ] <- NA



## I don't remove the NAs because sometimes they mean something in your data, but you can remove them

# 2. 
## way 1 Make a function that runs a loop
way1 <- function(){
    families <- unique(smith_dat$Family) # character vector of unique families
    fam_mean<- data.frame(matrix(NA, 1, 2)) # set up dataframe to store data
    colnames(fam_mean) <- c("Family", "Mean_mass") # name the columns
    i <- 1 # initiate counter object
    for (f in families){ # for f element in character vectore families
        fam_mean[i,"Family"] <- f # store what family you are subsetting in our df
        fam_mean[i, "Mean_mass"] <- mean(smith_dat[smith_dat$Family == f,"Combined_Mass_g"]) # store the mean 
        i <- i + 1 # add to counter
    }
    return(fam_mean)
}
way1() # run the function (made it a function so it's easy to call system.time())

## way 2 make a function that subsets and use sapply
fam_mean_fun <- function(x){
    fam_mean2 <- mean(smith_dat[smith_dat$Family == x, "Combined_Mass_g"])
    return(fam_mean2)
}
sapply(unique(smith_dat$Family), FUN = fam_mean_fun)

## compare times

system.time(way1())
# user  system elapsed 
#   0.020   0.001   0.034 

system.time(sapply(unique(smith_dat$Family), FUN = fam_mean_fun))
#  user  system elapsed 
#   0.010   0.001   0.010 

# Second way is faster because it vectorizes the function vs. a loop.

### another way of doing it with  tidyverse
head(smith_dat)
smith_dat %>%
    group_by(Family) %>%
    summarise(mean(Combined_Mass_g))

system.time(smith_dat %>%
    group_by(Family) %>%
    summarise(mean(Combined_Mass_g)))

# user  system elapsed 
# 0.006   0.001   0.009 
# even faster than sapply

### Question 5.12.3 Leaf Area Using Image Processing
source("~/Downloads/getArea.R")

#make files object, a vector of file names as character strings
files <- list.files("~/Downloads/lab8/Q3", pattern = ".JPG")

# make sure getArea.R is working with first file name

getArea(files[1], file_dir = "~/Downloads/lab8/Q3")

### run loop to store areas

# initialize object to store data
file_area <- data.frame(matrix(NA, length(files), 2)) # matrices can only store data of one class so change it to a df for columns of different classes
colnames(file_area) <- c("image", "area") # change col names

i<-1 # initialize counter
for (img in files){
    image_area <- getArea(img,file_dir = "~/Downloads/lab8/Q3") #store area calc
    file_area[i,"image"] <- img # put name in right spot
    file_area[i,"area"] <- image_area # put area in right spot

    i <- i +1 #add to counter
 }

 # split the image column into time and name
file_area[c("time", "name")] <- str_split_fixed(file_area$image, 
                                                            pattern = "_",
                                                            n =2)

# split data into two objects
file_area_t1 <- file_area[file_area$time =="t1", ]
file_area_t2 <- file_area[file_area$time == "t2", ]
# notice these are different lengths, so lets see if they have the same elements

file_area_t2$name %in% file_area_t1$name #notice the 6th element is not
file_area_t1$name %in% file_area_t2$name #looks good

#plot the areas against one another but remove the 6th of file_area_2 which is unique to that df
plot(file_area_t1$area ~ file_area_t2$area[-6])

## looks pretty linear!

## t-test
t.test(file_area_t1$area, file_area_t2$area)

# yes, significant, p-value = 0.0008868

### 5.12.4 Titles and Citations

papers <- read_csv("~/Downloads/lab8/Q4/Letchford2015_data.csv")

## do for year 2010
papers_2010 <- papers[papers$year == 2010, ]
res <- cor.test(rank(papers_2010$title_length), rank(papers_2010$cites), method = "kendall")
#highly signficant p-value < 2.2e-16 with positive tau -0.06551962 

## for nature and science papers

# print all unique paper names
journals <- c("Nature", "Science", "The Lancet", "New Eng J Med")

# first lets make sure our names are correct
journals %in% papers$journal # looks good


## let's do the loop, you should recognize the format by now
test_results <- data.frame(matrix(NA, 4, 3))
colnames(test_results) <- c("Journal", "tau","pval")

i <- 1

for (j in journals){
    paper_j <- papers[papers$journal == j, ]
    res <- cor.test(rank(paper_j$title_length), rank(paper_j$cites), method = "kendall")
    test_results[i,"Journal"] <- j
    test_results[i,"tau"] <- res$estimate
    test_results[i,"pval"] <- res$p.value

    i <- i +1
}
test_results
#lance is not significat and tau is positive and negative across journals.