# Lab 5: CH4 Exercise II

## 1. Using the tree data frame and calculate: what’s the average height of the cherry tree?
```
data("trees") # read in tree data

mean(trees$Height)

```
Height is 76.
## 2. What is the average girth of those that are more than 75ft tall?

```
mean(trees$Girth[trees$Height>75])
```

Average girth is 14.5

## 3. What is the maximum height of trees with a volume between 15 and 35 ft^3?

```
medium_trees <- trees[trees$Volume>15 & trees$Volume<35,]
max(medium_trees$Height)

```

Max height for trees with these volumes is 86.

## 4. Read in “Gesquiere2011_data.csv”, for male ID equal 3, what’s the maximum GC value, what’s the mean T value?

Important to remember that the below will have a path specific to your machine. Also, despite this being a .csv file it is actually tab seperated, hence the `sep = "\t"` command in the `read.csv()` command. The `"\t"` is the denominator for tab.

```
nf <- read.csv("~/Downloads/lab1_unix_DSB/data/Gesquiere2011_data.csv", sep = "\t") # read  data into nf object

max(nf[nf$maleID==3,"GC"])

mean(nf[nf$maleID==3,"T"])

```
Max: 136.31
Mean: 86

## 5. How many rows have GC larger than 50, and T larger than 100? (required for grads only)

```
nrow(nf[nf$GC >50 & nf$T > 100,])
```

1117 rows