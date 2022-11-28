# Header ------------------------------------------------------------------
# Assignment name: Lab 19 Key
# Author name:  Morgan Sparks
# Date:11/22/2022
# Notes:
#1. using *
# Script setup --------------------------------------------------------------

# set working directory below
setwd("~/Downloads/")

# load libraries below
library(tidyverse)
library(SummarizedExperiment)
library(ExploreModelMatrix)
library(limma)
library(Glimma)
library(edgeR)
library(Mus.musculus)

# Exercise 1 --------------------------------------------------------------

### Data
meta <- read_csv("lab19/GSE96870_coldata_all.csv")

# converse mouse id to factor
meta$mouse <- factor(meta$mouse, levels =sort(unique(meta$mouse)))
# convert time to covariates
meta <- meta %>% mutate(Day = as.numeric(str_replace(time,"Day(\\d+)","\\1")))

### Question 1
meta.1 <- meta %>% filter(tissue == "Spinalcord" & sex == "Male")

vd.1 <- VisualizeDesign(sampleData = meta.1, 
                      designFormula = ~ time )


vd.1$cooccurrenceplots
### Question 2
meta.2 <- meta %>% filter(infection == "NonInfected")

vd.2 <- VisualizeDesign(sampleData = meta.2, 
                        designFormula = ~ sex + tissue )


vd.2$cooccurrenceplots

# Exercise II --------------------------------------------------------------

### load files
counts <- read.csv("lab19/GSE96870_counts_all.csv",row.names=1)
meta <- read.csv("lab19/GSE96870_coldata_all.csv")
meta

y <- DGEList(counts = as.matrix(counts),
             samples = meta,
             genes = rownames(counts),
             group = meta$time)

### Question 1 --------------------------------

#counts per million
cpm <- cpm(y)
lcpm <- cpm(y, log=TRUE)

class(y$counts)

L <- mean(y$samples$lib.size) * 1e-6
M <- median(y$samples$lib.size) * 1e-6


# filter expression and filter
keep.exprs <- filterByExpr(y, group=group)
y_filter <- y[keep.exprs,keep.lib.sizes=FALSE]

#normalize filter
lcpm_filter <- cpm(y_filter,log=T)


### Question 2 --------------------------------

library(cowplot)
library(RColorBrewer)
# Cut off for lcpm
lcpm.cutoff <- log2(10/M + 2/L)
# get the original counts info
sampleStats_original <- as_tibble(lcpm,rownames="Genes") %>% pivot_longer(cols=2:10,names_to="Samples",values_to = "logCPM")
# get the filtered counts info
sampleStats_filter <- as_tibble(lcpm_filter,rownames="Genes") %>% pivot_longer(cols=2:10,names_to="Samples",values_to = "logCPM")

### make plots
p1<-ggplot(sampleStats_original)+
  aes(logCPM,group=Samples,color=Samples)+
  stat_density(geom="line",position="identity")+
  scale_color_brewer(palette = "Set3")+
  geom_vline(xintercept=lcpm.cutoff,linetype = 2)+
  ggtitle("A. Raw data")+
  theme_cowplot()+
  theme(legend.position = "none")

p2<-ggplot(sampleStats_filter)+
  aes(logCPM,group=Samples,color=Samples)+
  stat_density(geom="line",position="identity")+
  scale_color_brewer(palette = "Set3")+
  geom_vline(xintercept=lcpm.cutoff,linetype = 2)+
  ggtitle("B. Filtered Data")+
  theme_cowplot()+
  theme(legend.position = c(0.8, 0.6))

plot2by2<-plot_grid(p1,p2,  ncol =2)
plot2by2

### Question 3  --------------------------------

par(mfrow=c(1,1))
# set color pattern for the category "group"
col.group <- as.factor(y_filter$samples$tissue)
levels(col.group) <-  brewer.pal(nlevels(col.group), "Set1")
col.group <- as.character(col.group)

# figure A
plotMDS(lcpm, labels=y_filter$samples$sample, col=col.group)
title(main="A. Tissue")

#Answer

#Sample GSM2545374 is doing something weird, it looks like it's a spinal cord tissue.


### Question 4  --------------------------------
time <- meta$time
tissue <- meta$tissue
sex <- meta$sex

# design matrix (wihout intercept)
design <- model.matrix(~0 +time + tissue +sex)

# contrats matrix
contr.matrix <- makeContrasts(
  Day0vDay4 = timeDay0 - timeDay4, 
  Day4vDay8 = timeDay4 - timeDay8, 
  Day0vDay8 = timeDay0 - timeDay8, 
  levels = colnames(design))
contr.matrix

### Question 5 --------------------------------

par(mfrow=c(1,2))
# Transform count data to log2-counts per million (logCPM), estimate the mean-variance relationship and use this to compute appropriate observation-level weights.
# v$weights calculate the inverse variance weights
v <- voom(y_filter, design, plot=TRUE)
# fit each gene to a linear model using the "design"
vfit <- lmFit(v, design)
# fit each gene 
vfit <- contrasts.fit(vfit, contrasts=contr.matrix)
# Given a linear model fit from lmFit, compute moderated t-statistics, moderated F-statistic, and log-odds of differential expression by empirical Bayes moderation of the standard errors towards a global value.
efit <- eBayes(vfit)
plotSA(efit, main="Final model: Mean-variance trend")

## looks good! variance removed

### Question 6 --------------------------------

tfit <- treat(vfit, lfc=1)
dt <- decideTests(tfit)

par(mfrow=c(1,1))
vennDiagram(dt[,c(1,3)], circle.col=c("turquoise", "salmon","maroon"))

## not a lot of sifnificant genes and none shared!

### Question 7 --------------------------------

Day0.vs.Day8 <- topTreat(tfit, coef=3, number=Inf) # coefficent 4 is Day0vDay8

Day0.vs.Day8.topgenes <- Day0.vs.Day8$genes[1:100]

i <- which(y_filter$genes$genes %in% Day0.vs.Day8.topgenes)

coolmap(lcpm_filter[i,], labRow=y_filter$genes$genes[i],labCol=y_filter$samples$group[y_filter$samples$group])


