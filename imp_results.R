# Script used to read the results of the genetic attack and print them.

source("imp_functions.R")


pctTrain <- 50; popSize <- 50; maxIter <- 1500

path <- paste0(constants$rootPath,"results/",pctTrain,"_",maxIter,"_",popSize,"/")

# Read stats

stats <- read.csv(paste0(path,"stats.csv"))

mean(stats$its) # mean iterations

mean(stats$timesecs) # mean time in seconds

sum(stats$success) / 55 # success rate

mean(stats$its * popSize) # mean number of queries


# Read stats for binary case

bstats <- read.csv(paste0(constants$rootPath,"results/binary_",pctTrain,"_",maxIter,"_",popSize,"/stats.csv"))

mean(bstats$its)

mean(bstats$timesecs) # mean time in seconds

sum(bstats$success) / 55 # success rate

