# This file contains global variables and auxiliary functions.
# This file is imported by almost every other script.

library(caret)
library(dplyr)
library(reshape2)
library(randomForest)
library(e1071)
library(GA)

# R version 3.6.0 changed its random number generator. We need the following before set.seed() to have consistent results if you are running R >= 3.6.0
if(as.integer(version$major) > 3 ||
   (as.integer(version$major) == 3 && as.numeric(version$minor) >= 0.6)){
     RNGkind(sample.kind = "Rounding")
}

constants <- list()

constants$its <- 10 # Number of iterations for the classification experiment.

constants$nt <- 500 # Number of trees for Random Forest.

constants$seed <- 1010 # Seed for reproducibility.

# GA params

constants$populationSize <- 50 # Population size.

constants$maxIter <- 1500 # Max generations.

constants$runs <- 100 # Stop after n runs without any improvement.

constants$confidence <- 0.20 # Classification confidence threshold.

constants$gaseed <- 1234 # Random seed.

constants$parallel <- 10 # Number of CPU cores for the GA search. My PC has 12 so I choose to use 10.

constants$monitor <- F #Plot every iteration? (plot/F)

constants$rootPath <- "data/" # Root path to dataset


normalize <- function(x){
  #x is a data frame
  for(i in 1:ncol(x)){
    c <- x[,i]
    max <- max(c, na.rm = T)
    min <- -1
    
    if(max==min){
      x[,i] <- max
    }
    else{
      x[,i] <- (c - min) / (max - min)
    }
  }
  x
}

normalize2 <- function(trainset, testset){
  
  for(i in 2:ncol(trainset)){
    c <- trainset[,i]
    c2 <- testset[,i]
    
    max <- max(c, na.rm = T)
    min <- -1 # missing values are set to -1, so this is the minimum.
    
    if(max==min){
      trainset[,i] <- max
      testset[,i] <- max
    }
    else{
      trainset[,i] <- (c - min) / (max - min)
      
      # truncate max values on testset
      idxs <- which(c2 > max)
      if(length(idxs) > 0){
        c2[idxs] <- max
      }
      
      # we know the minimum is always -1 so no need to truncate.
      
      testset[,i] <- (c2 - min) / (max - min)
      
    }
  }
  
  return(list(train=trainset, test=testset))
}

evaluate <- function(it=1){
  
  # Evaluates target model performance.
  
  set.seed(it)
  
  dataset <- read.csv(paste0(constants$rootPath,"features/features.csv"), stringsAsFactors = F)
  
  dataset$userid <- as.factor(dataset$userid)
  users <- unique(dataset$userid)
  
  trainset <- NULL
  testset <- NULL
  
  #Build train/test sets
  for(u in users){
    
    userdata <- dataset[which(dataset$userid == u),]
    ntrain <- ceiling(nrow(userdata) / 2)
    
    idxs <- sample(nrow(userdata), ntrain, replace = F)
    
    tmptrain <- userdata[idxs,]
    trainset <- rbind(trainset, tmptrain)
    
    tmptest <- userdata[-idxs,]
    testset <- rbind(testset, tmptest)
  }

  # Nomralize data
  res <- normalize2(trainset, testset)
  
  trainset <- res$train
  testset <- res$test
  
  #Evaluate
  predictions <- NULL
  groundTruth <- NULL
  
  for(u in users){
    
    usertest <- testset[testset$userid == u,]
    
    new_trainset <- trainset
    
    m <- randomForest(userid ~., new_trainset, ntree = constants$nt)
    preds <- as.character(predict(m, newdata = usertest, type = "class"))
    
    majorityClass <- names(which.max(table(preds)))
    
    groundTruth <- c(groundTruth, u)
    predictions <- c(predictions, majorityClass)
    
  }
  
  results <- data.frame(it=it, groundTruth=groundTruth, predictions=predictions)
  
  return(results)
  
}

runExperiments <- function(){
  
  # Run experiment that evaluates the target model performance.
  
  all <- NULL
  
  for(i in 1:constants$its){
    print(paste0("iteration: ", i))
    res <- evaluate(it=i)
    all <- rbind(all, res)
  }
  
  write.csv(all, paste0(constants$rootPath,"results/results.csv"), quote = F, row.names = F)

}

getCM <- function(){
  
  # Compute confusion matrix.
  
  df <- read.csv(paste0(constants$rootPath,"results/results.csv"))
  
  levels <- as.character(unique(df$groundTruth))
  
  cm <- confusionMatrix(factor(df$predictions, levels = levels), reference = factor(df$groundTruth, levels = levels))
  
  return(cm)
  
}

