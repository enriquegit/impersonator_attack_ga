# Run the genetic attack against a random forest trained with 50% of the data.
# In this case, the target model does not return conficence scores just 0 or 1.

source("imp_functions.R")

# Set seed.
set.seed(constants$seed)

# Create directories to store results. pctTrain_maxit_popsize
dirPath <- paste0(constants$rootPath,"results/binary_50_",constants$maxIter,"_",constants$populationSize)

if(dir.exists(dirPath)){ # Delete if already exsits.
  unlink(dirPath, recursive = T)
}
dir.create(dirPath)

# Read dataset
dataset <- read.csv(paste0(constants$rootPath,"features/features.csv"), stringsAsFactors = F)

# Get a list of all users
targetUsers <- unique(dataset$userid)

dataset$userid <- as.factor(dataset$userid)

# Generate training set and normalize
trainset <- NULL; testset <- NULL

#Build train/test sets
for(u in targetUsers){
  userdata <- dataset[which(dataset$userid == u),]
  ntrain <- ceiling(nrow(userdata) * (50 / 100))
  idxs <- sample(nrow(userdata), ntrain, replace = F)
  tmptrain <- userdata[idxs,]
  trainset <- rbind(trainset, tmptrain)
  tmptest <- userdata[-idxs,]
  testset <- rbind(testset, tmptest)
}
# Nomralize data
trainset <- cbind(userid=trainset[,1], normalize(trainset[,2:ncol(trainset)]))

# Train the random forest
m <- randomForest(userid ~., trainset, ntree = constants$nt)

# Define fitness function
myfitness <- function(individual, targetUser, feature.names){
  a <- as.data.frame(t(individual))
  colnames(a) <- feature.names
  #pred <- as.character(predict(m, newdata = a, type = "class"))
  probs <- predict(m, newdata = a, type = "prob")
  idx <- max.col(probs)
  pred <- colnames(probs)[idx]
  p <- 0
  if(pred == targetUser){
    p <- 1
  }

  return(p)
}

feature.names <- colnames(trainset)[-c(1)] # get feature names

ub <- rep(1, 24); lb <- rep(0, 24) # set upper and lower bounds

# Perform analysis for each targetUser
user.stats <- NULL; user.rand <- NULL; user.gt <- NULL; user.profile <- NULL
count <- 0
for(targetUser in targetUsers){
  
  print(paste0("Processing user ", count+1,"/",length(targetUsers)," id: ",targetUser))
  count <- count + 1
  
  # Randomly generate initial population
  suggestions <- matrix(data = runif(24*5), ncol = 24)
  
  tiempo <- system.time(GA <- ga("real-valued",
           fitness = myfitness,
           names = feature.names,
           lower = lb,
           upper = ub,
           monitor = constants$monitor,
           popSize = constants$populationSize,
           maxiter = constants$maxIter,
           run = constants$maxIter, # This is set to maxIter to prevent very early termination.
           seed = constants$gaseed,
           suggestions = suggestions,
           parallel = constants$parallel,
           targetUser=targetUser,
           feature.names=feature.names))
  
  if(class(GA@solution)=="matrix"){
    adv <- as.data.frame(t(GA@solution[1,])) # final data point (impersonator example)
  }
  else{
    warning("GA@solution was not a matrix. Result was not stored.")
  }
  
  probs <- predict(m, newdata = adv, type = "prob")
  idx <- max.col(probs); pred <- colnames(probs)[idx]; maxprob <- probs[idx]
  
  success <- F
  if(pred == targetUser && maxprob >= constants$confidence) success <- T
  
  # Generate stats data frame
  tmp <- data.frame(userid=targetUser, its=GA@iter, timesecs=tiempo[3], fitness=GA@fitnessValue, success=success)
  user.stats <- rbind(user.stats, tmp)
  
  print(tmp)
  
  # Generate feature vectors data frames
  u.gt <- trainset[trainset$userid == targetUser,-c(1)]
  u.gt <- colMeans(u.gt)
  user.gt <- rbind(user.gt, cbind(userid=targetUser, as.data.frame(t(u.gt))) )
  
  randgenotype <- as.data.frame(t(suggestions[1,]))
  colnames(randgenotype) <- feature.names
  user.rand <- rbind(user.rand, cbind(userid=targetUser, randgenotype))
  
  user.profile <- rbind(user.profile, cbind(userid=targetUser, adv))
  
} # end of loop targetUsers

# Write results.
write.csv(user.stats, paste0(dirPath,"/stats.csv"), quote = F, row.names = F)
write.csv(user.gt, paste0(dirPath,"/gt.csv"), quote = F, row.names = F)
write.csv(user.profile, paste0(dirPath,"/profile.csv"), quote = F, row.names = F)
write.csv(user.rand, paste0(dirPath,"/rand.csv"), quote = F, row.names = F)

print("Done!!!")