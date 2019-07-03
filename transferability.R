# Script used to test transferability of impersonator examples.

source("imp_functions.R")

# Set seed
set.seed(constants$seed)

# directory where results are for the strong attack. pctTrain_maxit_popsize
dirPath <- paste0(constants$rootPath,"results/50_1500_50/")

# Read stats
stats <- read.csv(paste0(dirPath,"stats.csv"))

# Read dataset
dataset <- read.csv(paste0(constants$rootPath,"features/features.csv"), stringsAsFactors = F)

success.users <- stats$userid[which(stats$success == T)]

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

# Read generated profiles
profiles <- read.csv(paste0(dirPath,"profile.csv"))

# Remove users that were not successfull with random forest.
profiles <- profiles[profiles$userid %in% success.users,]

n <- nrow(profiles)

#### train classifiers ####

# svm
set.seed(constants$seed)
m.svm <- svm(userid ~., trainset, probability = TRUE)
probs <- predict(m.svm, profiles[,-1], probability = T)
matprobs <- attr(probs, "probabilities")
maxs <- apply(matprobs, 1, max)
equals <- probs == profiles$userid
count.conf.svm <- sum(equals[which(maxs >= constants$confidence)])
count.svm <- sum(equals)
count.conf.svm / n
count.svm / n

# naive bayes
set.seed(constants$seed)
m.nb <- naiveBayes(userid ~., trainset)
probs <- predict(m.nb, profiles[,-1], type="raw")
maxs <- apply(probs, 1, max)
equals <- profiles$userid == predict(m.nb, profiles[,-1])
count.conf.nb <- sum(equals[which(maxs >= constants$confidence)])
count.nb <- sum(equals)
count.conf.nb / n
count.nb / n

# knn, k=1
set.seed(constants$seed)
m.knn <- knn3(userid ~., trainset, k=1)
equals <- profiles$userid == predict(m.knn, profiles, type="class")
probs <- predict(m.knn, profiles, type="prob")
maxs <- apply(probs, 1, max)
count.conf.knn <- sum(equals[which(maxs >= constants$confidence)])
count.knn <- sum(equals)
count.conf.knn / n
count.knn / n

# nn
set.seed(constants$seed)
m.nn <- train(userid ~., data = trainset, method = "nnet")
probs <- predict(m.nn, profiles, type="prob")
idxs <- max.col(probs)
equals <- profiles$userid == colnames(probs)[idxs]
maxs <- apply(probs, 1, max)
count.conf.nn <- sum(equals[which(maxs >= constants$confidence)])
count.nn <- sum(equals)
count.conf.nn / n
count.nn / n

# lda
set.seed(constants$seed)
m.lda <- train(userid ~., data = trainset, method = "lda")
probs <- predict(m.lda, profiles, type="prob")
idxs <- max.col(probs)
equals <- profiles$userid == colnames(probs)[idxs]
maxs <- apply(probs, 1, max)
count.conf.lda <- sum(equals[which(maxs >= constants$confidence)])
count.lda <- sum(equals)
count.conf.lda / n
count.lda / n
