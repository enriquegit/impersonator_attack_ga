# Script to plot the failure analysis graph.

source("imp_functions.R")

# Set seed
set.seed(constants$seed)

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

# plot lines.
failedusers <- c("condition1", "condition20", "condition21")

df.grouped <- trainset %>%
  group_by(userid) %>% 
  summarise_at(.vars = paste0("h",1:24), .funs = mean)

meltdf <- melt(df.grouped,id="userid")

meltdf$hour <- as.integer(gsub("h","",meltdf$variable))

typeuser <- rep("success", nrow(meltdf))

typeuser[which(meltdf$userid %in% failedusers)] <- "failed"

meltdf$result <- as.factor(typeuser)

p <- ggplot(meltdf,aes(x=hour, y=value, group=userid, color=result)) +
  ggtitle("Ground truth averaged biometric profiles.") + xlab("Hour of day") + ylab("Activity level") +
  geom_line(size=1, aes(alpha=result)) +
  scale_alpha_manual(values=c(1,0.2)) +
  #scale_linetype_manual(values=c("dashed","solid")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

pdf("plots/failedusers.pdf", width = 7, height = 3)
print(p)
dev.off()
