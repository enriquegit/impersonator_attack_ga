# Script to produce the plots and results.

source("imp_functions.R")

#### Plot user lines ####
path <- paste0(constants$rootPath,"features/features.csv")

df <- read.csv(path)

df <- cbind(userid=df[,1], normalize(df[,2:ncol(df)])) # Normalize

df.grouped <- df %>%
  group_by(userid) %>% 
  summarise_at(.vars = paste0("h",1:24), .funs = mean)

meltdf <- melt(df.grouped,id="userid")

meltdf$hour <- as.integer(gsub("h","",meltdf$variable))

ggplot(meltdf,aes(x=hour, y=value, group=userid)) +
  geom_line(color="gray", size=1.2, alpha=0.8)

#### Plot correlations ####

# Transpose df
tdf <- t(df.grouped[,-1])
colnames(tdf) <- df.grouped$userid
library(corrplot)

M <- cor(tdf)

pdf(file = "plots/userscorr.pdf")

corrplot(M, method = "color", tl.pos = "n", order = "FPC", mar = c(0,0,1,0), title="Correlaton between users")

dev.off()

#### Line plots of resulting impersonator examples ####
targetUser <- "control10"

df.profile <- read.csv(paste0(constants$rootPath,"results/50_1500_50/profile.csv"))
df.profile <- melt(df.profile, id="userid")
df.profile$hour <- as.integer(gsub("h","", df.profile$variable))
df.profile <- df.profile[df.profile$userid == targetUser,]
df.profile$type <- "impersonator"

df.gt <- read.csv(paste0(constants$rootPath,"results/50_1500_50/gt.csv"))
df.gt <- melt(df.gt, id="userid")
df.gt$hour <- as.integer(gsub("h","", df.gt$variable))
df.gt <- df.gt[df.gt$userid == targetUser,]
df.gt$type <- "ground truth"

df.rand <- read.csv(paste0(constants$rootPath,"results/50_1500_50/rand.csv"))
df.rand <- melt(df.rand, id="userid")
df.rand$hour <- as.integer(gsub("h","", df.rand$variable))
df.rand <- df.rand[df.rand$userid == targetUser,]
df.rand$type <- "random"

df.all <- rbind(df.profile, df.gt, df.rand)

lines.plot <- ggplot(data=df.all, aes(x=hour, y=value, group=type, color=type)) +
  ggtitle("Biometric profiles") + xlab("Hour of day") + ylab("Activity level") +
  geom_line(aes(linetype=type), size=1) +
  #geom_point(size=1) +
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(values=c("#000000", "#ff0000", "#cccccc")) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

pdf("plots/lines.pdf", width = 7, height = 3)
print(lines.plot)
dev.off()

#### Generate heatmap ####
# Note: the heatmap was generated in Excel by copying the values to the clipboard with write.table()
targetUser <- "control10"

df.profile <- read.csv(paste0(constants$rootPath,"results/50_1500_50/profile.csv"))
df.profile <- df.profile[df.profile$userid == targetUser,]
write.table(df.profile, "clipboard", sep="\t", row.names=FALSE)

df.gt <- read.csv(paste0(constants$rootPath,"results/50_1500_50/gt.csv"))
df.gt <- df.gt[df.gt$userid == targetUser,]
write.table(df.gt, "clipboard", sep="\t", row.names=FALSE, col.names = F)

df.rand <- read.csv(paste0(constants$rootPath,"results/50_1500_50/rand.csv"))
df.rand <- df.rand[df.rand$userid == targetUser,]
write.table(df.rand, "clipboard", sep="\t", row.names=FALSE, col.names = F)


#### compute metrics for Random Forest user classifier of the target model ####
cm <- getCM()

cm$overall["Accuracy"]

mean(cm$byClass[,"Sensitivity"])

mean(cm$byClass[,"Specificity"])

mean(cm$byClass[,"Balanced Accuracy"])
