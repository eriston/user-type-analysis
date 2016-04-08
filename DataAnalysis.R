library(mclust)
library(plyr)

df = dataFor10percentAsOf0402.2300

drops <- c("X","user","events")
df <- df[ , !(names(df) %in% drops)]

df <- df[df[,c('numOfEvents')] > 2, ]
df <- df[df[,c('numOfEvents')] < 999, ]
df <- df[df[,c('totalDistance')] < 999000, ]
df <- df[df[,c('totalSimilarity')] > 0, ]
df <- df[df[,c('totalSimilarity')] < 999999, ]

summary(df)

hist(df$numOfEvents)
hist(df$totalDistance)
hist(df$totalSimilarity)

df <- as.data.frame(scale(df))

COV = cov(t(df))
mydata = COV

set.seed(13)
# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 4, iter.max = 50) # 4 cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
df <- data.frame(df, fit$cluster) 

col.list <- c("orange","black","black","black","black","black")
pch.list <- c(19,3,3,3,3,3)
pch_lookup <- c('1'=19, '2'=3, '3'=3, '4'=3, '5'=3, '6'=3)
cex_lookup <- c('1'=1.0, '2'=0.2, '3'=0.2, '4'=0.2, '5'=0.2, '6'=0.2)
palette(col.list)
par(mfrow=c(2,3))

barplot(table(df$fit.cluster), col = unique(df$fit.cluster), ylim = c(0,800))

df <- arrange(df,numOfEvents)
plot(df[,c('numOfEvents')], col = df$fit.cluster, 
     pch = pch_lookup[as.character(df$fit.cluster)], cex=cex_lookup[as.character(df$fit.cluster)])

df <- arrange(df,totalDistance)
plot(df[,c('totalDistance')], col = df$fit.cluster, 
     pch = pch_lookup[as.character(df$fit.cluster)], cex=cex_lookup[as.character(df$fit.cluster)])

df <- arrange(df,totalSimilarity)
plot(df[,c('totalSimilarity')], col = df$fit.cluster, 
     pch = pch_lookup[as.character(df$fit.cluster)], cex=cex_lookup[as.character(df$fit.cluster)])

df <- arrange(df,avgEventCost)
plot(df[,c('avgEventCost')], col = df$fit.cluster, 
     pch = pch_lookup[as.character(df$fit.cluster)], cex=cex_lookup[as.character(df$fit.cluster)])

df <- arrange(df,totalEventSpending)
plot(df[,c('totalEventSpending')], col = df$fit.cluster, 
     pch = pch_lookup[as.character(df$fit.cluster)], cex=cex_lookup[as.character(df$fit.cluster)])
