bank_data <- read.csv('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/Scale free networks/Bank failures/bank-data.csv')

cost <- bank_data$COST
cost <- na.omit(cost)
cost2 <- cost[cost > 0]

qbfasset <- bank_data$QBFASSET
qbfdep <- bank_data$QBFDEP
hist(qbfasset)
hist(qbfdep)


hist1 <- hist(cost2,breaks=40)

magplot(hist1$mids,hist1$counts,log='xy',majorn=c(10,5),logpretty=TRUE,prettybase=TRUE,pch=4,
        xlab='Losses (Thousand dollars)',ylab='Frequency',main='Banks failures (or requiring FDIC assistance) since 1934 - losses')

hist2 <- hist(log(qbfdep),breaks=50)
magplot(hist2$mids,hist2$counts,log='xy',majorn=c(10,5),logpretty=TRUE,prettybase=TRUE,pch=4,
        xlab='Number of patients being kept in',ylab='Frequency',main='April 2021')

