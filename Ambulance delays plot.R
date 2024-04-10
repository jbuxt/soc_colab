amb_data <- read.csv('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/Scale free networks/Healthcare/Winter Sitreps/Time lost to ambulance delays Winter 2022-23.csv')
cost <- bank_data$COST
cost <- na.omit(cost)
cost2 <- cost[cost > 0]

qbfasset <- bank_data$QBFASSET
qbfdep <- bank_data$QBFDEP
hist(qbfasset)
hist(qbfdep)



hist1 <- hist(as.numeric(unlist(amb_data)),breaks=40)
magplot(hist1$mids,hist1$counts,log='xy',majorn=c(10,5),logpretty=TRUE,prettybase=TRUE,pch=4,
        xlab='Hours lost to ambulance delays',ylab='Frequency',main='Hours lost to ambulance delays per day per hospital - Winter 22/23')
