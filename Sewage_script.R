library(poweRlaw)
library(readxl)
library(magicaxis)


anglian_water <- read_excel('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/Scale free networks/Sewage overflow data/EDM_2021_Storm_Overflow_Annual_Return/Anglian Waterr 2021.xlsx')
all_water <- read_excel('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/Scale free networks/Sewage overflow data/EDM_2021_Storm_Overflow_Annual_Return/EDM 2021 Storm Overflow Annual Return - all water and sewerage companies.xlsx')

for (i in 2:10){
  water <- read_excel('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/Scale free networks/Sewage overflow data/EDM_2021_Storm_Overflow_Annual_Return/EDM 2021 Storm Overflow Annual Return - all water and sewerage companies.xlsx',sheet=i)
  all_water <- rbind(all_water,water)
  print(i)
}

all_water_2021 <- all_water

all_total_spills <- na.omit(as.numeric(na.omit(all_water_2021$`Counted spills using 12-24h count method`)))
all_spill_duration <- as.numeric(na.omit(all_water_2021$`Total Duration (hrs) all spills prior to processing through 12-24h count method`))


total_spills <- anglian_water$`Counted spills using 12-24h count method`
spill_duration <- anglian_water$`Total Duration (hrs) all spills prior to processing through 12-24h count method`

t_pl = displ$new(na.omit(total_spills))

test <- all_total_spills
test[test == 0] <- NA
test <- na.omit(test)

hist1 <- hist(na.omit(total_spills),breaks=30)
hist1_all <- hist(na.omit(all_total_spills),breaks=50)
hist_test <- hist(test,breaks=100)
hist2 <- hist(na.omit(spill_duration),breaks=30)
hist2_all <- hist(na.omit(all_spill_duration),breaks=50)

plot(hist1$counts, log=c('x','y'),type='h',lwd=10,lend=2)

plot(hist1$counts,hist1$mids,pch=4,col='blue')

plot(log10(hist1$mids),log10(hist1$counts),pch=4,col='blue')
plot(log10(hist2$mids),log10(hist2$counts),pch=4,col='blue')

plot(log10(hist1_all$mids),log10(hist1_all$counts),pch=4,col='blue',xlab='Number of spills per station',ylab='Frequency',main='2021 Number of spills',
     xlim=c(0,3),ylim=c(0,4))
axis(1, xaxp=c(0, 3, 3))
minor.ticks.axis(1,9,mn=0,mx=3)
minor.ticks.axis(2,9,mn=0,mx=4)

magplot(hist1_all$mids,hist1_all$counts,log='xy',majorn=c(10,5),logpretty=TRUE,prettybase=TRUE,pch=4,
        xlab='Number of spills per station',ylab='Frequency',main='2021 Number of spills')

magplot(hist2_all$mids,hist2_all$counts,log='xy',majorn=c(10,5),logpretty=TRUE,prettybase=TRUE,pch=4,
        xlab='Spill duration per station',ylab='Frequency',main='2021 Spill duration')

#Number of spills regression

data <- rbind(hist1_all$counts,hist1_all$mids)
data[data ==0] <- NA
data <- na.omit(t(data))


abline(lm(log10(data[,1])~log10(data[,2])),col='red')
summary(lm(log10(data[,1])~log10(data[,2])))
        


#Number of spills regression

data2 <- rbind(hist2_all$counts,hist2_all$mids)
data2[data2 ==0] <- NA
data2 <- na.omit(t(data2))


abline(lm(log10(data2[,1])~log10(data2[,2])),col='red')

summary(lm(log10(data2[,1])~log10(data2[,2]))


        
#Plot just for Anglian water

magplot(hist1$mids,hist1$counts,log='xy',majorn=c(10,5),logpretty=TRUE,prettybase=TRUE,pch=4,
        xlab='Number of spills per station',ylab='Frequency',main='2021 Number of spills - Anglian Water')

magplot(hist2$mids,hist2$counts,log='xy',majorn=c(10,5),logpretty=TRUE,prettybase=TRUE,pch=4,
        xlab='Spill duration per station',ylab='Frequency',main='2021 Spill duration - Anglian Water')




minor.ticks.axis <- function(ax,n,t.ratio=0.5,mn,mx,...){
  
  lims <- par("usr")
  if(ax %in%c(1,3)) lims <- lims[1:2] else lims[3:4]
  
  major.ticks <- pretty(lims,n=5)
  if(missing(mn)) mn <- min(major.ticks)
  if(missing(mx)) mx <- max(major.ticks)
  
  major.ticks <- major.ticks[major.ticks >= mn & major.ticks <= mx]
  
  labels <- sapply(major.ticks,function(i)
    as.expression(bquote(10^ .(i)))
  )
  axis(ax,at=major.ticks,labels=labels,...)
  
  n <- n+2
  minors <- log10(pretty(10^major.ticks[1:2],n))-major.ticks[1]
  minors <- minors[-c(1,n)]
  
  minor.ticks = c(outer(minors,major.ticks,`+`))
  minor.ticks <- minor.ticks[minor.ticks > mn & minor.ticks < mx]
  
  
  axis(ax,at=minor.ticks,tcl=par("tcl")*t.ratio,labels=FALSE)
}





testing <- all_total_spills
na.omit(testing)
testing[testing == 0] <- NA
length(testing)
