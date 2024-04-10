brent <- read.csv('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/Scale free networks/Oil and Gas Prices/DCOILBRENTEU.csv')

brent_price <- na.omit(as.numeric(brent$DCOILBRENTEU))

brent_diff <- array(NA,dim=length(brent_price))

for (i in 1:length(brent_price)){
  brent_diff[i] <- brent_price[i+1] - brent_price[i]
}

abs_brent_diff <- abs(brent_diff[-9151])

hist(abs_brent_diff)


hist1 <- hist(abs_brent_diff,breaks=60)

magplot(hist1$mids,hist1$counts,log='xy',majorn=c(10,5),logpretty=TRUE,prettybase=TRUE,pch=4,
        xlab='Daily Oil Price Changes',ylab='Frequency',main='Oil Price Change')

plot((hist1$mids),(hist1$counts))


library("poweRlaw")

m_pl = conpl$new(abs_brent_diff+1)

est = estimate_xmin(m_pl)

m_pl$setXmin(est[[2]])
m_pl$setPars(est[[3]])

plot(m_pl)
lines(m_pl, col=2)
