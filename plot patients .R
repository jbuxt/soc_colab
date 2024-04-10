library(poweRlaw)
library(readxl)
library(magicaxis)

setwd('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/Scale free networks/Healthcare')

file.list <- list.files(pattern='*.xlsx')

all_data <- april_data2

for (i in 2:length(file.list)){
  hosp <- read_excel(file.list[i])
  hosp_data <- as.numeric(unlist(hosp[,-1]))
  all_data <- c(all_data,hosp_data)
  print(i)
}

April_hosp <- read_excel('/Users/joshbuxton/Library/CloudStorage/OneDrive-UniversityofExeter/Scale free networks/Healthcare/4 Hospital delayed release April 2022.xlsx')
April_data <- April_hosp[,-1]
april_data2 <- as.numeric(unlist(April_data))

hist1 <- hist(as.numeric(unlist(all_data)),breaks=70)

magplot(hist1$mids,hist1$counts,log='xy',majorn=c(10,5),logpretty=TRUE,prettybase=TRUE,pch=4,
        xlab='Number of patients being kept in',ylab='Frequency',main='Patients kept in who should be released - April-December 2022')
