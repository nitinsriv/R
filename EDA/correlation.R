pric <- read.csv("C:\\IIITB\\EDA\\EDA_Gold_Silver_prices _changed.csv", header = TRUE)
cor <- cor(pric$SilverPrice,pric$GoldPrice)

cur <- read.csv("C:\\IIITB\\EDA\\currencies.csv", header = TRUE)
cormat <- round(cor(cur[,2:51]),2)
library(reshape2)
melted_cormat <- melt(cormat)


nas <- cur <- read.csv("C:\\IIITB\\EDA\\nas.csv", header = TRUE)
cor_medu_sib <- cor(nas$Mother.edu,nas$Siblings)