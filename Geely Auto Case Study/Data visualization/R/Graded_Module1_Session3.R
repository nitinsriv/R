install.packages("zoo")
library(zoo)
ukacc <- data.frame(Seatbelts, date = as.Date(as.yearmon((time(Seatbelts)))))
library(tidyr)
library(dplyr)
library(ggplot2)

ggplot(ukacc,aes(ukacc$drivers,ukacc$kms,col=factor(ukacc$law),size=ukacc$VanKilled)) + geom_point()
ggplot(ukacc,aes(ukacc$drivers,ukacc$kms,col=factor(ukacc$law),size=ukacc$PetrolPrice)) + geom_point()


ggplot(ukacc,aes(ukacc$date,ukacc$drivers)) + geom_line() +geom_smooth() 
ggplot(ukacc,aes(ukacc$date,ukacc$rear)) + geom_line() +geom_smooth()
ggplot(ukacc,aes(ukacc$date,ukacc$front)) + geom_line() +geom_smooth()
ggplot(ukacc,aes(ukacc$date,ukacc$PetrolPrice)) + geom_line() +geom_smooth()
ggplot(ukacc,aes(ukacc$date,ukacc$law)) + geom_line() +geom_smooth()

ggplot(ukacc,aes(ukacc$date,ukacc$drivers,col=ukacc$front,size=ukacc$rear)) + geom_line() 

#OR

ggplot(ukacc,aes(x=ukacc$date,size=ukacc$law))+geom_line(aes(y=ukacc$drivers,color="red"))+
  geom_line(aes(y=ukacc$rear,color="blue")) +geom_line(aes(y=ukacc$front,color="brown"))+
  geom_line(aes(y=ukacc$PetrolPrice*4000,color="yellow"))+
  geom_vline(xintercept = as.numeric(ukacc$date[c(1970,1982)]), linetype=4, size = 1, alpha = .5)

ukacc1 <- ukacc %>% separate(date, into = c("year", "month", "day"), sep="-")
mnth <- ukacc1 %>% group_by(month) %>% summarise(totaldeaths = mean(drivers))

ggplot(mnth,aes(month,totaldeaths))+geom_bar(stat = "identity")

yr <- ukacc1 %>% group_by(year) %>% summarise(totaldeaths = mean(drivers))

ggplot(yr,aes(year,totaldeaths))+geom_bar(stat = "identity")

boxplot(totaldeaths~year,yr)

ggplot(USArrests, aes(x = row.names(USArrests), y = USArrests$Murder, lab)) + geom_col() + theme(axis.text.x=element_text(angle=90, hjust=1))

bowl_perf <- read.csv("C:\\IIITB\\Data visualization\\sir.csv")

bp <- bowl_perf %>% group_by(Wkts) %>% summarise(totalWkts = sum(Inns))

ggplot(bp,aes(Wkts,totalWkts))+geom_col()

bp_med <- bowl_perf %>% group_by(Inns) %>% summarise(medWkts = median(Wkts))

ggplot(bp_med,aes(Inns,medWkts))+geom_col()

ggplot(bowl_perf,aes(as.numeric(Mdns),as.numeric(Econ)))+geom_point() +geom_smooth()

ggplot(sleep, aes(x = ID, y = extra, fill = group)) + geom_col()