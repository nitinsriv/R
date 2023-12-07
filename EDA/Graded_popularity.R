pop <- read.csv("C:\\IIITB\\EDA\\popularity.csv",header=TRUE)
pop_mean <- mean(pop$shares,na.rm=TRUE)
pop_med <- median(pop$shares,na.rm=TRUE)
pop_cnt <- max(pop$shares)
a<-quantile(pop$shares, seq(0.01, 1, 0.01))
df_qt <- pop[which(pop$shares<=10800),]
pop_qt_mean <- mean(df_qt$shares,na.rm=TRUE)
sd_qt <- sd(df_qt$shares)
perc_qt <- length(df_qt)/length(pop)