# find duplicated rows
cust <- read.csv("C:\\IIITB\\EDA\\practice_telex\\customer.csv",header=TRUE)
cust_dup <-cust[duplicated(cust),]
g_cust<-which(duplicated(cust))

#find duplicated rows
intrnt <- read.csv("C:\\IIITB\\EDA\\practice_telex\\internet.csv",header=TRUE)
intrnt_dup <-intrnt[duplicated(intrnt),]
g_intrnt<-which(duplicated(intrnt))

# find duplicated vaues
churn <- read.csv("C:\\IIITB\\EDA\\practice_telex\\churn.csv",header=TRUE)
churn_dup <-churn[duplicated(churn),]
g_churn<-which(duplicated(churn))

#find NA values from churn dataframe
churn_na <- which(is.na(churn))
