Online.Retail <- read.csv("Online Retail.csv", stringsAsFactors=FALSE)

order_wise <- na.omit(Online.Retail)

Amount <- order_wise$Quantity * order_wise$UnitPrice
order_wise <- cbind(order_wise,Amount)

order_wise <- order_wise[order(order_wise$CustomerID),]
monetary <- aggregate(Amount~CustomerID, order_wise, sum)

frequency <- order_wise[,c(7,1)]
temp<-table(as.factor(frequency$CustomerID))
temp<-data.frame(temp)
colnames(temp)[1]<-c("CustomerID")
RFM <-merge(monetary,temp,by="CustomerID")

recency <- order_wise[,c(7,5)]
recency$InvoiceDate<-as.Date(recency$InvoiceDate,"%m/%d/%Y %H:%M")
maximum<-max(recency$InvoiceDate)
maximum<-maximum+1
maximum$diff <-maximum-recency$InvoiceDate
recency$diff<-maximum$diff
recency<-aggregate(recency$diff,by=list(recency$CustomerID),FUN="min")
colnames(recency)[1]<- "CustomerID"
colnames(recency)[2]<- "Recency"

RFM <- merge(RFM, recency, by = ("CustomerID"))
RFM$Recency <- as.numeric(RFM$Recency)

box <- boxplot.stats(RFM$Amount)
out <- box$out
RFM1 <- RFM[ !RFM$Amount %in% out, ]
RFM <- RFM1
box <- boxplot.stats(RFM$Freq)
out <- box$out
RFM1 <- RFM[ !RFM$Freq %in% out, ]
RFM <- RFM1
box <- boxplot.stats(RFM$Recency)
out <- box$out
RFM1 <- RFM[ !RFM$Recency %in% out, ]
RFM <- RFM1

