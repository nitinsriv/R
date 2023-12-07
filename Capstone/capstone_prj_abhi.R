install.packages("tidyr")
install.packages("dplyr")
install.packages("tseries")
install.packages("forecast")
install.packages("car")
install.packages("MASS")
install.packages("ggplot2")
install.packages("cowplot")
install.packages("reshape2")

library(dplyr)
library(tseries)
library(forecast)
library(car)
library(MASS)
library(ggplot2)
library(cowplot)
library(tidyr)
library(lubridate)
library(dummies)
library(boot)
library(reshape2)

setwd("C:/Users/ORUser/Documents/Misc")
#load the data
rawdata_mainv1<-read.csv("ConsumerElectronics.csv",stringsAsFactors = F)
rawdata_InvestMainv1<-read.csv("Invest.csv",stringsAsFactors = F)
rawdata_ProductListMainv1<-read.csv("ProductList.csv",stringsAsFactors = F)
rawdata_MoreInfoMainv1<-read.csv("MoreInfo.csv",stringsAsFactors = F)

#Explore the main datasets
#-----------------------ProductList Prep-----------------------------#
nrow(rawdata_ProductListMainv1)
str(rawdata_ProductListMainv1)
#Remove Total and \N; Convert Frequency to Numeric 
rawdata_ProductListMainv1<-rawdata_ProductListMainv1[-which(rawdata_ProductListMainv1$ProductName %in% c("Total","\\N")),]
rawdata_ProductListMainv1$Frequency<-as.numeric(rawdata_ProductListMainv1$Frequency)
#Check NAs for Frequency
sum(is.na(rawdata_ProductListMainv1$Frequency)) #No NAs
#Check Summary
summary(rawdata_ProductListMainv1$Frequency)
#Check NAs for Percent
sum(is.na(rawdata_ProductListMainv1$Percent)) #No NAs
#Check Summary
summary(rawdata_ProductListMainv1$Percent)
#Let us bring in the sub_category column to understand the percent of ad split among the ad sub-categories
rawdata_ProductListMainv1<-distinct(merge(rawdata_ProductListMainv1,subset(rawdata_mainv1, select=c(product_analytic_sub_category,product_analytic_vertical)),by.x=c("ProductName"),by.y = c("product_analytic_vertical"),all.x=T))
#View(rawdata_ProductListMainv1)
#We will take the percent of advertisements run for each of the 3 sub category
rawdata_ProductList_grpv1<-rawdata_ProductListMainv1 %>%
  group_by(product_analytic_sub_category) %>%
  summarise(percent=sum(Percent))
#We have the percent investment for each sub-category
Invest_HomeAudio<-(rawdata_ProductList_grpv1$percent[which(rawdata_ProductList_grpv1$product_analytic_sub_category=="HomeAudio")])/100
Invest_CameraAccessory<-(rawdata_ProductList_grpv1$percent[which(rawdata_ProductList_grpv1$product_analytic_sub_category=="CameraAccessory")])/100
Invest_GamingAccessory<-(rawdata_ProductList_grpv1$percent[which(rawdata_ProductList_grpv1$product_analytic_sub_category=="GamingAccessory")])/100


#-----------------------ProductList Prep-----------------------------#

#-----------------------Order Data Prep-----------------------------#
##Consumer Electronics Data
summary(rawdata_mainv1)
str(rawdata_mainv1)

#Need to check the row duplicates before aggregating to week
sum(duplicated(rawdata_mainv1)) #104843 duplicates
#nrow(OrderMain_datav1)
rawdata_mainv1<-distinct(rawdata_mainv1)


# We need to analyse for CameraAccessory, HomeAudio and GamingAccessory only
SubCatergory<-c("CameraAccessory","GamingAccessory","HomeAudio")
rawdata_mainv1<-subset(rawdata_mainv1,rawdata_mainv1$product_analytic_sub_category %in% SubCatergory)
#Check unique(rawdata_mainv1$product_analytic_sub_category)
#Check NAs in complete dataset first
sum(is.na(rawdata_mainv1)) #4086 NA values, let us dig deeper

#Check NAs for fsn_id
sum(is.na(rawdata_mainv1$ï..fsn_id)) #no NAs

#Check NAs, Trim Order DateTime to date and convert to date datatype
sum(is.na(rawdata_mainv1$order_date)) #no NAs
rawdata_mainv1$order_date<-as.Date(substr(rawdata_mainv1$order_date,1,10),"%Y-%m-%d")

#Check NAs for Year
sum(is.na(rawdata_mainv1$Year)) #no NAs

#Check NAs for Month
sum(is.na(rawdata_mainv1$Month)) #no NAs

#Check NAs for order_id
sum(is.na(rawdata_mainv1$order_id)) #no NAs

#Check NAs for order_item_id
sum(is.na(rawdata_mainv1$order_item_id)) #no NAs

#Check NAs for gmv
sum(is.na(rawdata_mainv1$gmv)) #1362 NAs
#View(subset(rawdata_mainv1,is.na(rawdata_mainv1$gmv))) 
#The records no not have customer id or pincode, these might be cancelled orders.
#We can remove these records as they will not help us in the analysis
rawdata_mainv1<-rawdata_mainv1[!is.na(rawdata_mainv1$gmv),]
#Let us check thespread of the GMV
summary(rawdata_mainv1$gmv) ##Investigate GMV 0
#View(subset(rawdata_mainv1,rawdata_mainv1$gmv==0))
#GMV 0 records can also be removed as they will not help in analysis and might be from replace orders
rawdata_mainv1<-rawdata_mainv1[-which(rawdata_mainv1$gmv==0),]
#Check spread using quantile
quantile(rawdata_mainv1$gmv,seq(0,1,0.01)) # Sudden jump from 99% to 100%
#investigate
View(rawdata_mainv1[which(rawdata_mainv1$gmv>9347),])
#Looks like valid orders, 

#Check NAs for units
sum(is.na(rawdata_mainv1$units)) #No NAs
#Let us check thespread of the units
summary(rawdata_mainv1$units) #Spread is very uneven
#Check spread using quantile
quantile(rawdata_mainv1$units,seq(0,1,0.01)) # Sudden jump from 99% to 100%
#investigate
View(rawdata_mainv1[which(rawdata_mainv1$units>2),])
#Looks like valid orders, do not remove



#Check NAs for deliverybdays
sum(is.na(rawdata_mainv1$deliverybdays)) #No NAs
#\N is for days in which delivery was expected to be late but finally did not
#So we can replace \N with 0 and cast the column to numeric
rawdata_mainv1[which(rawdata_mainv1$deliverybdays == "\\N"),]$deliverybdays <- 0
rawdata_mainv1$deliverybdays<-as.numeric(rawdata_mainv1$deliverybdays)
#Check Summary 
summary(rawdata_mainv1$deliverybdays) 
#There are negative values, convert them to 0 that would mean no delay
rawdata_mainv1$deliverybdays[rawdata_mainv1$deliverybdays < 0] <- 0
#Check spread using quantile
quantile(rawdata_mainv1$deliverybdays,seq(0,1,0.01)) # Sudden jump from 99% to 100%
#investigate
View(rawdata_mainv1[which(rawdata_mainv1$deliverybdays>8),])

#Any delay above 10 days will most likely result in cancelled order, let's keep upper limit to 10
#fix
rawdata_mainv1$deliverybdays[which(rawdata_mainv1$deliverybdays>10)]<-10


#Check NAs for deliverycdays
sum(is.na(rawdata_mainv1$deliverycdays)) #No NAs
#\N is for days in which delivery was expected to be late but finally did not
#So we can replace \N with 0 and cast the column to numeric
rawdata_mainv1[which(rawdata_mainv1$deliverycdays == "\\N"),]$deliverycdays <- 0
rawdata_mainv1$deliverycdays<-as.numeric(rawdata_mainv1$deliverycdays)
#Check Summary 
summary(rawdata_mainv1$deliverycdays) 
#There are negative values, convert them to 0 that would mean no delay
rawdata_mainv1$deliverycdays[rawdata_mainv1$deliverycdays < 0] <- 0
#Check spread using quantile
quantile(rawdata_mainv1$deliverycdays,seq(0,1,0.01)) # Sudden jump from 99% to 100%
#investigate
View(rawdata_mainv1[which(rawdata_mainv1$deliverycdays>10),])
#Any delay above 10 days will most likely result in cancelled order, let's keep upper limit to 10
#fix
rawdata_mainv1$deliverycdays[which(rawdata_mainv1$deliverycdays>10)]<-10


#Check NAs for s1_fact.order_payment_type
sum(is.na(rawdata_mainv1$s1_fact.order_payment_type )) #No NAs
#Quick way to take Payment type usage each week
#Add the dummy column for Payment Type and drop the main one
rawdata_mainv1<-cbind(subset(rawdata_mainv1, select=-c(s1_fact.order_payment_type)),dummy(rawdata_mainv1$s1_fact.order_payment_type, sep = "_") )
#str(rawdata_mainv1)
#Change to appropriate column names
colnames(rawdata_mainv1)[which(names(rawdata_mainv1) == "subset(rawdata_mainv1, select = -c(s1_fact.order_payment_type))_COD")] <- "PaymentType_COD"
colnames(rawdata_mainv1)[which(names(rawdata_mainv1) == "subset(rawdata_mainv1, select = -c(s1_fact.order_payment_type))_Prepaid")] <- "PaymentType_Prepaid"

#Check NAs for sla
sum(is.na(rawdata_mainv1$sla)) #No NAs
#Check Summary 
summary(rawdata_mainv1$sla) 
#Check spread using quantile
quantile(rawdata_mainv1$sla,seq(0,1,0.01)) # Sudden jump from 99% to 100%
#investigate
View(rawdata_mainv1[which(rawdata_mainv1$sla>14),])
#Any delivery above 15 days will most likely result in cancelled order, let's keep upper limit to 15
#fix
rawdata_mainv1$sla[which(rawdata_mainv1$sla>15)]<-15


#Check NAs for cust_id
sum(is.na(rawdata_mainv1$cust_id)) #No NAs
#Check Summary 
summary(rawdata_mainv1$cust_id) 

#Check NAs for pincode
sum(is.na(rawdata_mainv1$pincode)) #No NAs
#Check Summary 
summary(rawdata_mainv1$pincode) 

## Both Cust_id and Pincode are masked values and that is why they are in neagtive
## We will take a distinct count when rolling up to week

#Check NAs for product_analytic_super_category
sum(is.na(rawdata_mainv1$product_analytic_super_category)) #No NAs

#Check NAs for product_analytic_category
sum(is.na(rawdata_mainv1$product_analytic_category)) #No NAs

#Check NAs for product_analytic_sub_category
sum(is.na(rawdata_mainv1$product_analytic_sub_category)) #No NAs

#Check NAs for product_analytic_vertical
sum(is.na(rawdata_mainv1$product_analytic_vertical)) #No NAs

#Check NAs for product_mrp
sum(is.na(rawdata_mainv1$product_mrp)) #No NAs
#Check Summary 
summary(rawdata_mainv1$product_mrp) 
#Product MRP 0 records are present, these might be complimentary products, not very relevant for analysis
rawdata_mainv1<-rawdata_mainv1[-which(rawdata_mainv1$product_mrp==0),]
#Check spread using quantile
quantile(rawdata_mainv1$product_mrp,seq(0,1,0.01)) # Sudden jump from 99% to 100%
#investigate
View(rawdata_mainv1[which(rawdata_mainv1$product_mrp>18600),])
#Looks like valid products, with premium price
#we can create a new column for premium prducts by doing the same steps for required sub-categories
rawdata_mainv1$Premium_prod<-NA
#For CameraAccessory
quantile(subset(rawdata_mainv1,rawdata_mainv1$product_analytic_sub_category=="CameraAccessory")$product_mrp,seq(0,1,0.01))
#Check the number of records
nrow(rawdata_mainv1[which(rawdata_mainv1$product_analytic_sub_category=="CameraAccessory" & rawdata_mainv1$product_mrp>19500),]) #1340 products
rawdata_mainv1$Premium_prod[which(rawdata_mainv1$product_analytic_sub_category=="CameraAccessory" & rawdata_mainv1$product_mrp>19500)]<-1

#For GamingAccessory
quantile(subset(rawdata_mainv1,rawdata_mainv1$product_analytic_sub_category=="GamingAccessory")$product_mrp,seq(0,1,0.01))
#Check the number of records
nrow(rawdata_mainv1[which(rawdata_mainv1$product_analytic_sub_category=="GamingAccessory" & rawdata_mainv1$product_mrp> 6990),]) # 3700 products
rawdata_mainv1$Premium_prod[which(rawdata_mainv1$product_analytic_sub_category=="GamingAccessory" & rawdata_mainv1$product_mrp>  6990)]<-1

#For HomeAudio
quantile(subset(rawdata_mainv1,rawdata_mainv1$product_analytic_sub_category=="HomeAudio")$product_mrp,seq(0,1,0.01))
#Check the number of records
nrow(rawdata_mainv1[which(rawdata_mainv1$product_analytic_sub_category=="HomeAudio" & rawdata_mainv1$product_mrp> 10500),]) # 1050 products
rawdata_mainv1$Premium_prod[which(rawdata_mainv1$product_analytic_sub_category=="HomeAudio" & rawdata_mainv1$product_mrp>  10500)]<-1

#1 will define Premium Product and 2 mass products
#we will use them to take the count of both type of products sold in a week
rawdata_mainv1$Premium_prod[which(is.na(rawdata_mainv1$Premium_prod))]<-2
#Add the dummy column for Premium_prod and drop the main one
rawdata_mainv1<-cbind(subset(rawdata_mainv1, select=-c(Premium_prod)),dummy(rawdata_mainv1$Premium_prod, sep = "_") )
#Change to appropriate column names
colnames(rawdata_mainv1)[which(names(rawdata_mainv1) == "subset(rawdata_mainv1, select = -c(Premium_prod))_1")] <- "PremiumProduct"
colnames(rawdata_mainv1)[which(names(rawdata_mainv1) == "subset(rawdata_mainv1, select = -c(Premium_prod))_2")] <- "MassProduct"
str(rawdata_mainv1)


#Check NAs for product_procurement_sla
sum(is.na(rawdata_mainv1$product_procurement_sla)) #No NAs
#Check Summary 
summary(rawdata_mainv1$product_procurement_sla) 
#product_procurement_sla -1 records are present, must me coerced to 0
rawdata_mainv1$product_procurement_sla[which(rawdata_mainv1$product_procurement_sla==-1)]<-0
#Check spread using quantile
quantile(rawdata_mainv1$product_procurement_sla,seq(0,1,0.01)) # Sudden jump from 99% to 100%
#investigate
View(rawdata_mainv1[which(rawdata_mainv1$product_procurement_sla>6),])
#We will assume that Procurement above 10 days will most likely result in cancelled order, let's keep upper limit to 10
#fix
rawdata_mainv1$product_procurement_sla[which(rawdata_mainv1$product_procurement_sla>10)]<-10

#Derived KPIs
#Add columns discount, Marked up products, Marked Down products
#Assumption: MRP*Units will be compared with GMV

##Add a ListPrice Column
rawdata_mainv1$ListPrice<-NA
rawdata_mainv1$ListPrice<-rawdata_mainv1$gmv/rawdata_mainv1$units


##Add a MarkUp Column
rawdata_mainv1$MarkUp_Prods<-NA
rawdata_mainv1$MarkUp_Prods[which(rawdata_mainv1$gmv>(rawdata_mainv1$units*rawdata_mainv1$product_mrp))]<-1
rawdata_mainv1$MarkUp_Prods[which(is.na(rawdata_mainv1$MarkUp_Prods))]<-0

##Add a MarkDown Column
rawdata_mainv1$MarkDown_Prods<-NA
rawdata_mainv1$MarkDown_Prods[which(rawdata_mainv1$gmv<(rawdata_mainv1$units*rawdata_mainv1$product_mrp))]<-1
rawdata_mainv1$MarkDown_Prods[which(is.na(rawdata_mainv1$MarkDown_Prods))]<-0

##Add Discount Percent Column
rawdata_mainv1$Discount_prcnt<-NA
rawdata_mainv1$Discount_prcnt <- (((rawdata_mainv1$units*rawdata_mainv1$product_mrp)-rawdata_mainv1$gmv)/(rawdata_mainv1$units*rawdata_mainv1$product_mrp))*100
rawdata_mainv1$Discount_prcnt[-which(rawdata_mainv1$Discount_prcnt>0)]<-0
#Check spread using quantile and histogram
quantile(rawdata_mainv1$Discount_prcnt,seq(0,1,0.01)) 
hist(rawdata_mainv1$Discount_prcnt, breaks=4)
rawdata_mainv1$Disc_Category <- cut(rawdata_mainv1$Discount_prcnt, 
                                    breaks = c(0, 30, 60, 80, 100), 
                                    labels = c("Low", "Medium", "High", "VeryHigh"), 
                                    right = FALSE)
#Add the dummy column for DiscountCategory and drop the main one
#this will help us check how many times which discounts were used in the orders
#str(rawdata_mainv1)
rawdata_mainv1<-cbind(subset(rawdata_mainv1, select=-c(Discount_prcnt,Disc_Category)),dummy(rawdata_mainv1$Disc_Category, sep = "_") )
#Change to appropriate column names
colnames(rawdata_mainv1)[which(names(rawdata_mainv1) == "subset(rawdata_mainv1, select = -c(Discount_prcnt, Disc_Category))_Low")] <- "Disc_Category_Low"
colnames(rawdata_mainv1)[which(names(rawdata_mainv1) == "subset(rawdata_mainv1, select = -c(Discount_prcnt, Disc_Category))_Medium")] <- "Disc_Category_Medium"
colnames(rawdata_mainv1)[which(names(rawdata_mainv1) == "subset(rawdata_mainv1, select = -c(Discount_prcnt, Disc_Category))_High")] <- "Disc_Category_High"
colnames(rawdata_mainv1)[which(names(rawdata_mainv1) == "subset(rawdata_mainv1, select = -c(Discount_prcnt, Disc_Category))_VeryHigh")] <- "Disc_Category_VeryHigh"

#-----------------------Order Data Prep-----------------------------#



#-----------------------Investment dataset prep-----------------------------#
require(lubridate)
nrow(rawdata_InvestMainv1)
str(rawdata_InvestMainv1)
#Add days of month to calculate daily investment
rawdata_InvestMainv1$DaysOfMonth<-days_in_month(as.Date(paste(rawdata_InvestMainv1$Year,"-",rawdata_InvestMainv1$Month,"-01",sep=""),"%Y-%m-%d"))
#Max GMV is in lacs, we will multiply the investment amount by 100 to convert to lacs 
rawdata_InvestMainv1[,3:12]<-rawdata_InvestMainv1[,3:12]*100
#Calculate Daily Investment Rate for all mediums and Total Investment
rawdata_InvestMainv1$Daily_TotalInvestment<- rawdata_InvestMainv1$TotalInvestment/rawdata_InvestMainv1$DaysOfMonth
rawdata_InvestMainv1$Daily_TV<- rawdata_InvestMainv1$TV/rawdata_InvestMainv1$DaysOfMonth
rawdata_InvestMainv1$Daily_Digital<- rawdata_InvestMainv1$Digital/rawdata_InvestMainv1$DaysOfMonth
rawdata_InvestMainv1$Daily_Sponsorship<- rawdata_InvestMainv1$Sponsorship/rawdata_InvestMainv1$DaysOfMonth
rawdata_InvestMainv1$Daily_ContentMarketing<- rawdata_InvestMainv1$ContentMarketing/rawdata_InvestMainv1$DaysOfMonth
rawdata_InvestMainv1$Daily_OnlineMarketing<- rawdata_InvestMainv1$OnlineMarketing/rawdata_InvestMainv1$DaysOfMonth
rawdata_InvestMainv1$Daily_Affiliates<- rawdata_InvestMainv1$Affiliates/rawdata_InvestMainv1$DaysOfMonth
rawdata_InvestMainv1$Daily_SEM<- rawdata_InvestMainv1$SEM/rawdata_InvestMainv1$DaysOfMonth
rawdata_InvestMainv1$Daily_Radio<- rawdata_InvestMainv1$Radio/rawdata_InvestMainv1$DaysOfMonth
rawdata_InvestMainv1$Daily_Other<- rawdata_InvestMainv1$Other/rawdata_InvestMainv1$DaysOfMonth
#Later we will add this to the Daily Order Data and roll up to weeks
#-----------------------Investment dataset prep-----------------------------#


#-----------------------Other Info Data Prep-----------------------------#
#this dataset contains other info regarding NPS, special Sale
#Macro Economic data has been added as part of best practise
#Calendar data such as Holidays, Weekends, weekdays and weeknum are also added
nrow(rawdata_MoreInfoMainv1)
str(rawdata_MoreInfoMainv1)

#Convert Date Char to Date
rawdata_MoreInfoMainv1$Date<-as.Date(rawdata_MoreInfoMainv1$Date,"%m/%d/%Y")

#SpecialSale Column
#While import instead of NA "" has been taken, so we will convert "" to NA
rawdata_MoreInfoMainv1$SpecialSale[which(rawdata_MoreInfoMainv1$SpecialSale=="")]<-NA
#We just need to mark the Special sale days as 1 and convert to integer, let's do that now
rawdata_MoreInfoMainv1$SpecialSale[which(!is.na(rawdata_MoreInfoMainv1$SpecialSale))]<-1
rawdata_MoreInfoMainv1$SpecialSale[which(is.na(rawdata_MoreInfoMainv1$SpecialSale))]<-0
rawdata_MoreInfoMainv1$SpecialSale<-as.integer(rawdata_MoreInfoMainv1$SpecialSale)

#Indian Holidays , replace NA with 0
rawdata_MoreInfoMainv1$IndiaHolidays[which(is.na(rawdata_MoreInfoMainv1$IndiaHolidays))]<-0

#Macro Economic columns are added: Internet Users and MObile Users per 100
#The data was yearly Hence only two values will be there
#Variables were added as best practise
unique(rawdata_MoreInfoMainv1$ME_InternetUsers)
unique(rawdata_MoreInfoMainv1$ME_MobileUsersPer100)


#-----------------------Other Info Data Prep-----------------------------#




#-----------------------Daily Master Dataset Build-------------------------#
# Order Data + Investment Data
#We will only take Days for which Investment is given. i.e. July-2015 to June-2016
OrderMain_datav1<-merge(rawdata_mainv1,rawdata_InvestMainv1[c(1,2,14:23)],by.x=c("Year","Month"),by.y = c("Year","Month"))
# Order Data + Investment Data + OtherDetails
OrderMain_datav2<-merge(OrderMain_datav1,rawdata_MoreInfoMainv1,by.x=c("order_date"),by.y = c("Date"))
# Order Data + Investment Data + OtherDetails + ProdList
OrderMain_datav1<-merge(OrderMain_datav2,rawdata_ProductListMainv1,by.x=c("product_analytic_vertical"),by.y = c("ProductName"), all.x=T)

#Add Week-Year column for aggregation
OrderMain_datav1$Week_Year<-paste(OrderMain_datav1$WeekNum,"-",OrderMain_datav1$Year,sep="")
#Add DaysInWeek column for Weekly Investment Calculation
OrderMain_datav2<-OrderMain_datav1 %>%
  group_by(Week_Year) %>%
  summarise(DaysInWeek = n_distinct(order_date))
OrderMain_datav1<-merge(OrderMain_datav1,OrderMain_datav2,by="Week_Year",all.x=T)
#We will use the DaysInWeek to calculate weekly investments using Invest_CameraAcessory, Invest_GamingAccessory and Invest_HomeAudio when we create subsets



OrderMain_datav1$Daily_Radio[which(is.na(OrderMain_datav1$Daily_Radio))]<-0
OrderMain_datav1$Daily_Other[which(is.na(OrderMain_datav1$Daily_Other))]<-0
#-----------------------Daily Master Dataset Build-------------------------#
str(OrderMain_datav1)



#-----------------------Weekly Master Dataset Build-------------------------#
str(OrderMain_datav1)
OrderMain_Weeklyv2<-OrderMain_datav1 %>%
  group_by(Week_Year,product_analytic_sub_category.x) %>%
  summarise(
    Year=max(Year),
    Month=max(Month),
    WeekNum=max(WeekNum),
    OrderCount = n_distinct(order_id),
    UniqueItemQty = n_distinct(order_item_id),
    gmv = sum(gmv),
    OrderQty = sum(units),
    DaysInWeek=mean(DaysInWeek),
    Avg_deliverybdays=mean(deliverybdays),
    Median_deliverybdays=median(deliverybdays),
    Max_deliverybdays=max(deliverybdays),
    Avg_deliverycdays=mean(deliverycdays),
    Median_deliverycdays=median(deliverycdays),
    Max_deliverycdays=max(deliverycdays),
    Avg_sla=mean(sla),
    Median_sla=median(sla),
    Max_sla=max(sla),
    Avg_sla=mean(sla),
    Median_sla=median(sla),
    Dist_CustIds=n_distinct(cust_id),
    Total_CustIds=n(),
    Dist_pincode=n_distinct(pincode),
    Total_pincode=n(),
    Avg_product_mrp=mean(product_mrp),
    Median_product_mrp=median(product_mrp),
    Max_product_procurement_sla=max(product_procurement_sla),
    Avg_product_procurement_sla=mean(product_procurement_sla),
    Median_product_procurement_sla=median(product_procurement_sla),
    PaymentType_COD=sum(PaymentType_COD),
    PaymentType_Prepaid=sum(PaymentType_Prepaid),
    PremiumProduct=sum(PremiumProduct),
    MassProduct=sum(MassProduct),
    MarkUp_Prods=sum(MarkUp_Prods),
    MarkDown_Prods=sum(MarkDown_Prods),
    Disc_Category_Low=sum(Disc_Category_Low),
    Disc_Category_Medium=sum(Disc_Category_Medium),
    Disc_Category_High=sum(Disc_Category_High),
    Disc_Category_VeryHigh=sum(Disc_Category_VeryHigh),
    ListPrice=mean(ListPrice),
    Dummy_Special_sale_week=max(SpecialSale),
    NPS_week=max(MonthlyNPS),
    IndiaHolidays=sum(IndiaHolidays),
    ME_InternetUsers=mean(ME_InternetUsers.),
    ME_MobileUsersPer100=mean(ME_MobileUsersPer100),
    Avg_frequency=mean(Frequency),
    Daily_TotalInvestment=mean(Daily_TotalInvestment),
    Daily_TV=mean(Daily_TV),
    Daily_Digital=mean(Daily_Digital),
    Daily_Sponsorship=mean(Daily_Sponsorship),
    Daily_ContentMarketing=mean(Daily_ContentMarketing),
    Daily_OnlineMarketing=mean(Daily_OnlineMarketing),
    Daily_Affiliates=mean(Daily_Affiliates),
    Daily_SEM=mean(Daily_SEM),
    Daily_Radio=mean(Daily_Radio),
    Daily_Other=mean(Daily_Other)
  )

OrderMain_Weeklyv2 <- OrderMain_Weeklyv2[order(OrderMain_Weeklyv2$Year, OrderMain_Weeklyv2$Month,OrderMain_Weeklyv2$WeekNum),]
str(GamingAccessory_WeeklyDatav1)
#-----------------------Weekly Master Dataset Build-------------------------#

#-----------------------GamingAccessory Dataset Build-------------------------#
#Adding Weekly List Price inflation metrics till 4 weeks
GamingAccessory_WeeklyDatav1<-as.data.frame(subset(OrderMain_Weeklyv2,OrderMain_Weeklyv2$product_analytic_sub_category.x=="GamingAccessory"))
GamingAccessory_WeeklyDatav1$ListPricevsLastWeek_value <- lag(GamingAccessory_WeeklyDatav1$ListPrice,1)
GamingAccessory_WeeklyDatav1$ListPricevsLast2Week_value <- lag(GamingAccessory_WeeklyDatav1$ListPrice,2)
GamingAccessory_WeeklyDatav1$ListPricevsLast3Week_value <- lag(GamingAccessory_WeeklyDatav1$ListPrice,3)
GamingAccessory_WeeklyDatav1$ListPricevsLast4Week_value <- lag(GamingAccessory_WeeklyDatav1$ListPrice,4)
GamingAccessory_WeeklyDatav1$ListPricevsLastWeek_value[which(is.na(GamingAccessory_WeeklyDatav1$ListPricevsLastWeek_value))]<-0
GamingAccessory_WeeklyDatav1$ListPricevsLast2Week_value[which(is.na(GamingAccessory_WeeklyDatav1$ListPricevsLast2Week_value))]<-0
GamingAccessory_WeeklyDatav1$ListPricevsLast3Week_value[which(is.na(GamingAccessory_WeeklyDatav1$ListPricevsLast3Week_value))]<-0
GamingAccessory_WeeklyDatav1$ListPricevsLast4Week_value[which(is.na(GamingAccessory_WeeklyDatav1$ListPricevsLast4Week_value))]<-0

GamingAccessory_WeeklyDatav1$ListPricevsLastWeek_InflationPrcnt<-((GamingAccessory_WeeklyDatav1$ListPrice-GamingAccessory_WeeklyDatav1$ListPricevsLastWeek_value)/GamingAccessory_WeeklyDatav1$ListPricevsLastWeek_value)*100
GamingAccessory_WeeklyDatav1$ListPricevsLast2Week_InflationPrcnt<-((GamingAccessory_WeeklyDatav1$ListPrice-GamingAccessory_WeeklyDatav1$ListPricevsLast2Week_value)/GamingAccessory_WeeklyDatav1$ListPricevsLast2Week_value)*100
GamingAccessory_WeeklyDatav1$ListPricevsLast3Week_InflationPrcnt<-((GamingAccessory_WeeklyDatav1$ListPrice-GamingAccessory_WeeklyDatav1$ListPricevsLast3Week_value)/GamingAccessory_WeeklyDatav1$ListPricevsLast3Week_value)*100
GamingAccessory_WeeklyDatav1$ListPricevsLast4Week_InflationPrcnt<-((GamingAccessory_WeeklyDatav1$ListPrice-GamingAccessory_WeeklyDatav1$ListPricevsLast4Week_value)/GamingAccessory_WeeklyDatav1$ListPricevsLast4Week_value)*100

GamingAccessory_WeeklyDatav1$ListPricevsLastWeek_InflationPrcnt[which(GamingAccessory_WeeklyDatav1$ListPricevsLastWeek_InflationPrcnt=="Inf")]<-NA
GamingAccessory_WeeklyDatav1$ListPricevsLast2Week_InflationPrcnt[which(GamingAccessory_WeeklyDatav1$ListPricevsLast2Week_InflationPrcnt=="Inf")]<-NA
GamingAccessory_WeeklyDatav1$ListPricevsLast3Week_InflationPrcnt[which(GamingAccessory_WeeklyDatav1$ListPricevsLast3Week_InflationPrcnt=="Inf")]<-NA
GamingAccessory_WeeklyDatav1$ListPricevsLast4Week_InflationPrcnt[which(GamingAccessory_WeeklyDatav1$ListPricevsLast4Week_InflationPrcnt=="Inf")]<-NA
#Weekly Investment Calculation
#We are Calculating weekly investment using MOnthly Investment as explained below:
#Break Monthly investment to daily by diving MOnthly Investment by Number of Days in month
#Calculate Number of days in all the week
#Daily Investment * Number of Days in week * Percent_InvestmentIn_GameAccessory = Total investment for the week
GamingAccessory_WeeklyDatav1$Weekly_TotalInvestment=Invest_GamingAccessory*GamingAccessory_WeeklyDatav1$Daily_TotalInvestment*GamingAccessory_WeeklyDatav1$DaysInWeek
GamingAccessory_WeeklyDatav1$Weekly_TV=Invest_GamingAccessory*GamingAccessory_WeeklyDatav1$Daily_TV*GamingAccessory_WeeklyDatav1$DaysInWeek
GamingAccessory_WeeklyDatav1$Weekly_Digital=Invest_GamingAccessory*GamingAccessory_WeeklyDatav1$Daily_Digital*GamingAccessory_WeeklyDatav1$DaysInWeek
GamingAccessory_WeeklyDatav1$Weekly_Sponsorship=Invest_GamingAccessory*GamingAccessory_WeeklyDatav1$Daily_Sponsorship*GamingAccessory_WeeklyDatav1$DaysInWeek
GamingAccessory_WeeklyDatav1$Weekly_ContentMarketing=Invest_GamingAccessory*GamingAccessory_WeeklyDatav1$Daily_ContentMarketing*GamingAccessory_WeeklyDatav1$DaysInWeek
GamingAccessory_WeeklyDatav1$Weekly_OnlineMarketing=Invest_GamingAccessory*GamingAccessory_WeeklyDatav1$Daily_OnlineMarketing*GamingAccessory_WeeklyDatav1$DaysInWeek
GamingAccessory_WeeklyDatav1$Weekly_Affiliates=Invest_GamingAccessory*GamingAccessory_WeeklyDatav1$Daily_Affiliates*GamingAccessory_WeeklyDatav1$DaysInWeek
GamingAccessory_WeeklyDatav1$Weekly_SEM=Invest_GamingAccessory*GamingAccessory_WeeklyDatav1$Daily_SEM*GamingAccessory_WeeklyDatav1$DaysInWeek
GamingAccessory_WeeklyDatav1$Weekly_Radio=Invest_GamingAccessory*GamingAccessory_WeeklyDatav1$Daily_Radio*GamingAccessory_WeeklyDatav1$DaysInWeek
GamingAccessory_WeeklyDatav1$Weekly_Other=Invest_GamingAccessory*GamingAccessory_WeeklyDatav1$Daily_Other*GamingAccessory_WeeklyDatav1$DaysInWeek
#Add Id column 
GamingAccessory_WeeklyDatav1$ID <- seq.int(nrow(GamingAccessory_WeeklyDatav1))


#Plot GMV to check the trend
ggplot(GamingAccessory_WeeklyDatav1)+labs(title = "Weekly GMV", x="Weeks", y="GMV") + 
  geom_bar(aes(x=ID, y=(gmv)),stat="identity", fill="#B2EBF2", colour="#E0F7FA")+theme_minimal()




#Plot GMV with Total Investments to check the trend
##Total Investments is in dotted line
ggplot(GamingAccessory_WeeklyDatav1) + labs(title = "Total Investment vs GMV", x="Weeks", y="GMV & Total Investment") + 
  geom_bar(aes(x=ID, y=(gmv/10000)),stat="identity", fill="#B2EBF2", colour="#E0F7FA")+
  geom_line(aes(x=ID, y=Weekly_TotalInvestment),stat="identity",color="#9C27B0",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_TotalInvestment,color="#E64A19"))+theme_minimal()+theme(legend.position="none")

#Plot GMV with TV Investments to check the trend
##TV Investments are in dotted line
ggplot(GamingAccessory_WeeklyDatav1) + labs(title = "TV Investment vs GMV", x="Weeks", y="GMV & TV Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_TV),stat="identity",color="#E64A19",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_TV,color="#E64A19"))+theme_minimal()+theme(legend.position="none")
#We consider that TV ad effect remains 5%  till 6th weeks, with percentage decay rates for 1-6 week given below:
#0.60	0.36	0.22	0.13	0.08	0.05 
GamingAccessory_WeeklyDatav1$TV_adstockv1 <- stats::filter(GamingAccessory_WeeklyDatav1$Weekly_TV,0.6,method="recursive")
ggplot(GamingAccessory_WeeklyDatav1)  + labs(title = "TV: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_TV),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=TV_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Plot GMV with Digital Investments to check the trend
##Digital Investments are in dotted line
ggplot(GamingAccessory_WeeklyDatav1) + labs(title = "Digital Investment vs GMV", x="Weeks", y="GMV & Digital Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Digital),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Digital,color="#006064"))+theme_minimal()+theme(legend.position="none")
#We consider that Digital ad effect remains 10%  till 1st week with percentage decay rate given below:
#0.1
GamingAccessory_WeeklyDatav1$Digital_adstockv1 <- stats::filter(GamingAccessory_WeeklyDatav1$Weekly_Digital,0.1,method="recursive")
ggplot(GamingAccessory_WeeklyDatav1)  + labs(title = "Digital: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Digital),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Digital_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Plot GMV with Sponsorship Investments to check the trend
##Sponsorship Investments are in dotted line
ggplot(GamingAccessory_WeeklyDatav1) + labs(title = "Sponsorship Investment vs GMV", x="Weeks", y="GMV & Sponsorship Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Sponsorship),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Sponsorship,color="#006064"))+theme_minimal()+theme(legend.position="none")
#We consider that Sponsorship ad effect remains 10%  till 4th week with percentage decay rates given below:
#0.4	0.40	0.16	0.06
GamingAccessory_WeeklyDatav1$Sponsorship_adstockv1 <- stats::filter(GamingAccessory_WeeklyDatav1$Weekly_Sponsorship,0.4,method="recursive")
ggplot(GamingAccessory_WeeklyDatav1)  + labs(title = "Sponsorship: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Sponsorship),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Sponsorship_adstockv1),stat="identity",color="blue")+
  theme_minimal()


#Plot GMV with ContentMarketing Investments to check the trend
##ContentMarketing Investments are in dotted line
ggplot(GamingAccessory_WeeklyDatav1) + labs(title = "ContentMarketing Investment vs GMV", x="Weeks", y="GMV & ContentMarketing Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_ContentMarketing),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_ContentMarketing,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that ContentMarketing ad effect remains 10%  till 4th week with percentage decay rates given below:
#0.4	0.40	0.16	0.06
GamingAccessory_WeeklyDatav1$ContentMarketing_adstockv1 <- stats::filter(GamingAccessory_WeeklyDatav1$Weekly_ContentMarketing,0.4,method="recursive")
ggplot(GamingAccessory_WeeklyDatav1)  + labs(title = "ContentMarketing: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_ContentMarketing),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=ContentMarketing_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Plot GMV with OnlineMarketing Investments to check the trend
##OnlineMarketing Investments are in dotted line
ggplot(GamingAccessory_WeeklyDatav1) + labs(title = "OnlineMarketing Investment vs GMV", x="Weeks", y="GMV & OnlineMarketing Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_OnlineMarketing),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_OnlineMarketing,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that OnlineMarketing ad effect remains 9%  till 2nd week with percentage decay rates given below:
#0.3  0.09
GamingAccessory_WeeklyDatav1$OnlineMarketing_adstockv1 <- stats::filter(GamingAccessory_WeeklyDatav1$Weekly_OnlineMarketing,0.3,method="recursive")
ggplot(GamingAccessory_WeeklyDatav1)  + labs(title = "OnlineMarketing: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_OnlineMarketing),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=OnlineMarketing_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Plot GMV with Affiliates Investments to check the trend
##Affiliates Investments are in dotted line
ggplot(GamingAccessory_WeeklyDatav1) + labs(title = "Affiliates Investment vs GMV", x="Weeks", y="GMV & Affiliates Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Affiliates),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Affiliates,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that Affiliates ad effect remains 6%  till 3rd week with percentage decay rates given below:
#0.40	0.16	0.06
GamingAccessory_WeeklyDatav1$Affiliates_adstockv1 <- stats::filter(GamingAccessory_WeeklyDatav1$Weekly_Affiliates,0.4,method="recursive")
ggplot(GamingAccessory_WeeklyDatav1)  + labs(title = "Affiliates: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Affiliates),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Affiliates_adstockv1),stat="identity",color="blue")+
  theme_minimal()


#Plot GMV with SEM Investments to check the trend
##SEM Investments are in dotted line
ggplot(GamingAccessory_WeeklyDatav1) + labs(title = "SEM Investment vs GMV", x="Weeks", y="GMV & SEM Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_SEM),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_SEM,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that SEM ad effect remains 9%  till 2nd week with percentage decay rates given below:
#0.3  0.09
GamingAccessory_WeeklyDatav1$SEM_adstockv1 <- stats::filter(GamingAccessory_WeeklyDatav1$Weekly_SEM,0.3,method="recursive")
ggplot(GamingAccessory_WeeklyDatav1)  + labs(title = "SEM: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_SEM),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=SEM_adstockv1),stat="identity",color="blue")+
  theme_minimal()


#Plot GMV with Radio Investments to check the trend
##Radio Investments are in dotted line
ggplot(GamingAccessory_WeeklyDatav1) + labs(title = "Radio Investment vs GMV", x="Weeks", y="GMV & Radio Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Radio),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Radio,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that Radio ad effect remains 6%  till 3rd week with percentage decay rates given below:
#0.40	0.16	0.06
GamingAccessory_WeeklyDatav1$Radio_adstockv1 <- stats::filter(GamingAccessory_WeeklyDatav1$Weekly_Radio,0.4,method="recursive")
ggplot(GamingAccessory_WeeklyDatav1)  + labs(title = "Radio: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Radio),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Radio_adstockv1),stat="identity",color="blue")+
  theme_minimal()  


#Plot GMV with Other Investments to check the trend
##Other Investments are in dotted line
ggplot(GamingAccessory_WeeklyDatav1) + labs(title = "Other Investment vs GMV", x="Weeks", y="GMV & Other Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Other),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Other,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that Other ad effect remains 9%  till 2nd week with percentage decay rates given below:
#0.3  0.09
GamingAccessory_WeeklyDatav1$Other_adstockv1 <- stats::filter(GamingAccessory_WeeklyDatav1$Weekly_Other,0.3,method="recursive")
ggplot(GamingAccessory_WeeklyDatav1)  + labs(title = "Other: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Other),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Other_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Let us reduce the number of variables below 40 to create a dataset for regression
#Le us remove the unnecessaryvariables first
GamingAccessory_WeeklyDatav2<-GamingAccessory_WeeklyDatav1[,c(6:9,11:45,60,75:83)]
#We will take correlatio for determining the Avg, Mean and Max type derived KPI relevance
library(corrplot)

corrplot(cor(GamingAccessory_WeeklyDatav2[,c(3,5:13,18:22)]),method="square")
#only the variables with good correlation will be retained
GamingAccessory_WeeklyDatav2<-GamingAccessory_WeeklyDatav2[,-c(5,6,8,9,11,12,19,21,22)]
#Let's check for the complete data frame
corrplot(cor(GamingAccessory_WeeklyDatav2,use = "complete.obs"),method="square")
#Take a closer look at first 12 columns for very high co-relation
corrplot(cor(GamingAccessory_WeeklyDatav2[,1:15],use = "complete.obs"),method="number")
#Remove Order Count, Total_custId, Total_PinCode,OrderQty for now
GamingAccessory_WeeklyDatav2<-GamingAccessory_WeeklyDatav2[,-c(1,4,9,11)]
#We will remove the rest using VIF

#Scale the quantitative variables 

GamingAccessory_WeeklyDatav2_scaled<-cbind(scale(GamingAccessory_WeeklyDatav2[,c(-2,-21)]  , center = FALSE, scale = TRUE),GamingAccessory_WeeklyDatav2[,c(2,21)])


#-----------------------GamingAccessory Dataset Build-------------------------#

#-----------------------GamingAccessory Additive MOdel-------------------------#
#set the seed to 100 
set.seed(123)
# Create row indices for train dataset
trainindices= sample(1:nrow(GamingAccessory_WeeklyDatav2_scaled), 0.8*nrow(GamingAccessory_WeeklyDatav2_scaled))
# Create the train data set
traindata = GamingAccessory_WeeklyDatav2_scaled[trainindices,]
# Create test dataset
testdata = GamingAccessory_WeeklyDatav2_scaled[-trainindices,]

#Model_1 
model1<-lm(gmv ~ ., data = traindata)
summary(model1)  
#Check StepAIC
library(MASS)
step<-stepAIC(model1, direction="both")
step
#Model2 from StepAIC output
model2<-lm(formula = gmv ~ 
             UniqueItemQty+
             Max_deliverybdays+
             Max_deliverycdays+
             Max_sla+
             Dist_CustIds+
             Dist_pincode+
             Max_product_procurement_sla+
             PaymentType_COD+
             PaymentType_Prepaid+
             PremiumProduct+
             MarkDown_Prods+
             Disc_Category_Low+
             Disc_Category_Medium+
             Disc_Category_High+
             ListPrice+
             NPS_week+
             ME_InternetUsers+
             Avg_frequency+
             ListPricevsLastWeek_InflationPrcnt+
             Digital_adstockv1+
             Sponsorship_adstockv1+
             ContentMarketing_adstockv1+
             OnlineMarketing_adstockv1+
             SEM_adstockv1+
             Radio_adstockv1+
             Other_adstockv1+
             TV_adstockv1
           ,data = traindata)
summary(model2)
#Adjusted R-squared - 0.995
#Check VIF
library(car)
as.data.frame(vif(model2))
#Remove Paymentype_COD

#Model 3 
model3<-lm(formula = gmv ~ 
             UniqueItemQty+
             Max_deliverybdays+
             Max_deliverycdays+
             Max_sla+
             Dist_CustIds+
             Dist_pincode+
             Max_product_procurement_sla+
             #PaymentType_COD+
             PaymentType_Prepaid+
             PremiumProduct+
             MarkDown_Prods+
             Disc_Category_Low+
             Disc_Category_Medium+
             Disc_Category_High+
             ListPrice+
             NPS_week+
             ME_InternetUsers+
             Avg_frequency+
             ListPricevsLastWeek_InflationPrcnt+
             Digital_adstockv1+
             Sponsorship_adstockv1+
             ContentMarketing_adstockv1+
             OnlineMarketing_adstockv1+
             SEM_adstockv1+
             Radio_adstockv1+
             Other_adstockv1+
             TV_adstockv1
           ,data = traindata)
summary(model3)
#Too high adjusted R-squared - 0.99
#Check VIF

as.data.frame(vif(model3))
#Remove UniqueItemQty

#Model 4
model4<-lm(formula = gmv ~ 
             #UniqueItemQty+
             Max_deliverybdays+
             Max_deliverycdays+
             Max_sla+
             Dist_CustIds+
             Dist_pincode+
             Max_product_procurement_sla+
             #PaymentType_COD+
             PaymentType_Prepaid+
             PremiumProduct+
             MarkDown_Prods+
             Disc_Category_Low+
             Disc_Category_Medium+
             Disc_Category_High+
             ListPrice+
             NPS_week+
             ME_InternetUsers+
             Avg_frequency+
             ListPricevsLastWeek_InflationPrcnt+
             Digital_adstockv1+
             Sponsorship_adstockv1+
             ContentMarketing_adstockv1+
             OnlineMarketing_adstockv1+
             SEM_adstockv1+
             Radio_adstockv1+
             Other_adstockv1+
             TV_adstockv1
           ,data = traindata)
summary(model4)
#Too high adjusted R-squared - 0.9946
#Check VIF

as.data.frame(vif(model4))
#Remove MarkDown_Prods

#Model 5
model5<-lm(formula = gmv ~ 
             #UniqueItemQty+
             Max_deliverybdays+
             Max_deliverycdays+
             Max_sla+
             Dist_CustIds+
             Dist_pincode+
             Max_product_procurement_sla+
             #PaymentType_COD+
             PaymentType_Prepaid+
             PremiumProduct+
             #MarkDown_Prods+
             Disc_Category_Low+
             Disc_Category_Medium+
             Disc_Category_High+
             ListPrice+
             NPS_week+
             ME_InternetUsers+
             Avg_frequency+
             ListPricevsLastWeek_InflationPrcnt+
             Digital_adstockv1+
             Sponsorship_adstockv1+
             ContentMarketing_adstockv1+
             OnlineMarketing_adstockv1+
             SEM_adstockv1+
             Radio_adstockv1+
             Other_adstockv1+
             TV_adstockv1
           ,data = traindata)
summary(model5)
#Too high adjusted R-squared - 0.994
#Check VIF

as.data.frame(vif(model5))
#Remove Dist_CustIds

#Model 6
model6<-lm(formula = gmv ~ 
             #UniqueItemQty+
             Max_deliverybdays+
             Max_deliverycdays+
             Max_sla+
             #Dist_CustIds+
             Dist_pincode+
             Max_product_procurement_sla+
             #PaymentType_COD+
             PaymentType_Prepaid+
             PremiumProduct+
             #MarkDown_Prods+
             Disc_Category_Low+
             Disc_Category_Medium+
             Disc_Category_High+
             ListPrice+
             NPS_week+
             ME_InternetUsers+
             Avg_frequency+
             ListPricevsLastWeek_InflationPrcnt+
             Digital_adstockv1+
             Sponsorship_adstockv1+
             ContentMarketing_adstockv1+
             OnlineMarketing_adstockv1+
             SEM_adstockv1+
             Radio_adstockv1+
             Other_adstockv1+
             TV_adstockv1
           ,data = traindata)
summary(model6)
#Too high adjusted R-squared - 0.9939
#Check VIF

as.data.frame(vif(model6))
#Remove SEM_adstockv1


#Model 7
model7<-lm(formula = gmv ~ 
             #UniqueItemQty+
             Max_deliverybdays+
             Max_deliverycdays+
             Max_sla+
             #Dist_CustIds+
             Dist_pincode+
             Max_product_procurement_sla+
             #PaymentType_COD+
             PaymentType_Prepaid+
             PremiumProduct+
             #MarkDown_Prods+
             Disc_Category_Low+
             Disc_Category_Medium+
             Disc_Category_High+
             ListPrice+
             NPS_week+
             ME_InternetUsers+
             Avg_frequency+
             ListPricevsLastWeek_InflationPrcnt+
             Digital_adstockv1+
             Sponsorship_adstockv1+
             ContentMarketing_adstockv1+
             OnlineMarketing_adstockv1+
             #SEM_adstockv1+
             Radio_adstockv1+
             Other_adstockv1+
             TV_adstockv1
           ,data = traindata)
summary(model7)
#Too high adjusted R-squared - 0.9935
#Check VIF

as.data.frame(vif(model7))
#Remove Max_deliverycdays


#Model 8
model8<-lm(formula = gmv ~ 
             #UniqueItemQty+
             Max_deliverybdays+
             #Max_deliverycdays+
             Max_sla+
             #Dist_CustIds+
             Dist_pincode+
             Max_product_procurement_sla+
             #PaymentType_COD+
             PaymentType_Prepaid+
             PremiumProduct+
             #MarkDown_Prods+
             Disc_Category_Low+
             Disc_Category_Medium+
             Disc_Category_High+
             ListPrice+
             NPS_week+
             ME_InternetUsers+
             Avg_frequency+
             ListPricevsLastWeek_InflationPrcnt+
             Digital_adstockv1+
             Sponsorship_adstockv1+
             ContentMarketing_adstockv1+
             OnlineMarketing_adstockv1+
             #SEM_adstockv1+
             Radio_adstockv1+
             Other_adstockv1+
             TV_adstockv1
           ,data = traindata)
summary(model8)
#Too high adjusted R-squared - 0.991
#Check VIF

as.data.frame(vif(model8))
#Remove ContentMarketing_adstockv1


#Model 9
model9<-lm(formula = gmv ~ 
             #UniqueItemQty+
             Max_deliverybdays+
             #Max_deliverycdays+
             Max_sla+
             #Dist_CustIds+
             Dist_pincode+
             Max_product_procurement_sla+
             #PaymentType_COD+
             PaymentType_Prepaid+
             PremiumProduct+
             #MarkDown_Prods+
             Disc_Category_Low+
             Disc_Category_Medium+
             Disc_Category_High+
             ListPrice+
             NPS_week+
             ME_InternetUsers+
             Avg_frequency+
             ListPricevsLastWeek_InflationPrcnt+
             Digital_adstockv1+
             Sponsorship_adstockv1+
             #ContentMarketing_adstockv1+
             OnlineMarketing_adstockv1+
             #SEM_adstockv1+
             Radio_adstockv1+
             Other_adstockv1+
             TV_adstockv1
           ,data = traindata)
summary(model9)
#Too high adjusted R-squared - 9911
#Check VIF

as.data.frame(vif(model9))
#Remove Other_adstockv1

#Model 10
model10<-lm(formula = gmv ~ 
              #UniqueItemQty+
              Max_deliverybdays+
              #Max_deliverycdays+
              Max_sla+
              #Dist_CustIds+
              Dist_pincode+
              Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              Disc_Category_High+
              ListPrice+
              NPS_week+
              ME_InternetUsers+
              Avg_frequency+
              ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1+
              #ContentMarketing_adstockv1+
              OnlineMarketing_adstockv1+
              #SEM_adstockv1+
              Radio_adstockv1+
              #Other_adstockv1+
              TV_adstockv1
            ,data = traindata)
summary(model10)
#Too high adjusted R-squared - 0.9862
#Check VIF

as.data.frame(vif(model10))
#Remove Dist_pincode


#Model 11
model11<-lm(formula = gmv ~ 
              #UniqueItemQty+
              Max_deliverybdays+
              #Max_deliverycdays+
              Max_sla+
              #Dist_CustIds+
              #Dist_pincode+
              Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              Disc_Category_High+
              ListPrice+
              NPS_week+
              ME_InternetUsers+
              Avg_frequency+
              ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1+
              #ContentMarketing_adstockv1+
              OnlineMarketing_adstockv1+
              #SEM_adstockv1+
              Radio_adstockv1+
              #Other_adstockv1+
              TV_adstockv1
            ,data = traindata)
summary(model11)
#Too high adjusted R-squared - 0.982
#Check VIF

as.data.frame(vif(model11))
#Remove Max_product_procurement_sla


#Model 12
model12<-lm(formula = gmv ~ 
              #UniqueItemQty+
              Max_deliverybdays+
              #Max_deliverycdays+
              Max_sla+
              #Dist_CustIds+
              #Dist_pincode+
              #Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              Disc_Category_High+
              ListPrice+
              NPS_week+
              ME_InternetUsers+
              Avg_frequency+
              ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1+
              #ContentMarketing_adstockv1+
              OnlineMarketing_adstockv1+
              #SEM_adstockv1+
              Radio_adstockv1+
              #Other_adstockv1+
              TV_adstockv1
            ,data = traindata)
summary(model12)
#Too high adjusted R-squared - 0.9822
#Check VIF

as.data.frame(vif(model12))
#Remove OnlineMarketing_adstockv1


#Model 13
model13<-lm(formula = gmv ~ 
              #UniqueItemQty+
              Max_deliverybdays+
              #Max_deliverycdays+
              Max_sla+
              #Dist_CustIds+
              #Dist_pincode+
              #Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              Disc_Category_High+
              ListPrice+
              NPS_week+
              ME_InternetUsers+
              Avg_frequency+
              ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1+
              #ContentMarketing_adstockv1+
              #OnlineMarketing_adstockv1+
              #SEM_adstockv1+
              Radio_adstockv1+
              #Other_adstockv1+
              TV_adstockv1
            ,data = traindata)
summary(model13)
#Too high adjusted R-squared - 0.9808
#Check VIF

as.data.frame(vif(model13))
#Remove ME_InternetUsers


#Model 14
model14<-lm(formula = gmv ~ 
              #UniqueItemQty+
              Max_deliverybdays+
              #Max_deliverycdays+
              Max_sla+
              #Dist_CustIds+
              #Dist_pincode+
              #Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              Disc_Category_High+
              ListPrice+
              NPS_week+
              #ME_InternetUsers+
              Avg_frequency+
              ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1+
              #ContentMarketing_adstockv1+
              #OnlineMarketing_adstockv1+
              #SEM_adstockv1+
              Radio_adstockv1+
              #Other_adstockv1+
              TV_adstockv1
            ,data = traindata)
summary(model14)
#Too high adjusted R-squared - 0.9815
#Check VIF

as.data.frame(vif(model14))
#Remove Max_deliverybdays

#Model 15
model15<-lm(formula = gmv ~ 
              #UniqueItemQty+
              #Max_deliverybdays+
              #Max_deliverycdays+
              Max_sla+
              #Dist_CustIds+
              #Dist_pincode+
              #Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              Disc_Category_High+
              ListPrice+
              NPS_week+
              #ME_InternetUsers+
              Avg_frequency+
              ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1+
              #ContentMarketing_adstockv1+
              #OnlineMarketing_adstockv1+
              #SEM_adstockv1+
              Radio_adstockv1+
              #Other_adstockv1+
              TV_adstockv1
            ,data = traindata)
summary(model15)
#Too high adjusted R-squared - 0.9821
#Check VIF

as.data.frame(vif(model15))
#Remove PremiumProduct


#Model 16
model16<-lm(formula = gmv ~ 
              #UniqueItemQty+
              #Max_deliverybdays+
              #Max_deliverycdays+
              Max_sla+
              #Dist_CustIds+
              #Dist_pincode+
              #Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              #PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              Disc_Category_High+
              ListPrice+
              NPS_week+
              #ME_InternetUsers+
              Avg_frequency+
              ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1+
              #ContentMarketing_adstockv1+
              #OnlineMarketing_adstockv1+
              #SEM_adstockv1+
              Radio_adstockv1+
              #Other_adstockv1+
              TV_adstockv1
            ,data = traindata)
summary(model16)
#Too high adjusted R-squared - 0.9824
#Check VIF

as.data.frame(vif(model16))
#Remove PremiumProduct


#Model 17
model17<-lm(formula = gmv ~ 
              #UniqueItemQty+
              #Max_deliverybdays+
              #Max_deliverycdays+
              #Max_sla+
              #Dist_CustIds+
              #Dist_pincode+
              #Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              #PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              Disc_Category_High+
              ListPrice+
              NPS_week+
              #ME_InternetUsers+
              Avg_frequency+
              ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1+
              #ContentMarketing_adstockv1+
              #OnlineMarketing_adstockv1+
              #SEM_adstockv1+
              Radio_adstockv1+
              #Other_adstockv1+
              TV_adstockv1
            ,data = traindata)
summary(model17)
#Too high adjusted R-squared - 0.9828
#Check VIF
as.data.frame(vif(model17))
#Remove NPS_week

#Model 18
model18<-lm(formula = gmv ~ 
              #UniqueItemQty+
              #Max_deliverybdays+
              #Max_deliverycdays+
              #Max_sla+
              #Dist_CustIds+
              #Dist_pincode+
              #Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              #PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              Disc_Category_High+
              ListPrice+
              #NPS_week+
              #ME_InternetUsers+
              Avg_frequency+
              ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1+
              #ContentMarketing_adstockv1+
              #OnlineMarketing_adstockv1+
              #SEM_adstockv1+
              Radio_adstockv1+
              #Other_adstockv1+
              TV_adstockv1
            ,data = traindata)
summary(model18)
#Too high adjusted R-squared - 0.982
#Check VIF
as.data.frame(vif(model18))
#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(model18,testdata)
testdata$test_gmv <- Predict
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.951
#Model R-squared: 0.982
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
#358820
# plot Residuals vs Fitted
plot(model18,pch=16,which=1,col="green")
#Seems random, let us continue to refine the model
#Remove Avg_frequency


#Model 19
model19<-lm(formula = gmv ~ 
              #UniqueItemQty+
              #Max_deliverybdays+
              #Max_deliverycdays+
              #Max_sla+
              #Dist_CustIds+
              #Dist_pincode+
              #Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              #PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              Disc_Category_High+
              ListPrice+
              #NPS_week+
              #ME_InternetUsers+
              #Avg_frequency+
              ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1+
              #ContentMarketing_adstockv1+
              #OnlineMarketing_adstockv1+
              #SEM_adstockv1+
              Radio_adstockv1+
              #Other_adstockv1+
              TV_adstockv1
            ,data = traindata)
summary(model19)

#Adjusted R-squared - 0.98
#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(model19,testdata)
testdata$test_gmv <- Predict
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.949
#Model R-squared: 0.98
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#336972 lowered a bit
# plot Residuals vs Fitted
plot(model19,pch=16,which=1,col="green")
#Seems random, let us continue to refine the model

#Remove ListPricevsLastWeek_InflationPrcnt

#Model 20
model20<-lm(formula = gmv ~ 
              #UniqueItemQty+
              #Max_deliverybdays+
              #Max_deliverycdays+
              #Max_sla+
              #Dist_CustIds+
              #Dist_pincode+
              #Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              #PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              Disc_Category_High+
              ListPrice+
              #NPS_week+
              #ME_InternetUsers+
              #Avg_frequency+
              #ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1+
              #ContentMarketing_adstockv1+
              #OnlineMarketing_adstockv1+
              #SEM_adstockv1+
              Radio_adstockv1+
              #Other_adstockv1+
              TV_adstockv1
            ,data = traindata)
summary(model20)
#Adjusted R-squared - 0.975
#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(model20,testdata)
testdata$test_gmv <- Predict
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.955
#Model R-squared: 0.975
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#315914 lowered a bit
# plot Residuals vs Fitted
plot(model20,pch=16,which=1,col="green")
#Seems random, let us continue to refine the model

#Remove ListPrice

#Model 21
model21<-lm(formula = gmv ~ 
              #UniqueItemQty+
              #Max_deliverybdays+
              #Max_deliverycdays+
              #Max_sla+
              #Dist_CustIds+
              #Dist_pincode+
              #Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              #PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              Disc_Category_High+
              #ListPrice+
              #NPS_week+
              #ME_InternetUsers+
              #Avg_frequency+
              #ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1+
              #ContentMarketing_adstockv1+
              #OnlineMarketing_adstockv1+
              #SEM_adstockv1+
              Radio_adstockv1+
              #Other_adstockv1+
              TV_adstockv1
            ,data = traindata)
summary(model21)
#Too high adjusted R-squared - 0.974
#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(model21,testdata)
testdata$test_gmv <- Predict
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.954
#Model R-squared: 0.974
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#321129 higher than model20
# plot Residuals vs Fitted
plot(model21,pch=16,which=1,col="green")
#Seems random, let us continue to refine the model

#Remove Disc_Category_High

#Model 22
model22<-lm(formula = gmv ~ 
              #UniqueItemQty+
              #Max_deliverybdays+
              #Max_deliverycdays+
              #Max_sla+
              #Dist_CustIds+
              #Dist_pincode+
              #Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              #PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              #Disc_Category_High+
              #ListPrice+
              #NPS_week+
              #ME_InternetUsers+
              #Avg_frequency+
              #ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1+
              #ContentMarketing_adstockv1+
              #OnlineMarketing_adstockv1+
              #SEM_adstockv1+
              Radio_adstockv1+
              #Other_adstockv1+
              TV_adstockv1
            ,data = traindata)
summary(model22)
#Too high adjusted R-squared - 0.971
#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(model22,testdata)
testdata$test_gmv <- Predict
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.941
#Model R-squared: 0.971
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#360732 higher than model20
# plot Residuals vs Fitted
plot(model22,pch=16,which=1,col="green")
#Seems random, let us continue to refine the model

#Remove Radio_adstockv1


#Model 23
model23<-lm(formula = gmv ~ 
              #UniqueItemQty+
              #Max_deliverybdays+
              #Max_deliverycdays+
              #Max_sla+
              #Dist_CustIds+
              #Dist_pincode+
              #Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              #PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              #Disc_Category_High+
              #ListPrice+
              #NPS_week+
              #ME_InternetUsers+
              #Avg_frequency+
              #ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1+
              #ContentMarketing_adstockv1+
              #OnlineMarketing_adstockv1+
              #SEM_adstockv1+
              #Radio_adstockv1+
              #Other_adstockv1+
              TV_adstockv1
            ,data = traindata)
summary(model23)
#Adjusted R-squared - 0.968
#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(model23,testdata)
testdata$test_gmv <- Predict
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.936
#Model R-squared: 0.968
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#368724 higher than model20 and increasing steadily
# plot Residuals vs Fitted
plot(model23,pch=16,which=1,col="green")
#Seems random, let us continue to refine the model

#Remove TV_adstockv1


#Model 24
model24<-lm(formula = gmv ~ 
              #UniqueItemQty+
              #Max_deliverybdays+
              #Max_deliverycdays+
              #Max_sla+
              #Dist_CustIds+
              #Dist_pincode+
              #Max_product_procurement_sla+
              #PaymentType_COD+
              PaymentType_Prepaid+
              #PremiumProduct+
              #MarkDown_Prods+
              Disc_Category_Low+
              Disc_Category_Medium+
              #Disc_Category_High+
              #ListPrice+
              #NPS_week+
              #ME_InternetUsers+
              #Avg_frequency+
              #ListPricevsLastWeek_InflationPrcnt+
              Digital_adstockv1+
              Sponsorship_adstockv1
            #ContentMarketing_adstockv1+
            #OnlineMarketing_adstockv1+
            #SEM_adstockv1+
            #Radio_adstockv1+
            #Other_adstockv1+
            #TV_adstockv1
            ,data = traindata)
summary(model24)
#Adjusted R-squared - 0.965
#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(model24,testdata)
testdata$test_gmv <- Predict
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.919
#Model R-squared: 0.965
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#411651 higher than model20 and increasing steadily
# plot Residuals vs Fitted
plot(model24,pch=16,which=1,col="green")
#Seems random, let us continue to refine the model

#We can stop here and consider model20 as the RMSE is low and Predicted and Actual adjusted R-squared are close
#predicted R-squared: 0.955
#Model R-squared: 0.975
#RMSE: 315914
# plot Residuals vs Fitted
plot(model20,pch=16,which=1,col="green")
#Seems random

#Try Bootstrapping
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 
# bootstrapping with 1000 replications 
bootresults <- boot(data=GamingAccessory_WeeklyDatav2_scaled, statistic=rsq, 
                    R=1000, formula = gmv ~ PaymentType_Prepaid+
                      Disc_Category_Low+
                      Disc_Category_Medium+
                      Disc_Category_High+
                      ListPrice+
                      Digital_adstockv1+
                      Sponsorship_adstockv1+
                      Radio_adstockv1+
                      TV_adstockv1)

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#   original  bias    std. error
# t1*    0.976 0.00278     0.00753

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ PaymentType_Prepaid+
                       Disc_Category_Low+
                       Disc_Category_Medium+
                       Disc_Category_High+
                       ListPrice+
                       Digital_adstockv1+
                       Sponsorship_adstockv1+
                       Radio_adstockv1+
                       TV_adstockv1,   # model to fit
                     data = GamingAccessory_WeeklyDatav2_scaled,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#   RMSE    Rsquared  MAE   
#  231820  0.964     191657    
#Final Model Summary
summary(model20)  
#Call:
#lm(formula = gmv ~ PaymentType_Prepaid + Disc_Category_Low + 
#    Disc_Category_Medium + Disc_Category_High + ListPrice + Digital_adstockv1 + 
#    Sponsorship_adstockv1 + Radio_adstockv1 + TV_adstockv1, data = traindata)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-541505  -84037   20665  125746  379749 
#
#Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            2972682      32624   91.12  < 2e-16 ***
#PaymentType_Prepaid     170041      47439    3.58  0.00108 ** 
#Disc_Category_Low       627362      74385    8.43  9.6e-10 ***
#Disc_Category_Medium    582516      54491   10.69  3.0e-12 ***
#Disc_Category_High      148891      55243    2.70  0.01098 *  
#ListPrice                77976      43787    1.78  0.08415 .  
#Digital_adstockv1      -196367      61141   -3.21  0.00294 ** 
#Sponsorship_adstockv1   248485      68413    3.63  0.00094 ***
#Radio_adstockv1         106735      43761    2.44  0.02027 *  
#TV_adstockv1           -162259      52955   -3.06  0.00433 ** 
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 209000 on 33 degrees of freedom
#Multiple R-squared:  0.981,	Adjusted R-squared:  0.975 
#F-statistic:  185 on 9 and 33 DF,  p-value: <2e-16
#-----------------------GamingAccessory Additive MOdel-------------------------#



#-----------------------GamingAccessory Mutiplicative MOdel-------------------------#
set.seed(111)
GamingAccessory_WeeklyDatav2_ln<-GamingAccessory_WeeklyDatav2[,-c(3:4,7:9,16:19,20,22:26)]
GamingAccessory_WeeklyDatav2_ln<-cbind(log(GamingAccessory_WeeklyDatav2_ln[,-11]),GamingAccessory_WeeklyDatav2_ln[,11])

colnames(GamingAccessory_WeeklyDatav2_ln)[which(names(GamingAccessory_WeeklyDatav2_ln) == "GamingAccessory_WeeklyDatav2_ln[, 11]")] <- "Dummy_SpecialSale"
#Fix Nan and Infs

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
GamingAccessory_WeeklyDatav2_ln[is.nan(GamingAccessory_WeeklyDatav2_ln)] <- 0
which(GamingAccessory_WeeklyDatav2_ln==-Inf)
which(GamingAccessory_WeeklyDatav2_ln==Inf)
GamingAccessory_WeeklyDatav2_ln<-replace(GamingAccessory_WeeklyDatav2_ln,GamingAccessory_WeeklyDatav2_ln==-Inf,0)
GamingAccessory_WeeklyDatav2_ln<-na.omit(GamingAccessory_WeeklyDatav2_ln)

# Create row indices for train dataset
trainindices= sample(1:nrow(GamingAccessory_WeeklyDatav2_ln), 0.8*nrow(GamingAccessory_WeeklyDatav2_ln))
# Create the train data set
traindata = GamingAccessory_WeeklyDatav2_ln[trainindices,]
# Create test dataset
testdata = GamingAccessory_WeeklyDatav2_ln[-trainindices,]




#Model_1 
log_ga_model1<-lm(gmv ~ .,na.action = na.exclude, data = traindata)
summary(log_ga_model1) 
#Check StepAIC
library(MASS)
step<-stepAIC(log_ga_model1, direction="both")
step
#Model2 from StepAIC output
summary(log_ga_model1)

log_ga_model2<-lm(formula = gmv ~ UniqueItemQty + ListPricevsLastWeek_InflationPrcnt + 
                    ContentMarketing_adstockv1 + OnlineMarketing_adstockv1 + 
                    Affiliates_adstockv1 + Dummy_SpecialSale + Radio_adstockv1, 
                  data = traindata, na.action = na.exclude)



summary(log_ga_model2)
#Too high adjusted R-squared - 0.997
#Check VIF
as.data.frame(vif(log_ga_model2))

#Remove Affiliates_adstockv1
#Iterate the Process till a suitable model is found

#Model Iteration
log_ga_model<-  lm(formula = gmv ~ UniqueItemQty + ListPricevsLastWeek_InflationPrcnt + 
                     ContentMarketing_adstockv1 + 
                     #OnlineMarketing_adstockv1 + 
                     #Affiliates_adstockv1 + 
                     Dummy_SpecialSale #+ Radio_adstockv1, 
                   ,data = traindata, na.action = na.exclude)

#log_ga_model 
#plot(traindata$gmv,traindata$OnlineMarketing_adstockv1)
summary(log_ga_model)
#Too high adjusted R-squared - 0.971
#Check VIF
as.data.frame(vif(log_ga_model))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(log_ga_model,testdata)
testdata$test_gmv <- exp(Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.981
#Model R-squared: 0.957
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#0.484
# plot Residuals vs Fitted
plot(log_ga_model,pch=16,which=1,col="green")


#Seems random

#Try Bootstrapping

# bootstrapping with 1000 replications 
bootresults <- boot(data=GamingAccessory_WeeklyDatav2_ln, statistic=rsq, 
                    R=1000, formula = gmv ~ UniqueItemQty + ListPricevsLastWeek_InflationPrcnt + 
                      ContentMarketing_adstockv1 + 
                      Dummy_SpecialSale)

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#      original  bias    std. error
# t1*     0.994 -0.00028     0.00889

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ UniqueItemQty + ListPricevsLastWeek_InflationPrcnt + 
                       ContentMarketing_adstockv1 + 
                       Dummy_SpecialSale,   # model to fit
                     data = GamingAccessory_WeeklyDatav2_ln,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#   RMSE    Rsquared  MAE   
#  0.164  0.914     0.128 

#Final model result
summary(log_ga_model)
#Call:
#lm(formula = gmv ~ UniqueItemQty + ListPricevsLastWeek_InflationPrcnt + 
#    ContentMarketing_adstockv1 + Dummy_SpecialSale, data = traindata, 
#    na.action = na.exclude)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-0.4651 -0.0792  0.0051  0.0914  0.3725 
#
#Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                          6.2587     0.1386   45.15  < 2e-16 ***
#UniqueItemQty                        1.0660     0.0167   63.94  < 2e-16 ***
#ListPricevsLastWeek_InflationPrcnt   0.0786     0.0176    4.46  7.4e-05 ***
#ContentMarketing_adstockv1          -0.0483     0.0189   -2.56  0.01477 *  
#Dummy_SpecialSale                   -0.1986     0.0532   -3.73  0.00063 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.156 on 37 degrees of freedom
#Multiple R-squared:  0.992,	Adjusted R-squared:  0.991 
#F-statistic: 1.1e+03 on 4 and 37 DF,  p-value: <2e-16

#-----------------------GamingAccessory Mutiplicative MOdel-------------------------#


#-----------------------GamingAccessory KOYK MOdel-------------------------#
#set the seed to 100 
set.seed(123)
# Create row indices for train dataset
GamingAccessory_WeeklyDatav2_koyk<-GamingAccessory_WeeklyDatav2
GamingAccessory_WeeklyDatav2_koyk$gmv_lag1<- lag(GamingAccessory_WeeklyDatav1$gmv,1)

GamingAccessory_WeeklyDatav2_koykscaled<-cbind(scale(GamingAccessory_WeeklyDatav2_koyk[,c(-2,-21)]  , center = FALSE, scale = TRUE),GamingAccessory_WeeklyDatav2_koyk[,c(2,21)])
GamingAccessory_WeeklyDatav2_koykscaled<-na.omit(GamingAccessory_WeeklyDatav2_koykscaled)

trainindices= sample(1:nrow(GamingAccessory_WeeklyDatav2_koykscaled), 0.8*nrow(GamingAccessory_WeeklyDatav2_koykscaled))
# Create the train data set
traindata = GamingAccessory_WeeklyDatav2_koykscaled[trainindices,]
# Create test dataset
testdata = GamingAccessory_WeeklyDatav2_koykscaled[-trainindices,]

#Model_1 
model1<-lm(gmv ~ ., data = traindata)
summary(model1)  
#Check StepAIC
library(MASS)
step<-stepAIC(model1, direction="both")
step
#Model2 from StepAIC output
summary(koyk_ga_model1)

koyk_ga_model2<-lm(formula = gmv ~ Max_deliverybdays + Max_deliverycdays + Max_sla + 
                     Dist_pincode + Avg_product_mrp + PaymentType_COD + PaymentType_Prepaid + 
                     MarkUp_Prods + Disc_Category_Low + Disc_Category_Medium + 
                     Disc_Category_High + NPS_week + IndiaHolidays + ME_InternetUsers + 
                     Avg_frequency + ListPricevsLastWeek_InflationPrcnt + Sponsorship_adstockv1 + 
                     ContentMarketing_adstockv1 + OnlineMarketing_adstockv1 + 
                     SEM_adstockv1 + Radio_adstockv1 + Other_adstockv1 + TV_adstockv1 + 
                     gmv_lag1 + Dummy_Special_sale_week, data = traindata)




summary(koyk_ga_model2)
#Too high adjusted R-squared - 0.994
#Check VIF
as.data.frame(vif(koyk_ga_model2))

#Remove PaymentType_COD
#Iterate the Process till a suitable model is found

#Model Iteration
koyk_ga_model<-  lm(formula = gmv ~ Max_deliverybdays + #Max_deliverycdays + 
                      #Max_sla + 
                      #Dist_pincode + 
                      #Avg_product_mrp + #PaymentType_COD + 
                      PaymentType_Prepaid + 
                      MarkUp_Prods + #Disc_Category_Low + Disc_Category_Medium + 
                      #Disc_Category_High + 
                      NPS_week + IndiaHolidays + #ME_InternetUsers + 
                      #Avg_frequency + 
                      #ListPricevsLastWeek_InflationPrcnt + 
                      #Sponsorship_adstockv1 + 
                      #ContentMarketing_adstockv1 + 
                      #OnlineMarketing_adstockv1 + 
                      #SEM_adstockv1 + 
                      #Radio_adstockv1 + 
                      Other_adstockv1 + TV_adstockv1 #+ 
                    #gmv_lag1 #+ Dummy_Special_sale_week
                    , data = traindata)

#log_ga_model 
#plot(traindata$gmv,traindata$OnlineMarketing_adstockv1)
summary(koyk_ga_model)

#Too high adjusted R-squared - 0.811
#Check VIF
as.data.frame(vif(koyk_ga_model))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(koyk_ga_model,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.434
#Model R-squared: 0.811
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#0.484
# plot Residuals vs Fitted
plot(model20,pch=16,which=1,col="green")


#Seems random
#Try Bootstrapping

# bootstrapping with 1000 replications 
bootresults <- boot(data=GamingAccessory_WeeklyDatav2_ln, statistic=rsq, 
                    R=1000, formula = gmv ~ UniqueItemQty + ListPricevsLastWeek_InflationPrcnt + 
                      ContentMarketing_adstockv1 + 
                      Dummy_SpecialSale)

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#      original  bias    std. error
# t1*     0.994 -0.00028     0.00889

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ UniqueItemQty + ListPricevsLastWeek_InflationPrcnt + 
                       ContentMarketing_adstockv1 + 
                       Dummy_SpecialSale,   # model to fit
                     data = GamingAccessory_WeeklyDatav2_ln,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#   RMSE    Rsquared  MAE   
#  0.164  0.914     0.128 

#Final model result
summary(log_ga_model)
#Call:
#lm(formula = gmv ~ UniqueItemQty + ListPricevsLastWeek_InflationPrcnt + 
#    ContentMarketing_adstockv1 + Dummy_SpecialSale, data = traindata, 
#    na.action = na.exclude)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-0.4651 -0.0792  0.0051  0.0914  0.3725 
#
#Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                          6.2587     0.1386   45.15  < 2e-16 ***
#UniqueItemQty                        1.0660     0.0167   63.94  < 2e-16 ***
#ListPricevsLastWeek_InflationPrcnt   0.0786     0.0176    4.46  7.4e-05 ***
#ContentMarketing_adstockv1          -0.0483     0.0189   -2.56  0.01477 *  
#Dummy_SpecialSale                   -0.1986     0.0532   -3.73  0.00063 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.156 on 37 degrees of freedom
#Multiple R-squared:  0.992,	Adjusted R-squared:  0.991 
#F-statistic: 1.1e+03 on 4 and 37 DF,  p-value: <2e-16

#-----------------------GamingAccessory Mutiplicative MOdel-------------------------#