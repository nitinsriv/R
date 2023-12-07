#The source xlsx file was pre-processed to csv
#MoreInfo.csv contains data like IndiaHolidays, NPS, Special Sale Week etc
#ProductList.csv contains product list with te frequency of ads aired and the contribution rates
#invest.csv contains investment data in Crs
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

setwd("C:/Users/karki/Documents/upgrad/capstone/data")
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
#We have the percent investment for each sub-category which will be used to calculate from the total investment rates given
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

#----------------------------------------GamingAccessory--------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
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
GamingAccessory_WeeklyDatav2_scaled<-na.omit(GamingAccessory_WeeklyDatav2_scaled)

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
model1<-lm(gmv ~ ., data = traindata,na.action = na.exclude)
summary(model1)  
#Check StepAIC
library(MASS)
step<-stepAIC(model1, direction="both")
step
#Model2 from StepAIC output
model2<-lm(formula = gmv ~ Max_deliverybdays + Max_deliverycdays + Max_sla + 
             Dist_CustIds + PaymentType_COD + PaymentType_Prepaid + PremiumProduct + 
             MarkUp_Prods + MarkDown_Prods + Disc_Category_Low + Disc_Category_Medium + 
             ListPrice + ME_InternetUsers + Avg_frequency + ListPricevsLastWeek_InflationPrcnt + 
             TV_adstockv1 + Sponsorship_adstockv1 + ContentMarketing_adstockv1 + 
             OnlineMarketing_adstockv1 + Radio_adstockv1 + Other_adstockv1, 
           data = traindata, na.action = na.exclude)
summary(model2)
#Adjusted R-squared - 0.9974
#Check VIF
as.data.frame(vif(model2))
#Remove PaymentType_COD

#Iterate the Process till a suitable model is found

#Model Iteration
ga_model<-  lm(formula = gmv ~ #Max_deliverybdays + #Max_deliverycdays + 
                     #Max_sla + Avg_frequency +
                     #Dist_CustIds + #PaymentType_COD + MarkDown_Prods + 
                     PaymentType_Prepaid + PremiumProduct + 
                     #MarkUp_Prods + #Disc_Category_Low + #Disc_Category_Medium + 
                     #ListPrice + #ME_InternetUsers + 
                     #ListPricevsLastWeek_InflationPrcnt + 
                     TV_adstockv1 + #Sponsorship_adstockv1 + 
                     #ContentMarketing_adstockv1 + 
                     OnlineMarketing_adstockv1 #+ Radio_adstockv1 #+ Other_adstockv1
                   ,data = traindata, na.action = na.exclude)

#ga_model 

summary(ga_model)
#Too high adjusted R-squared - 0.971
#Check VIF
as.data.frame(vif(ga_model))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(ga_model,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.718319
#Model R-squared: 0.7242
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#947595.3
# plot Residuals vs Fitted
plot(ga_model,pch=16,which=1,col="green")


#Try Bootstrapping
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 
# bootstrapping with 1000 replications 
bootresults <- boot(data=GamingAccessory_WeeklyDatav2_scaled, statistic=rsq, 
                    R=1000, formula = gmv ~ PaymentType_Prepaid + 
                      PremiumProduct + 
                      TV_adstockv1 + 
                      OnlineMarketing_adstockv1)

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#       original     bias         std. error
# t1*   0.7264465   0.009647546  0.08701722

# get 95% confidence interval 
boot.ci(bootresults, type="bca")
library(caret)
#Try Cross Validation too
data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ PaymentType_Prepaid + 
                       PremiumProduct + 
                       TV_adstockv1 + 
                       OnlineMarketing_adstockv1,   # model to fit
                     data = GamingAccessory_WeeklyDatav2_scaled,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#   RMSE    Rsquared  MAE   
#  863493.1  0.6315794  710072.7   
#Final Model Summary
summary(ga_model)  
#Call:
#lm(formula = gmv ~ PaymentType_Prepaid + PremiumProduct + TV_adstockv1 + 
#    OnlineMarketing_adstockv1, data = traindata, na.action = na.exclude)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1272390  -390285   -26869   456517  1021641 
#
#Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                1128808     248049   4.551 5.60e-05 ***
#PaymentType_Prepaid         903597     192001   4.706 3.48e-05 ***
#PremiumProduct              504502     114990   4.387 9.18e-05 ***
#TV_adstockv1               -898083     356930  -2.516   0.0163 *  
#OnlineMarketing_adstockv1  1844703     364352   5.063 1.16e-05 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 651000 on 37 degrees of freedom
#Multiple R-squared:  0.7511,	Adjusted R-squared:  0.7242 
#F-statistic: 27.91 on 4 and 37 DF,  p-value: 9.987e-11
#-----------------------GamingAccessory Additive MOdel-------------------------#



#-----------------------GamingAccessory Mutiplicative MOdel-------------------------#
GamingAccessory_WeeklyDatav2_ln<-GamingAccessory_WeeklyDatav2[,-c(3:4,7:9,16:19,20,22:26)]
GamingAccessory_WeeklyDatav2_ln<-cbind(log(GamingAccessory_WeeklyDatav2_ln[,-11]),GamingAccessory_WeeklyDatav2_ln[,11])

colnames(GamingAccessory_WeeklyDatav2_ln)[which(names(GamingAccessory_WeeklyDatav2_ln) == "GamingAccessory_WeeklyDatav2_ln[, 11]")] <- "Dummy_SpecialSale"
#Fix Nan and Infs

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
GamingAccessory_WeeklyDatav2_ln[is.nan(GamingAccessory_WeeklyDatav2_ln)] <- 1
which(GamingAccessory_WeeklyDatav2_ln==-Inf)
which(GamingAccessory_WeeklyDatav2_ln==Inf)
GamingAccessory_WeeklyDatav2_ln<-replace(GamingAccessory_WeeklyDatav2_ln,GamingAccessory_WeeklyDatav2_ln==-Inf,1)
GamingAccessory_WeeklyDatav2_ln<-na.omit(GamingAccessory_WeeklyDatav2_ln)

# Create row indices for train dataset
trainindices= sample(1:nrow(GamingAccessory_WeeklyDatav2_ln), 0.8*nrow(GamingAccessory_WeeklyDatav2_ln))
# Create the train data set
traindata = GamingAccessory_WeeklyDatav2_ln[trainindices,]
# Create test dataset
testdata = GamingAccessory_WeeklyDatav2_ln[-trainindices,]




#Model_1 
ln_ga_model1<-lm(gmv ~ .,na.action = na.exclude, data = traindata)
summary(ln_ga_model1) 
#Check StepAIC

step<-stepAIC(ln_ga_model1, direction="both")
step
#Model2 from StepAIC output

ln_ga_model2<-lm(formula = gmv ~ UniqueItemQty + Max_sla + PaymentType_COD + 
                    PaymentType_Prepaid + PremiumProduct + MassProduct + MarkUp_Prods + 
                    MarkDown_Prods + ListPricevsLastWeek_InflationPrcnt + Sponsorship_adstockv1 + 
                    ContentMarketing_adstockv1 + OnlineMarketing_adstockv1 + 
                    Other_adstockv1 + Dummy_SpecialSale, data = traindata, na.action = na.exclude)



summary(ln_ga_model2)
#Too high adjusted R-squared - 0.997
#Check VIF
as.data.frame(vif(ln_ga_model2))

#Remove MassProduct
#Iterate the Process till a suitable model is found

#Model Iteration
ln_ga_model<-  lm(formula = gmv ~ #UniqueItemQty + 
                    #Max_sla + #PaymentType_COD + 
                    PaymentType_Prepaid + 
                    PremiumProduct + #MassProduct + MarkDown_Prods +
                    #MarkUp_Prods + 
                    #Sponsorship_adstockv1 + 
                    #ListPricevsLastWeek_InflationPrcnt + 
                    #ContentMarketing_adstockv1 + 
                    OnlineMarketing_adstockv1 
                    #Other_adstockv1 
                    #+ Dummy_SpecialSale
                  , data = traindata, na.action = na.exclude)

#log_ga_model 
#plot(traindata$gmv,traindata$OnlineMarketing_adstockv1)
summary(ln_ga_model)
#Adjusted R-squared - 0.9548
#Check VIF
as.data.frame(vif(ln_ga_model))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(ln_ga_model,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.864
#Model R-squared: 0.957
#quite low let's check by bootstrapping and CV
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#0.658
# plot Residuals vs Fitted
plot(ln_ga_model,pch=16,which=1,col="green")


#Seems random

#Try Bootstrapping

# bootstrapping with 1000 replications 
bootresults <- boot(data=GamingAccessory_WeeklyDatav2_ln, statistic=rsq, 
                    R=1000, formula = gmv ~ 
                      PaymentType_Prepaid + 
                      PremiumProduct +
                      OnlineMarketing_adstockv1 )

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#      original  bias    std. error
# t1*   0.9423862 -0.001866553  0.04837164

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ 
                       PaymentType_Prepaid + 
                       PremiumProduct +
                       OnlineMarketing_adstockv1 ,   # model to fit
                     data = GamingAccessory_WeeklyDatav2_ln,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#    RMSE    Rsquared  MAE   
#  0.5464052  0.8001232  0.4023568

#Final model result
summary(ln_ga_model)
#Call:
#lm(formula = gmv ~ PaymentType_Prepaid + PremiumProduct + OnlineMarketing_adstockv1, 
#    data = traindata, na.action = na.exclude)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1.13171 -0.15897  0.05235  0.25164  1.06149 
#
#Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                7.00574    0.30746  22.786  < 2e-16 ***
#PaymentType_Prepaid        0.72524    0.10908   6.649 7.38e-08 ***
#PremiumProduct             0.51685    0.18755   2.756  0.00894 ** 
#OnlineMarketing_adstockv1  0.26696    0.07964   3.352  0.00182 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.4767 on 38 degrees of freedom
#Multiple R-squared:  0.9581,	Adjusted R-squared:  0.9548 
#F-statistic: 289.4 on 3 and 38 DF,  p-value: < 2.2e-16

#-----------------------GamingAccessory Mutiplicative MOdel-------------------------#


#-----------------------GamingAccessory KOYK MOdel-------------------------#

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
model_koyk<-lm(gmv ~ ., data = traindata)
summary(model_koyk)  
#Check StepAIC
library(MASS)
step<-stepAIC(model_koyk, direction="both")
step
#Model2 from StepAIC output

koyk_ga_model2<-lm(formula = gmv ~ Max_deliverybdays + Max_deliverycdays + Max_sla + 
                     Dist_CustIds + Avg_product_mrp + PaymentType_COD + PaymentType_Prepaid + 
                     PremiumProduct + MarkDown_Prods + Disc_Category_Low + Disc_Category_Medium + 
                     Disc_Category_High + ListPrice + NPS_week + IndiaHolidays + 
                     ME_InternetUsers + TV_adstockv1 + Digital_adstockv1 + Sponsorship_adstockv1 + 
                     ContentMarketing_adstockv1 + OnlineMarketing_adstockv1 + 
                     Radio_adstockv1 + Other_adstockv1 + gmv_lag1 + Dummy_Special_sale_week, 
                   data = traindata)



summary(koyk_ga_model2)
#Too high adjusted R-squared - 0.993
#Check VIF
as.data.frame(vif(koyk_ga_model2))

#Remove PaymentType_COD
#Iterate the Process till a suitable model is found

#Model Iteration
koyk_ga_model<- lm(formula = gmv ~ #Max_deliverybdays + #Max_deliverycdays + 
                                      Max_sla + 
                                      #Avg_product_mrp + #PaymentType_COD + Dist_CustIds +
                                      PaymentType_Prepaid + 
                                      PremiumProduct + #MarkDown_Prods + 
                                      #Disc_Category_Low + Disc_Category_Medium + 
                                      #Disc_Category_High + ListPrice + 
                                      #NPS_week + #IndiaHolidays + 
                                      #ME_InternetUsers + 
                                      TV_adstockv1 + #Digital_adstockv1 + #Sponsorship_adstockv1 + 
                                      #ContentMarketing_adstockv1 + 
                                      OnlineMarketing_adstockv1 + 
                                      #Radio_adstockv1 + 
                                      Other_adstockv1 #+ gmv_lag1 #+ Dummy_Special_sale_week, 
                                    ,data = traindata)

#log_ga_model 
#plot(traindata$gmv,traindata$OnlineMarketing_adstockv1)
summary(koyk_ga_model)

#Adjusted R-squared - 0.8438
#Check VIF
as.data.frame(vif(koyk_ga_model))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(koyk_ga_model,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.44
#Model R-squared: 0.853

# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#898866.9
# plot Residuals vs Fitted
plot(koyk_ga_model,pch=16,which=1,col="green")


#Seems random
#Try Bootstrapping

# bootstrapping with 1000 replications 
bootresults <- boot(data=GamingAccessory_WeeklyDatav2_koykscaled, statistic=rsq, 
                    R=1000, formula = gmv ~ 
                      Max_sla + 
                      PaymentType_Prepaid + 
                      PremiumProduct +
                      TV_adstockv1 +
                      OnlineMarketing_adstockv1 + 
                      Other_adstockv1)

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#      original  bias    std. error
# t1*   0.8069994 0.01280667   0.0684179

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ 
                       Max_sla + 
                       PaymentType_Prepaid + 
                       PremiumProduct +
                       TV_adstockv1 +
                       OnlineMarketing_adstockv1 + 
                       Other_adstockv1,   # model to fit
                     data = GamingAccessory_WeeklyDatav2_koykscaled,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#     RMSE    Rsquared  MAE   
#  710121.3  0.6300467  573417.6

#Final model result
summary(koyk_ga_model)
#Call:
#lm(formula = gmv ~ Max_sla + PaymentType_Prepaid + PremiumProduct + 
#    TV_adstockv1 + OnlineMarketing_adstockv1 + Other_adstockv1, 
#    data = traindata)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1103911  -272994    20053   371890  1322056 
#
#Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
#(Intercept)               -1156398     580570  -1.992 0.054237 .  
#Max_sla                    2501274     696744   3.590 0.001003 ** 
#PaymentType_Prepaid         897841     165716   5.418 4.52e-06 ***
#PremiumProduct              505486     102819   4.916 2.07e-05 ***
#TV_adstockv1              -1157451     316328  -3.659 0.000827 ***
#OnlineMarketing_adstockv1  1743138     341512   5.104 1.17e-05 ***
#Other_adstockv1             471373     146812   3.211 0.002836 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 578600 on 35 degrees of freedom
#Multiple R-squared:  0.8667,	Adjusted R-squared:  0.8438 
#F-statistic: 37.92 on 6 and 35 DF,  p-value: 6.675e-14


#-----------------------GamingAccessory KOYK MOdel-------------------------#

#-----------------------GamingAccessory Distributed Lag MOdel-------------------------#
str(GamingAccessory_WeeklyDatav2_distlag) 
# Create row indices for train dataset
GamingAccessory_WeeklyDatav2_distlag<-GamingAccessory_WeeklyDatav2[,-c(3:7,8,9,16:19,20,23:26)]
GamingAccessory_WeeklyDatav2_distlag<-cbind(GamingAccessory_WeeklyDatav2_distlag,GamingAccessory_WeeklyDatav1[,c(60:61)])

#Calculate lag for Distributed lag model
GamingAccessory_WeeklyDatav2_distlag$gmv_lastweek1_lag=lag(GamingAccessory_WeeklyDatav2_distlag$gmv,1) 
GamingAccessory_WeeklyDatav2_distlag$gmv_lastweek2_lag=lag(GamingAccessory_WeeklyDatav2_distlag$gmv,2) 
GamingAccessory_WeeklyDatav2_distlag$gmv_lastweek3_lag=lag(GamingAccessory_WeeklyDatav2_distlag$gmv,3) 

GamingAccessory_WeeklyDatav2_distlag$PaymentType_COD_lastweek1_lag=lag(GamingAccessory_WeeklyDatav2_distlag$PaymentType_COD,1) 
GamingAccessory_WeeklyDatav2_distlag$PaymentType_COD_lastweek2_lag=lag(GamingAccessory_WeeklyDatav2_distlag$PaymentType_COD,2) 

GamingAccessory_WeeklyDatav2_distlag$PaymentType_Prepaid_lastweek1_lag=lag(GamingAccessory_WeeklyDatav2_distlag$PaymentType_Prepaid,1) 
GamingAccessory_WeeklyDatav2_distlag$PaymentType_Prepaid_lastweek2_lag=lag(GamingAccessory_WeeklyDatav2_distlag$PaymentType_Prepaid,2) 

GamingAccessory_WeeklyDatav2_distlag$PremiumProduct_lastweek1_lag=lag(GamingAccessory_WeeklyDatav2_distlag$PremiumProduct,1) 
GamingAccessory_WeeklyDatav2_distlag$PremiumProduct_lastweek2_lag=lag(GamingAccessory_WeeklyDatav2_distlag$PremiumProduct,2) 

GamingAccessory_WeeklyDatav2_distlag$MarkUp_Prods_lastweek1_lag=lag(GamingAccessory_WeeklyDatav2_distlag$MarkUp_Prods,1) 
GamingAccessory_WeeklyDatav2_distlag$MarkUp_Prods_lastweek2_lag=lag(GamingAccessory_WeeklyDatav2_distlag$MarkUp_Prods,2) 

GamingAccessory_WeeklyDatav2_distlag$MarkDown_Prods_lastweek1_lag=lag(GamingAccessory_WeeklyDatav2_distlag$MarkDown_Prods,1) 
GamingAccessory_WeeklyDatav2_distlag$MarkDown_Prods_lastweek2_lag=lag(GamingAccessory_WeeklyDatav2_distlag$MarkDown_Prods,2) 

GamingAccessory_WeeklyDatav2_distlag$TV_adstockv1_lastweek1_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$TV_adstockv1,1) 
GamingAccessory_WeeklyDatav2_distlag$TV_adstockv1_lastweek2_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$TV_adstockv1,2) 

GamingAccessory_WeeklyDatav2_distlag$Digital_adstockv1_lastweek1_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$Digital_adstockv1,1) 
GamingAccessory_WeeklyDatav2_distlag$Digital_adstockv1_lastweek2_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$Digital_adstockv1,2) 

GamingAccessory_WeeklyDatav2_distlag$Sponsorship_adstockv1_lastweek1_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$Sponsorship_adstockv1,1) 
GamingAccessory_WeeklyDatav2_distlag$Sponsorship_adstockv1_lastweek2_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$Sponsorship_adstockv1,2) 

GamingAccessory_WeeklyDatav2_distlag$ContentMarketing_adstockv1_lastweek1_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$ContentMarketing_adstockv1,1) 
GamingAccessory_WeeklyDatav2_distlag$ContentMarketing_adstockv1_lastweek2_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$ContentMarketing_adstockv1,2) 

GamingAccessory_WeeklyDatav2_distlag$OnlineMarketing_adstockv1_lastweek1_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$OnlineMarketing_adstockv1,1) 
GamingAccessory_WeeklyDatav2_distlag$OnlineMarketing_adstockv1_lastweek2_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$OnlineMarketing_adstockv1,2) 

GamingAccessory_WeeklyDatav2_distlag$Affiliates_adstockv1_lastweek1_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$Affiliates_adstockv1,1) 
GamingAccessory_WeeklyDatav2_distlag$Affiliates_adstockv1_lastweek2_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$Affiliates_adstockv1,2) 

GamingAccessory_WeeklyDatav2_distlag$SEM_adstockv1_lastweek1_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$SEM_adstockv1,1) 
GamingAccessory_WeeklyDatav2_distlag$SEM_adstockv1_lastweek2_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$SEM_adstockv1,2) 

GamingAccessory_WeeklyDatav2_distlag$Radio_adstockv1_lastweek1_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$Radio_adstockv1,1) 
GamingAccessory_WeeklyDatav2_distlag$Radio_adstockv1_lastweek2_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$Radio_adstockv1,2) 

GamingAccessory_WeeklyDatav2_distlag$Other_adstockv1_lastweek1_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$Other_adstockv1,1) 
GamingAccessory_WeeklyDatav2_distlag$Other_adstockv1_lastweek2_lag=stats::lag(GamingAccessory_WeeklyDatav2_distlag$Other_adstockv1,2) 



GamingAccessory_WeeklyDatav2_distlag_scaled<-cbind(scale(GamingAccessory_WeeklyDatav2_distlag[,c(-2,-9)]  , center = FALSE, scale = TRUE),GamingAccessory_WeeklyDatav2_distlag[,c(2,9)])
GamingAccessory_WeeklyDatav2_distlag_scaled<-na.omit(GamingAccessory_WeeklyDatav2_distlag_scaled)
GamingAccessory_WeeklyDatav2_distlag_scaled<-na.omit(GamingAccessory_WeeklyDatav2_distlag_scaled)
trainindices= sample(1:nrow(GamingAccessory_WeeklyDatav2_distlag_scaled), 0.8*nrow(GamingAccessory_WeeklyDatav2_distlag_scaled))
# Create the train data set
traindata = GamingAccessory_WeeklyDatav2_distlag_scaled[trainindices,]
# Create test dataset
testdata = GamingAccessory_WeeklyDatav2_distlag_scaled[-trainindices,]

#Model_1 
model_dlag<-lm(gmv ~ ., data = traindata)
summary(model_dlag)  
#Check StepAIC
library(MASS)
step<-stepAIC(model_dlag, direction="both")
step
#Model2 from StepAIC output

model_dlag2<-lm(formula = gmv ~ UniqueItemQty + PaymentType_Prepaid + PremiumProduct + 
                     MarkDown_Prods + NPS_week + ListPricevsLastWeek_InflationPrcnt + 
                     TV_adstockv1 + Sponsorship_adstockv1 + ContentMarketing_adstockv1 + 
                     OnlineMarketing_adstockv1 + Affiliates_adstockv1 + SEM_adstockv1 + 
                     Radio_adstockv1 + Other_adstockv1 + ListPricevsLast2Week_InflationPrcnt + 
                     gmv_lastweek2_lag + gmv_lastweek3_lag + PaymentType_COD_lastweek1_lag + 
                     PaymentType_COD_lastweek2_lag + PaymentType_Prepaid_lastweek2_lag + 
                     MarkUp_Prods_lastweek1_lag + Dummy_Special_sale_week, data = traindata)



summary(model_dlag2)
#Too high adjusted R-squared - 0.9945
#Check VIF
as.data.frame(vif(model_dlag2))

#Remove UniqueItemQty
#Iterate the Process till a suitable model is found

#Model Iteration
model_dlag<- lm(formula = gmv ~ #UniqueItemQty + 
                  PaymentType_Prepaid + PremiumProduct + 
                  #MarkDown_Prods + 
                  #NPS_week + #ListPricevsLastWeek_InflationPrcnt + 
                  TV_adstockv1 + Sponsorship_adstockv1 + ContentMarketing_adstockv1 + 
                  #OnlineMarketing_adstockv1 + 
                  Affiliates_adstockv1 + #SEM_adstockv1 + 
                  Radio_adstockv1 + #Other_adstockv1 + 
                  #ListPricevsLast2Week_InflationPrcnt + 
                  #gmv_lastweek2_lag + 
                  #gmv_lastweek3_lag + 
                  #PaymentType_COD_lastweek1_lag + 
                  PaymentType_COD_lastweek2_lag #+ #PaymentType_Prepaid_lastweek2_lag + 
                  #MarkUp_Prods_lastweek1_lag + 
                  #Dummy_Special_sale_week
                , data = traindata)

#log_ga_model 
#plot(traindata$gmv,traindata$OnlineMarketing_adstockv1)
summary(model_dlag)

#Adjusted R-squared - 0.8721
#Check VIF
as.data.frame(vif(model_dlag))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(model_dlag,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.354
#Model R-squared: 0.87
#Will check using cross validation
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#898866.9
# plot Residuals vs Fitted
plot(model_dlag,pch=16,which=1,col="green")


#Seems random
#Try Bootstrapping

# bootstrapping with 1000 replications 
bootresults <- boot(data=GamingAccessory_WeeklyDatav2_distlag_scaled, statistic=rsq, 
                    R=1000, formula = gmv ~ 
                      PaymentType_Prepaid + PremiumProduct + 
                      TV_adstockv1 + Sponsorship_adstockv1 + ContentMarketing_adstockv1 + 
                      Affiliates_adstockv1 + 
                      Radio_adstockv1 + 
                      PaymentType_COD_lastweek2_lag 
)

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#      original  bias    std. error
# t1*  0.8383482 0.01904863  0.05390948

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ 
                       PaymentType_Prepaid + PremiumProduct + 
                       TV_adstockv1 + Sponsorship_adstockv1 + ContentMarketing_adstockv1 + 
                       Affiliates_adstockv1 + 
                       Radio_adstockv1 + 
                       PaymentType_COD_lastweek2_lag ,   # model to fit
                     data = GamingAccessory_WeeklyDatav2_distlag_scaled,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#     RMSE    Rsquared  MAE   
#   715527.4  0.7191597  586402.1  

#Final model result
summary(model_dlag)
#Call:
#lm(formula = gmv ~ PaymentType_Prepaid + PremiumProduct + TV_adstockv1 + 
#    Sponsorship_adstockv1 + ContentMarketing_adstockv1 + Affiliates_adstockv1 + 
#    Radio_adstockv1 + PaymentType_COD_lastweek2_lag, data = traindata)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1153106  -318281    48353   282533  1121523 
#
#Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                    -278451     259482  -1.073 0.291507    
#PaymentType_Prepaid             975470     183473   5.317 8.66e-06 ***
#PremiumProduct                  563991     111997   5.036 1.94e-05 ***
#TV_adstockv1                  -1262662     282023  -4.477 9.55e-05 ***
#Sponsorship_adstockv1          1168553     291855   4.004 0.000361 ***
#ContentMarketing_adstockv1     -795311     213505  -3.725 0.000779 ***
#Affiliates_adstockv1           2418226     369166   6.551 2.59e-07 ***
#Radio_adstockv1                 403243     142214   2.835 0.007987 ** 
#PaymentType_COD_lastweek2_lag   536309     174774   3.069 0.004441 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 523700 on 31 degrees of freedom
#Multiple R-squared:  0.8984,	Adjusted R-squared:  0.8721 
#F-statistic: 34.25 on 8 and 31 DF,  p-value: 2.68e-13

#-----------------------GamingAccessory Distributed Lag MOdel-------------------------#


#-----------------------GamingAccessory Mutiplicative Distributed Lag MOdel-------------------------#
str(GamingAccessory_WeeklyDatav2_distlag_ln)
GamingAccessory_WeeklyDatav2_distlag_ln<-cbind(log(GamingAccessory_WeeklyDatav2_distlag[,-9]),GamingAccessory_WeeklyDatav2_distlag[,9])

colnames(GamingAccessory_WeeklyDatav2_distlag_ln)[which(names(GamingAccessory_WeeklyDatav2_distlag_ln) == "GamingAccessory_WeeklyDatav2_distlag[, 9]")] <- "Dummy_SpecialSale"
#Fix Nan and Infs

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
GamingAccessory_WeeklyDatav2_distlag_ln[is.nan(GamingAccessory_WeeklyDatav2_distlag_ln)] <- 1
which(GamingAccessory_WeeklyDatav2_distlag_ln==-Inf)
which(GamingAccessory_WeeklyDatav2_distlag_ln==Inf)
GamingAccessory_WeeklyDatav2_distlag_ln<-replace(GamingAccessory_WeeklyDatav2_distlag_ln,GamingAccessory_WeeklyDatav2_distlag_ln==-Inf,1)
GamingAccessory_WeeklyDatav2_distlag_ln<-na.omit(GamingAccessory_WeeklyDatav2_distlag_ln)

# Create row indices for train dataset
trainindices= sample(1:nrow(GamingAccessory_WeeklyDatav2_distlag_ln), 0.8*nrow(GamingAccessory_WeeklyDatav2_distlag_ln))
# Create the train data set
traindata = GamingAccessory_WeeklyDatav2_distlag_ln[trainindices,]
# Create test dataset
testdata = GamingAccessory_WeeklyDatav2_distlag_ln[-trainindices,]




#Model_1 
Dlagln_ga_model1<-lm(gmv ~ .,na.action = na.exclude, data = traindata)
summary(Dlagln_ga_model1) 
#Check StepAIC

step<-stepAIC(Dlagln_ga_model1, direction="both")
step
#Model2 from StepAIC output

Dlagln_ga_model2<-lm(formula = gmv ~ UniqueItemQty + PaymentType_Prepaid + PremiumProduct + 
                   MarkUp_Prods + MarkDown_Prods + ListPricevsLastWeek_InflationPrcnt + 
                   TV_adstockv1 + ContentMarketing_adstockv1 + OnlineMarketing_adstockv1 + 
                   Affiliates_adstockv1 + SEM_adstockv1 + Radio_adstockv1 + 
                   Other_adstockv1 + ListPricevsLast2Week_InflationPrcnt + gmv_lastweek1_lag + 
                   gmv_lastweek2_lag + gmv_lastweek3_lag + PaymentType_COD_lastweek1_lag + 
                   PaymentType_COD_lastweek2_lag + PremiumProduct_lastweek1_lag + 
                   MarkUp_Prods_lastweek1_lag + MarkDown_Prods_lastweek2_lag + 
                   MarkDown_Prods_lastweek1_lag + Dummy_SpecialSale, data = traindata, 
                 na.action = na.exclude)



summary(Dlagln_ga_model2)
#Too high adjusted R-squared - 0.9989
#Check VIF
as.data.frame(vif(Dlagln_ga_model2))

#Remove MarkDown_Prods
#Iterate the Process till a suitable model is found

#Model Iteration
Dlagln_ga_model<-  lm(formula = gmv ~ #UniqueItemQty +
                        PaymentType_Prepaid + #PremiumProduct + 
                        #MarkUp_Prods + #MarkDown_Prods +
                        ListPricevsLastWeek_InflationPrcnt + 
                        #TV_adstockv1 + 
                       # ContentMarketing_adstockv1 + #OnlineMarketing_adstockv1 + 
                        #Affiliates_adstockv1 + #SEM_adstockv1 + 
                        Radio_adstockv1 + 
                        Other_adstockv1 + #ListPricevsLast2Week_InflationPrcnt + #gmv_lastweek1_lag + 
                        gmv_lastweek2_lag + #gmv_lastweek3_lag + 
                        PaymentType_COD_lastweek1_lag  
                        #PaymentType_COD_lastweek2_lag + 
                       # PremiumProduct_lastweek1_lag #+
                        #MarkUp_Prods_lastweek1_lag #+ #MarkDown_Prods_lastweek2_lag + 
                        #MarkDown_Prods_lastweek1_lag + 
                        #Dummy_SpecialSale
                      , data = traindata, 
                      na.action = na.exclude)

#Dlagln_ga_model 
#plot(traindata$gmv,traindata$OnlineMarketing_adstockv1)
summary(Dlagln_ga_model)
#Adjusted R-squared - 0.9548
#Check VIF
as.data.frame(vif(Dlagln_ga_model))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(Dlagln_ga_model,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.94
#Model R-squared: 0.95
#quite low let's check by bootstrapping and CV
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#0.658
# plot Residuals vs Fitted
plot(Dlagln_ga_model,pch=16,which=1,col="green")


#Seems random

#Try Bootstrapping

# bootstrapping with 1000 replications 
bootresults <- boot(data=GamingAccessory_WeeklyDatav2_distlag_ln, statistic=rsq, 
                    R=1000, formula = gmv ~ #UniqueItemQty +
                      PaymentType_Prepaid + #PremiumProduct + 
                      #MarkUp_Prods + #MarkDown_Prods +
                      ListPricevsLastWeek_InflationPrcnt + 
                      #TV_adstockv1 + 
                      # ContentMarketing_adstockv1 + #OnlineMarketing_adstockv1 + 
                      #Affiliates_adstockv1 + #SEM_adstockv1 + 
                      Radio_adstockv1 + 
                      Other_adstockv1 + #ListPricevsLast2Week_InflationPrcnt + #gmv_lastweek1_lag + 
                      gmv_lastweek2_lag + #gmv_lastweek3_lag + 
                      PaymentType_COD_lastweek1_lag  )

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#      original  bias    std. error
# t1*  0.9653673 0.001646941  0.04024856

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ #UniqueItemQty +
                       PaymentType_Prepaid + #PremiumProduct + 
                       #MarkUp_Prods + #MarkDown_Prods +
                       ListPricevsLastWeek_InflationPrcnt + 
                       #TV_adstockv1 + 
                       # ContentMarketing_adstockv1 + #OnlineMarketing_adstockv1 + 
                       #Affiliates_adstockv1 + #SEM_adstockv1 + 
                       Radio_adstockv1 + 
                       Other_adstockv1 + #ListPricevsLast2Week_InflationPrcnt + #gmv_lastweek1_lag + 
                       gmv_lastweek2_lag + #gmv_lastweek3_lag + 
                       PaymentType_COD_lastweek1_lag  ,   # model to fit
                     data = GamingAccessory_WeeklyDatav2_distlag_ln,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#    RMSE    Rsquared  MAE   
#  0.483105  0.7426866  0.3619723

#Final model result
summary(Dlagln_ga_model)
#Call:
#lm(formula = gmv ~ PaymentType_Prepaid + ListPricevsLastWeek_InflationPrcnt + 
#    Radio_adstockv1 + Other_adstockv1 + gmv_lastweek2_lag + PaymentType_COD_lastweek1_lag, 
#    data = traindata, na.action = na.exclude)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-0.89841 -0.17785  0.05662  0.18612  0.65606 
#
#Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                         5.15467    0.55301   9.321 9.13e-11 ***
#PaymentType_Prepaid                 0.97988    0.04754  20.612  < 2e-16 ***
#ListPricevsLastWeek_InflationPrcnt  0.16137    0.06495   2.484 0.018225 *  
#Radio_adstockv1                     0.24799    0.10242   2.421 0.021125 *  
#Other_adstockv1                    -0.14067    0.06270  -2.243 0.031696 *  
#gmv_lastweek2_lag                   0.10279    0.03955   2.599 0.013873 *  
#PaymentType_COD_lastweek1_lag       0.19915    0.05144   3.872 0.000484 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.3614 on 33 degrees of freedom
#Multiple R-squared:  0.9717,	Adjusted R-squared:  0.9665 
#F-statistic: 188.7 on 6 and 33 DF,  p-value: < 2.2e-16

#-----------------------GamingAccessory Mutiplicative Distributed Lag MOdel-------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#----------------------------------------GamingAccessory--------------------------------------------#



#----------------------------------------HomeAudio--------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#-----------------------HomeAudio Dataset Build-------------------------#
#Adding Weekly List Price inflation metrics till 4 weeks
HomeAudio_WeeklyDatav1<-as.data.frame(subset(OrderMain_Weeklyv2,OrderMain_Weeklyv2$product_analytic_sub_category.x=="HomeAudio"))
HomeAudio_WeeklyDatav1$ListPricevsLastWeek_value <- lag(HomeAudio_WeeklyDatav1$ListPrice,1)
HomeAudio_WeeklyDatav1$ListPricevsLast2Week_value <- lag(HomeAudio_WeeklyDatav1$ListPrice,2)
HomeAudio_WeeklyDatav1$ListPricevsLast3Week_value <- lag(HomeAudio_WeeklyDatav1$ListPrice,3)
HomeAudio_WeeklyDatav1$ListPricevsLast4Week_value <- lag(HomeAudio_WeeklyDatav1$ListPrice,4)
HomeAudio_WeeklyDatav1$ListPricevsLastWeek_value[which(is.na(HomeAudio_WeeklyDatav1$ListPricevsLastWeek_value))]<-0
HomeAudio_WeeklyDatav1$ListPricevsLast2Week_value[which(is.na(HomeAudio_WeeklyDatav1$ListPricevsLast2Week_value))]<-0
HomeAudio_WeeklyDatav1$ListPricevsLast3Week_value[which(is.na(HomeAudio_WeeklyDatav1$ListPricevsLast3Week_value))]<-0
HomeAudio_WeeklyDatav1$ListPricevsLast4Week_value[which(is.na(HomeAudio_WeeklyDatav1$ListPricevsLast4Week_value))]<-0

HomeAudio_WeeklyDatav1$ListPricevsLastWeek_InflationPrcnt<-((HomeAudio_WeeklyDatav1$ListPrice-HomeAudio_WeeklyDatav1$ListPricevsLastWeek_value)/HomeAudio_WeeklyDatav1$ListPricevsLastWeek_value)*100
HomeAudio_WeeklyDatav1$ListPricevsLast2Week_InflationPrcnt<-((HomeAudio_WeeklyDatav1$ListPrice-HomeAudio_WeeklyDatav1$ListPricevsLast2Week_value)/HomeAudio_WeeklyDatav1$ListPricevsLast2Week_value)*100
HomeAudio_WeeklyDatav1$ListPricevsLast3Week_InflationPrcnt<-((HomeAudio_WeeklyDatav1$ListPrice-HomeAudio_WeeklyDatav1$ListPricevsLast3Week_value)/HomeAudio_WeeklyDatav1$ListPricevsLast3Week_value)*100
HomeAudio_WeeklyDatav1$ListPricevsLast4Week_InflationPrcnt<-((HomeAudio_WeeklyDatav1$ListPrice-HomeAudio_WeeklyDatav1$ListPricevsLast4Week_value)/HomeAudio_WeeklyDatav1$ListPricevsLast4Week_value)*100

HomeAudio_WeeklyDatav1$ListPricevsLastWeek_InflationPrcnt[which(HomeAudio_WeeklyDatav1$ListPricevsLastWeek_InflationPrcnt=="Inf")]<-NA
HomeAudio_WeeklyDatav1$ListPricevsLast2Week_InflationPrcnt[which(HomeAudio_WeeklyDatav1$ListPricevsLast2Week_InflationPrcnt=="Inf")]<-NA
HomeAudio_WeeklyDatav1$ListPricevsLast3Week_InflationPrcnt[which(HomeAudio_WeeklyDatav1$ListPricevsLast3Week_InflationPrcnt=="Inf")]<-NA
HomeAudio_WeeklyDatav1$ListPricevsLast4Week_InflationPrcnt[which(HomeAudio_WeeklyDatav1$ListPricevsLast4Week_InflationPrcnt=="Inf")]<-NA
#Weekly Investment Calculation
#We are Calculating weekly investment using MOnthly Investment as explained below:
#Break Monthly investment to daily by diving MOnthly Investment by Number of Days in month
#Calculate Number of days in all the week
#Daily Investment * Number of Days in week * Percent_InvestmentIn_GameAccessory = Total investment for the week
HomeAudio_WeeklyDatav1$Weekly_TotalInvestment=Invest_HomeAudio*HomeAudio_WeeklyDatav1$Daily_TotalInvestment*HomeAudio_WeeklyDatav1$DaysInWeek
HomeAudio_WeeklyDatav1$Weekly_TV=Invest_HomeAudio*HomeAudio_WeeklyDatav1$Daily_TV*HomeAudio_WeeklyDatav1$DaysInWeek
HomeAudio_WeeklyDatav1$Weekly_Digital=Invest_HomeAudio*HomeAudio_WeeklyDatav1$Daily_Digital*HomeAudio_WeeklyDatav1$DaysInWeek
HomeAudio_WeeklyDatav1$Weekly_Sponsorship=Invest_HomeAudio*HomeAudio_WeeklyDatav1$Daily_Sponsorship*HomeAudio_WeeklyDatav1$DaysInWeek
HomeAudio_WeeklyDatav1$Weekly_ContentMarketing=Invest_HomeAudio*HomeAudio_WeeklyDatav1$Daily_ContentMarketing*HomeAudio_WeeklyDatav1$DaysInWeek
HomeAudio_WeeklyDatav1$Weekly_OnlineMarketing=Invest_HomeAudio*HomeAudio_WeeklyDatav1$Daily_OnlineMarketing*HomeAudio_WeeklyDatav1$DaysInWeek
HomeAudio_WeeklyDatav1$Weekly_Affiliates=Invest_HomeAudio*HomeAudio_WeeklyDatav1$Daily_Affiliates*HomeAudio_WeeklyDatav1$DaysInWeek
HomeAudio_WeeklyDatav1$Weekly_SEM=Invest_HomeAudio*HomeAudio_WeeklyDatav1$Daily_SEM*HomeAudio_WeeklyDatav1$DaysInWeek
HomeAudio_WeeklyDatav1$Weekly_Radio=Invest_HomeAudio*HomeAudio_WeeklyDatav1$Daily_Radio*HomeAudio_WeeklyDatav1$DaysInWeek
HomeAudio_WeeklyDatav1$Weekly_Other=Invest_HomeAudio*HomeAudio_WeeklyDatav1$Daily_Other*HomeAudio_WeeklyDatav1$DaysInWeek
#Add Id column 
HomeAudio_WeeklyDatav1$ID <- seq.int(nrow(HomeAudio_WeeklyDatav1))


#Plot GMV to check the trend
ggplot(HomeAudio_WeeklyDatav1)+labs(title = "Weekly GMV", x="Weeks", y="GMV") + 
  geom_bar(aes(x=ID, y=(gmv)),stat="identity", fill="#B2EBF2", colour="#E0F7FA")+theme_minimal()




#Plot GMV with Total Investments to check the trend
##Total Investments is in dotted line
ggplot(HomeAudio_WeeklyDatav1) + labs(title = "Total Investment vs GMV", x="Weeks", y="GMV & Total Investment") + 
  geom_bar(aes(x=ID, y=(gmv/10000)),stat="identity", fill="#B2EBF2", colour="#E0F7FA")+
  geom_line(aes(x=ID, y=Weekly_TotalInvestment),stat="identity",color="#9C27B0",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_TotalInvestment,color="#E64A19"))+theme_minimal()+theme(legend.position="none")

#Plot GMV with TV Investments to check the trend
##TV Investments are in dotted line
ggplot(HomeAudio_WeeklyDatav1) + labs(title = "TV Investment vs GMV", x="Weeks", y="GMV & TV Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_TV),stat="identity",color="#E64A19",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_TV,color="#E64A19"))+theme_minimal()+theme(legend.position="none")
#We consider that TV ad effect remains 5%  till 6th weeks, with percentage decay rates for 1-6 week given below:
#0.60	0.36	0.22	0.13	0.08	0.05 
HomeAudio_WeeklyDatav1$TV_adstockv1 <- stats::filter(HomeAudio_WeeklyDatav1$Weekly_TV,0.6,method="recursive")
ggplot(HomeAudio_WeeklyDatav1)  + labs(title = "TV: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_TV),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=TV_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Plot GMV with Digital Investments to check the trend
##Digital Investments are in dotted line
ggplot(HomeAudio_WeeklyDatav1) + labs(title = "Digital Investment vs GMV", x="Weeks", y="GMV & Digital Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Digital),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Digital,color="#006064"))+theme_minimal()+theme(legend.position="none")
#We consider that Digital ad effect remains 10%  till 1st week with percentage decay rate given below:
#0.1
HomeAudio_WeeklyDatav1$Digital_adstockv1 <- stats::filter(HomeAudio_WeeklyDatav1$Weekly_Digital,0.1,method="recursive")
ggplot(HomeAudio_WeeklyDatav1)  + labs(title = "Digital: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Digital),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Digital_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Plot GMV with Sponsorship Investments to check the trend
##Sponsorship Investments are in dotted line
ggplot(HomeAudio_WeeklyDatav1) + labs(title = "Sponsorship Investment vs GMV", x="Weeks", y="GMV & Sponsorship Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Sponsorship),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Sponsorship,color="#006064"))+theme_minimal()+theme(legend.position="none")
#We consider that Sponsorship ad effect remains 10%  till 4th week with percentage decay rates given below:
#0.4	0.40	0.16	0.06
HomeAudio_WeeklyDatav1$Sponsorship_adstockv1 <- stats::filter(HomeAudio_WeeklyDatav1$Weekly_Sponsorship,0.4,method="recursive")
ggplot(HomeAudio_WeeklyDatav1)  + labs(title = "Sponsorship: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Sponsorship),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Sponsorship_adstockv1),stat="identity",color="blue")+
  theme_minimal()


#Plot GMV with ContentMarketing Investments to check the trend
##ContentMarketing Investments are in dotted line
ggplot(HomeAudio_WeeklyDatav1) + labs(title = "ContentMarketing Investment vs GMV", x="Weeks", y="GMV & ContentMarketing Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_ContentMarketing),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_ContentMarketing,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that ContentMarketing ad effect remains 10%  till 4th week with percentage decay rates given below:
#0.4	0.40	0.16	0.06
HomeAudio_WeeklyDatav1$ContentMarketing_adstockv1 <- stats::filter(HomeAudio_WeeklyDatav1$Weekly_ContentMarketing,0.4,method="recursive")
ggplot(HomeAudio_WeeklyDatav1)  + labs(title = "ContentMarketing: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_ContentMarketing),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=ContentMarketing_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Plot GMV with OnlineMarketing Investments to check the trend
##OnlineMarketing Investments are in dotted line
ggplot(HomeAudio_WeeklyDatav1) + labs(title = "OnlineMarketing Investment vs GMV", x="Weeks", y="GMV & OnlineMarketing Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_OnlineMarketing),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_OnlineMarketing,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that OnlineMarketing ad effect remains 9%  till 2nd week with percentage decay rates given below:
#0.3  0.09
HomeAudio_WeeklyDatav1$OnlineMarketing_adstockv1 <- stats::filter(HomeAudio_WeeklyDatav1$Weekly_OnlineMarketing,0.3,method="recursive")
ggplot(HomeAudio_WeeklyDatav1)  + labs(title = "OnlineMarketing: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_OnlineMarketing),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=OnlineMarketing_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Plot GMV with Affiliates Investments to check the trend
##Affiliates Investments are in dotted line
ggplot(HomeAudio_WeeklyDatav1) + labs(title = "Affiliates Investment vs GMV", x="Weeks", y="GMV & Affiliates Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Affiliates),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Affiliates,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that Affiliates ad effect remains 6%  till 3rd week with percentage decay rates given below:
#0.40	0.16	0.06
HomeAudio_WeeklyDatav1$Affiliates_adstockv1 <- stats::filter(HomeAudio_WeeklyDatav1$Weekly_Affiliates,0.4,method="recursive")
ggplot(HomeAudio_WeeklyDatav1)  + labs(title = "Affiliates: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Affiliates),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Affiliates_adstockv1),stat="identity",color="blue")+
  theme_minimal()


#Plot GMV with SEM Investments to check the trend
##SEM Investments are in dotted line
ggplot(HomeAudio_WeeklyDatav1) + labs(title = "SEM Investment vs GMV", x="Weeks", y="GMV & SEM Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_SEM),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_SEM,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that SEM ad effect remains 9%  till 2nd week with percentage decay rates given below:
#0.3  0.09
HomeAudio_WeeklyDatav1$SEM_adstockv1 <- stats::filter(HomeAudio_WeeklyDatav1$Weekly_SEM,0.3,method="recursive")
ggplot(HomeAudio_WeeklyDatav1)  + labs(title = "SEM: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_SEM),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=SEM_adstockv1),stat="identity",color="blue")+
  theme_minimal()


#Plot GMV with Radio Investments to check the trend
##Radio Investments are in dotted line
ggplot(HomeAudio_WeeklyDatav1) + labs(title = "Radio Investment vs GMV", x="Weeks", y="GMV & Radio Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Radio),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Radio,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that Radio ad effect remains 6%  till 3rd week with percentage decay rates given below:
#0.40	0.16	0.06
HomeAudio_WeeklyDatav1$Radio_adstockv1 <- stats::filter(HomeAudio_WeeklyDatav1$Weekly_Radio,0.4,method="recursive")
ggplot(HomeAudio_WeeklyDatav1)  + labs(title = "Radio: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Radio),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Radio_adstockv1),stat="identity",color="blue")+
  theme_minimal()  


#Plot GMV with Other Investments to check the trend
##Other Investments are in dotted line
ggplot(HomeAudio_WeeklyDatav1) + labs(title = "Other Investment vs GMV", x="Weeks", y="GMV & Other Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Other),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Other,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that Other ad effect remains 9%  till 2nd week with percentage decay rates given below:
#0.3  0.09
HomeAudio_WeeklyDatav1$Other_adstockv1 <- stats::filter(HomeAudio_WeeklyDatav1$Weekly_Other,0.3,method="recursive")
ggplot(HomeAudio_WeeklyDatav1)  + labs(title = "Other: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Other),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Other_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Let us reduce the number of variables below 40 to create a dataset for regression
#Le us remove the unnecessaryvariables first
HomeAudio_WeeklyDatav2<-HomeAudio_WeeklyDatav1[,c(6:9,11:45,60,75:83)]
#We will take correlatio for determining the Avg, Mean and Max type derived KPI relevance

corrplot(cor(HomeAudio_WeeklyDatav2[,c(3,5:13,18:22)]),method="square")
#only the variables with good correlation will be retained
HomeAudio_WeeklyDatav2<-HomeAudio_WeeklyDatav2[,-c(5,6,8,9,11,12,19,21,22)]
#Let's check for the complete data frame
corrplot(cor(HomeAudio_WeeklyDatav2,use = "complete.obs"),method="square")
#Take a closer look at first 12 columns for very high co-relation
corrplot(cor(HomeAudio_WeeklyDatav2[,1:15],use = "complete.obs"),method="number")
#Remove Order Count, Total_custId, Total_PinCode,OrderQty for now
HomeAudio_WeeklyDatav2<-HomeAudio_WeeklyDatav2[,-c(1,4,9,11)]
#We will remove the rest using VIF

#Scale the quantitative variables 

HomeAudio_WeeklyDatav2_scaled<-cbind(scale(HomeAudio_WeeklyDatav2[,c(-2,-21)]  , center = FALSE, scale = TRUE),HomeAudio_WeeklyDatav2[,c(2,21)])
HomeAudio_WeeklyDatav2_scaled<-na.omit(HomeAudio_WeeklyDatav2_scaled)

#-----------------------HomeAudio Dataset Build-------------------------#

#-----------------------HomeAudio Additive MOdel-------------------------#
#set the seed to 100 
set.seed(123)
# Create row indices for train dataset
trainindices= sample(1:nrow(HomeAudio_WeeklyDatav2_scaled), 0.8*nrow(HomeAudio_WeeklyDatav2_scaled))
# Create the train data set
traindata = HomeAudio_WeeklyDatav2_scaled[trainindices,]
# Create test dataset
testdata = HomeAudio_WeeklyDatav2_scaled[-trainindices,]

#Model_1 
model1<-lm(gmv ~ ., data = traindata,na.action = na.exclude)
summary(model1)  
#Check StepAIC
library(MASS)
step<-stepAIC(model1, direction="both")
step
#Model2 from StepAIC output
model2<-lm(formula = gmv ~ UniqueItemQty + Max_deliverycdays + Max_sla + 
             Dist_CustIds + Dist_pincode + Avg_product_mrp + Max_product_procurement_sla + 
             PaymentType_COD + PaymentType_Prepaid + PremiumProduct + 
             MarkUp_Prods + MarkDown_Prods + Disc_Category_Low + Disc_Category_Medium + 
             Disc_Category_High + ListPrice + NPS_week + IndiaHolidays + 
             ME_InternetUsers + Avg_frequency + TV_adstockv1 + Digital_adstockv1 + 
             Sponsorship_adstockv1 + ContentMarketing_adstockv1 + OnlineMarketing_adstockv1 + 
             Affiliates_adstockv1 + SEM_adstockv1 + Radio_adstockv1 + 
             Other_adstockv1 + Dummy_Special_sale_week, data = traindata, 
           na.action = na.exclude)

summary(model2)
#Adjusted R-squared - 0.9995
#Check VIF
as.data.frame(vif(model2))
#Remove PaymentType_COD

#Iterate the Process till a suitable model is found

#Model Iteration
ha_model<-  lm(formula = gmv ~ #UniqueItemQty + 
                 #Max_deliverycdays +
                 Max_sla + 
                 #Dist_CustIds + 
                 #Dist_pincode + 
                 #Avg_product_mrp + 
                 #Max_product_procurement_sla + 
                 #PaymentType_COD + 
                 #PaymentType_Prepaid + 
                 PremiumProduct + 
                 #MarkUp_Prods + 
                 #MarkDown_Prods + 
                 #Disc_Category_Low + Disc_Category_Medium + 
                 #Disc_Category_High + ListPrice +# NPS_week + 
                 #IndiaHolidays + 
                 #ME_InternetUsers + #Avg_frequency + 
                 #TV_adstockv1 + 
                 #Digital_adstockv1 + 
                 #Sponsorship_adstockv1 + #ContentMarketing_adstockv1 + OnlineMarketing_adstockv1 + 
                 Affiliates_adstockv1 + #SEM_adstockv1 +
                 #Radio_adstockv1 + 
                 #Other_adstockv1 + 
                 Dummy_Special_sale_week, data = traindata, 
               na.action = na.exclude)

#ha_model 

summary(ha_model)
#Too high adjusted R-squared - 0.971
#Check VIF
as.data.frame(vif(ha_model))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(ha_model,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.961
#Model R-squared: 0.4711
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#1421822
# plot Residuals vs Fitted
plot(ha_model,pch=16,which=1,col="green")


#Try Bootstrapping
# function to obtain R-Squared from the data 
# bootstrapping with 1000 replications 
bootresults <- boot(data=HomeAudio_WeeklyDatav2_scaled, statistic=rsq, 
                    R=1000, formula = gmv ~ Max_sla+ 
                      PremiumProduct + 
                      Affiliates_adstockv1+
                      Dummy_Special_sale_week)

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#       original     bias         std. error
# t1*  0.846724 -0.04847533   0.1302684

# get 95% confidence interval 
boot.ci(bootresults, type="bca")
#library(caret)
#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ Max_sla+ 
                       PremiumProduct + 
                       Affiliates_adstockv1+
                       Dummy_Special_sale_week,   # model to fit
                     data = HomeAudio_WeeklyDatav2_scaled,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#   RMSE    Rsquared  MAE   
#  1066433  0.7216636  859901.7   
#Final Model Summary
summary(ha_model)  
#Call:
#lm(formula = gmv ~ Max_sla + PremiumProduct + Affiliates_adstockv1 + 
#    Dummy_Special_sale_week, data = traindata, na.action = na.exclude)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-3149248  -433721  -118463   660965  2581651 
#
#Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             -10663217    4981289  -2.141 0.039347 *  
#Max_sla                  12639055    4972467   2.542 0.015612 *  
#PremiumProduct            2594252     824374   3.147 0.003362 ** 
#Affiliates_adstockv1      1388023     512545   2.708 0.010398 *  
#Dummy_Special_sale_week   1467268     403073   3.640 0.000872 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1120000 on 35 degrees of freedom
#Multiple R-squared:  0.5253,	Adjusted R-squared:  0.4711 
#F-statistic: 9.684 on 4 and 35 DF,  p-value: 2.214e-05
#-----------------------HomeAudio Additive MOdel-------------------------#



#-----------------------HomeAudio Mutiplicative MOdel-------------------------#
HomeAudio_WeeklyDatav2_ln<-HomeAudio_WeeklyDatav2[,-c(3:4,7:9,16:19,20,22:26)]
HomeAudio_WeeklyDatav2_ln<-cbind(log(HomeAudio_WeeklyDatav2_ln[,-11]),HomeAudio_WeeklyDatav2_ln[,11])

colnames(HomeAudio_WeeklyDatav2_ln)[which(names(HomeAudio_WeeklyDatav2_ln) == "HomeAudio_WeeklyDatav2_ln[, 11]")] <- "Dummy_SpecialSale"
#Fix Nan and Infs

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
HomeAudio_WeeklyDatav2_ln[is.nan(HomeAudio_WeeklyDatav2_ln)] <- 1
which(HomeAudio_WeeklyDatav2_ln==-Inf)
which(HomeAudio_WeeklyDatav2_ln==Inf)
HomeAudio_WeeklyDatav2_ln<-replace(HomeAudio_WeeklyDatav2_ln,HomeAudio_WeeklyDatav2_ln==-Inf,1)
HomeAudio_WeeklyDatav2_ln<-na.omit(HomeAudio_WeeklyDatav2_ln)

# Create row indices for train dataset
trainindices= sample(1:nrow(HomeAudio_WeeklyDatav2_ln), 0.8*nrow(HomeAudio_WeeklyDatav2_ln))
# Create the train data set
traindata = HomeAudio_WeeklyDatav2_ln[trainindices,]
# Create test dataset
testdata = HomeAudio_WeeklyDatav2_ln[-trainindices,]




#Model_1 
ln_ha_model1<-lm(gmv ~ .,na.action = na.exclude, data = traindata)
summary(ln_ha_model1) 
#Check StepAIC

step<-stepAIC(ln_ha_model1, direction="both")
step
#Model2 from StepAIC output

ln_ha_model2<-lm(formula = gmv ~ Max_sla + Dist_CustIds + PaymentType_COD + 
                   PaymentType_Prepaid + PremiumProduct + MassProduct + ListPricevsLastWeek_InflationPrcnt + 
                   TV_adstockv1 + Digital_adstockv1 + Sponsorship_adstockv1 + 
                   Affiliates_adstockv1 + SEM_adstockv1 + Dummy_SpecialSale, 
                 data = traindata, na.action = na.exclude)



summary(ln_ha_model2)
#Too high adjusted R-squared - 0.997
#Check VIF
as.data.frame(vif(ln_ha_model2))

#Remove Dist_CustIds
#Iterate the Process till a suitable model is found

#Model Iteration
ln_ha_model<-  lm(formula = gmv ~ #Max_sla + #Dist_CustIds + 
                    PaymentType_COD + 
                    #PaymentType_Prepaid + 
                    PremiumProduct + #MassProduct + 
                    #ListPricevsLastWeek_InflationPrcnt + 
                    TV_adstockv1 + #Digital_adstockv1 + #Sponsorship_adstockv1 + 
                    #Affiliates_adstockv1 + 
                    SEM_adstockv1 # + Dummy_SpecialSale
                  ,data = traindata, na.action = na.exclude)

#log_ha_model 
#plot(traindata$gmv,traindata$OnlineMarketing_adstockv1)
summary(ln_ha_model)
#Adjusted R-squared - 0.8839
#Check VIF
as.data.frame(vif(ln_ha_model))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(ln_ha_model,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.651
#Model R-squared: 0.8839
#quite low let's check by bootstrapping and CV
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#0.82
# plot Residuals vs Fitted
plot(ln_ha_model,pch=16,which=1,col="green")


#Seems random

#Try Bootstrapping

# bootstrapping with 1000 replications 
bootresults <- boot(data=HomeAudio_WeeklyDatav2_ln, statistic=rsq, 
                    R=1000, formula = gmv ~
                      PaymentType_COD + 
                      PremiumProduct + 
                      TV_adstockv1 + 
                      SEM_adstockv1 )

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#      original  bias    std. error
# t1*  0.821116 -0.008168896   0.1105188

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~
                       PaymentType_COD + 
                       PremiumProduct + 
                       TV_adstockv1 + 
                       SEM_adstockv1 ,   # model to fit
                     data = HomeAudio_WeeklyDatav2_ln,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#    RMSE       Rsquared   MAE   
#   0.6352771  0.6535302  0.4366947

#Final model result
summary(ln_ha_model)
#Call:
#lm(formula = gmv ~ PaymentType_COD + PremiumProduct + TV_adstockv1 + 
#    SEM_adstockv1, data = traindata, na.action = na.exclude)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1.05894 -0.15863 -0.05426  0.11717  1.80174 
#
#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      8.45977    0.46793  18.079  < 2e-16 ***
#PaymentType_COD  0.79897    0.06896  11.587 1.57e-13 ***
#PremiumProduct   0.43100    0.15466   2.787  0.00854 ** 
#TV_adstockv1     0.22658    0.07579   2.989  0.00509 ** 
#SEM_adstockv1   -0.28450    0.13889  -2.048  0.04808 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.4218 on 35 degrees of freedom
#Multiple R-squared:  0.8958,	Adjusted R-squared:  0.8839 
#F-statistic: 75.23 on 4 and 35 DF,  p-value: < 2.2e-16

#-----------------------HomeAudio Mutiplicative MOdel-------------------------#


#-----------------------HomeAudio KOYK MOdel-------------------------#

# Create row indices for train dataset
HomeAudio_WeeklyDatav2_koyk<-HomeAudio_WeeklyDatav2
HomeAudio_WeeklyDatav2_koyk$gmv_lag1<- lag(HomeAudio_WeeklyDatav1$gmv,1)

HomeAudio_WeeklyDatav2_koykscaled<-cbind(scale(HomeAudio_WeeklyDatav2_koyk[,c(-2,-21)]  , center = FALSE, scale = TRUE),HomeAudio_WeeklyDatav2_koyk[,c(2,21)])
HomeAudio_WeeklyDatav2_koykscaled<-na.omit(HomeAudio_WeeklyDatav2_koykscaled)

trainindices= sample(1:nrow(HomeAudio_WeeklyDatav2_koykscaled), 0.8*nrow(HomeAudio_WeeklyDatav2_koykscaled))
# Create the train data set
traindata = HomeAudio_WeeklyDatav2_koykscaled[trainindices,]
# Create test dataset
testdata = HomeAudio_WeeklyDatav2_koykscaled[-trainindices,]

#Model_1 
model_koyk<-lm(gmv ~ ., data = traindata)
summary(model_koyk)  
#Check StepAIC
library(MASS)
step<-stepAIC(model_koyk, direction="both")
step
#Model2 from StepAIC output

koyk_ha_model2<-lm(formula = gmv ~ Max_deliverybdays + Max_deliverycdays + Max_sla + 
                     Dist_CustIds + Avg_product_mrp + PaymentType_COD + PaymentType_Prepaid + 
                     PremiumProduct + MarkDown_Prods + Disc_Category_Low + Disc_Category_Medium + 
                     Disc_Category_High + ListPrice + NPS_week + IndiaHolidays + 
                     ME_InternetUsers + TV_adstockv1 + Digital_adstockv1 + Sponsorship_adstockv1 + 
                     ContentMarketing_adstockv1 + OnlineMarketing_adstockv1 + 
                     Radio_adstockv1 + Other_adstockv1 + gmv_lag1 + Dummy_Special_sale_week, 
                   data = traindata)



summary(koyk_ha_model2)
#Too high adjusted R-squared - 0.993
#Check VIF
as.data.frame(vif(koyk_ha_model2))

#Remove PaymentType_COD
#Iterate the Process till a suitable model is found

#Model Iteration
koyk_ha_model<- lm(formula = gmv ~ #Max_deliverybdays + #Max_deliverycdays + 
                     Max_sla + 
                     #Avg_product_mrp + #PaymentType_COD + Dist_CustIds +
                     PaymentType_Prepaid + 
                     PremiumProduct + #MarkDown_Prods + 
                     #Disc_Category_Low + Disc_Category_Medium + 
                     #Disc_Category_High + ListPrice + 
                     #NPS_week + #IndiaHolidays + 
                     #ME_InternetUsers + 
                     TV_adstockv1 + #Digital_adstockv1 + #Sponsorship_adstockv1 + 
                     #ContentMarketing_adstockv1 + 
                     OnlineMarketing_adstockv1 + 
                     #Radio_adstockv1 + 
                     Other_adstockv1 #+ gmv_lag1 #+ Dummy_Special_sale_week, 
                   ,data = traindata)

#log_ha_model 
#plot(traindata$gmv,traindata$OnlineMarketing_adstockv1)
summary(koyk_ha_model)

#Adjusted R-squared - 0.8438
#Check VIF
as.data.frame(vif(koyk_ha_model))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(koyk_ha_model,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.44
#Model R-squared: 0.853

# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#898866.9
# plot Residuals vs Fitted
plot(koyk_ha_model,pch=16,which=1,col="green")


#Seems random
#Try Bootstrapping

# bootstrapping with 1000 replications 
bootresults <- boot(data=HomeAudio_WeeklyDatav2_koykscaled, statistic=rsq, 
                    R=1000, formula = gmv ~ 
                      Max_sla + 
                      PaymentType_Prepaid + 
                      PremiumProduct +
                      TV_adstockv1 +
                      OnlineMarketing_adstockv1 + 
                      Other_adstockv1)

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#      original  bias    std. error
# t1*   0.8069994 0.01280667   0.0684179

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ 
                       Max_sla + 
                       PaymentType_Prepaid + 
                       PremiumProduct +
                       TV_adstockv1 +
                       OnlineMarketing_adstockv1 + 
                       Other_adstockv1,   # model to fit
                     data = HomeAudio_WeeklyDatav2_koykscaled,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#     RMSE    Rsquared  MAE   
#  710121.3  0.6300467  573417.6

#Final model result
summary(koyk_ha_model)
#Call:
#lm(formula = gmv ~ Max_sla + PaymentType_Prepaid + PremiumProduct + 
#    TV_adstockv1 + OnlineMarketing_adstockv1 + Other_adstockv1, 
#    data = traindata)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1103911  -272994    20053   371890  1322056 
#
#Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
#(Intercept)               -1156398     580570  -1.992 0.054237 .  
#Max_sla                    2501274     696744   3.590 0.001003 ** 
#PaymentType_Prepaid         897841     165716   5.418 4.52e-06 ***
#PremiumProduct              505486     102819   4.916 2.07e-05 ***
#TV_adstockv1              -1157451     316328  -3.659 0.000827 ***
#OnlineMarketing_adstockv1  1743138     341512   5.104 1.17e-05 ***
#Other_adstockv1             471373     146812   3.211 0.002836 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 578600 on 35 degrees of freedom
#Multiple R-squared:  0.8667,	Adjusted R-squared:  0.8438 
#F-statistic: 37.92 on 6 and 35 DF,  p-value: 6.675e-14


#-----------------------HomeAudio KOYK MOdel-------------------------#

#-----------------------HomeAudio Distributed Lag MOdel-------------------------#

# Create row indices for train dataset
HomeAudio_WeeklyDatav2_distlag<-HomeAudio_WeeklyDatav2[,-c(3:7,8,9,16:19,20,23:26)]
HomeAudio_WeeklyDatav2_distlag<-cbind(HomeAudio_WeeklyDatav2_distlag,HomeAudio_WeeklyDatav1[,c(60:61)])

#Calculate lag for Distributed lag model
HomeAudio_WeeklyDatav2_distlag$gmv_lastweek1_lag=lag(HomeAudio_WeeklyDatav2_distlag$gmv,1) 
HomeAudio_WeeklyDatav2_distlag$gmv_lastweek2_lag=lag(HomeAudio_WeeklyDatav2_distlag$gmv,2) 
HomeAudio_WeeklyDatav2_distlag$gmv_lastweek3_lag=lag(HomeAudio_WeeklyDatav2_distlag$gmv,3) 

HomeAudio_WeeklyDatav2_distlag$PaymentType_COD_lastweek1_lag=lag(HomeAudio_WeeklyDatav2_distlag$PaymentType_COD,1) 
HomeAudio_WeeklyDatav2_distlag$PaymentType_COD_lastweek2_lag=lag(HomeAudio_WeeklyDatav2_distlag$PaymentType_COD,2) 

HomeAudio_WeeklyDatav2_distlag$PaymentType_Prepaid_lastweek1_lag=lag(HomeAudio_WeeklyDatav2_distlag$PaymentType_Prepaid,1) 
HomeAudio_WeeklyDatav2_distlag$PaymentType_Prepaid_lastweek2_lag=lag(HomeAudio_WeeklyDatav2_distlag$PaymentType_Prepaid,2) 

HomeAudio_WeeklyDatav2_distlag$PremiumProduct_lastweek1_lag=lag(HomeAudio_WeeklyDatav2_distlag$PremiumProduct,1) 
HomeAudio_WeeklyDatav2_distlag$PremiumProduct_lastweek2_lag=lag(HomeAudio_WeeklyDatav2_distlag$PremiumProduct,2) 

HomeAudio_WeeklyDatav2_distlag$MarkUp_Prods_lastweek1_lag=lag(HomeAudio_WeeklyDatav2_distlag$MarkUp_Prods,1) 
HomeAudio_WeeklyDatav2_distlag$MarkUp_Prods_lastweek2_lag=lag(HomeAudio_WeeklyDatav2_distlag$MarkUp_Prods,2) 

HomeAudio_WeeklyDatav2_distlag$MarkDown_Prods_lastweek1_lag=lag(HomeAudio_WeeklyDatav2_distlag$MarkDown_Prods,1) 
HomeAudio_WeeklyDatav2_distlag$MarkDown_Prods_lastweek2_lag=lag(HomeAudio_WeeklyDatav2_distlag$MarkDown_Prods,2) 

HomeAudio_WeeklyDatav2_distlag$TV_adstockv1_lastweek1_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$TV_adstockv1,1) 
HomeAudio_WeeklyDatav2_distlag$TV_adstockv1_lastweek2_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$TV_adstockv1,2) 

HomeAudio_WeeklyDatav2_distlag$Digital_adstockv1_lastweek1_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$Digital_adstockv1,1) 
HomeAudio_WeeklyDatav2_distlag$Digital_adstockv1_lastweek2_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$Digital_adstockv1,2) 

HomeAudio_WeeklyDatav2_distlag$Sponsorship_adstockv1_lastweek1_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$Sponsorship_adstockv1,1) 
HomeAudio_WeeklyDatav2_distlag$Sponsorship_adstockv1_lastweek2_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$Sponsorship_adstockv1,2) 

HomeAudio_WeeklyDatav2_distlag$ContentMarketing_adstockv1_lastweek1_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$ContentMarketing_adstockv1,1) 
HomeAudio_WeeklyDatav2_distlag$ContentMarketing_adstockv1_lastweek2_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$ContentMarketing_adstockv1,2) 

HomeAudio_WeeklyDatav2_distlag$OnlineMarketing_adstockv1_lastweek1_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$OnlineMarketing_adstockv1,1) 
HomeAudio_WeeklyDatav2_distlag$OnlineMarketing_adstockv1_lastweek2_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$OnlineMarketing_adstockv1,2) 

HomeAudio_WeeklyDatav2_distlag$Affiliates_adstockv1_lastweek1_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$Affiliates_adstockv1,1) 
HomeAudio_WeeklyDatav2_distlag$Affiliates_adstockv1_lastweek2_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$Affiliates_adstockv1,2) 

HomeAudio_WeeklyDatav2_distlag$SEM_adstockv1_lastweek1_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$SEM_adstockv1,1) 
HomeAudio_WeeklyDatav2_distlag$SEM_adstockv1_lastweek2_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$SEM_adstockv1,2) 

HomeAudio_WeeklyDatav2_distlag$Radio_adstockv1_lastweek1_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$Radio_adstockv1,1) 
HomeAudio_WeeklyDatav2_distlag$Radio_adstockv1_lastweek2_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$Radio_adstockv1,2) 

HomeAudio_WeeklyDatav2_distlag$Other_adstockv1_lastweek1_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$Other_adstockv1,1) 
HomeAudio_WeeklyDatav2_distlag$Other_adstockv1_lastweek2_lag=stats::lag(HomeAudio_WeeklyDatav2_distlag$Other_adstockv1,2) 



HomeAudio_WeeklyDatav2_distlag_scaled<-cbind(scale(HomeAudio_WeeklyDatav2_distlag[,c(-2,-9)]  , center = FALSE, scale = TRUE),HomeAudio_WeeklyDatav2_distlag[,c(2,9)])
HomeAudio_WeeklyDatav2_distlag_scaled<-na.omit(HomeAudio_WeeklyDatav2_distlag_scaled)
HomeAudio_WeeklyDatav2_distlag_scaled<-na.omit(HomeAudio_WeeklyDatav2_distlag_scaled)
trainindices= sample(1:nrow(HomeAudio_WeeklyDatav2_distlag_scaled), 0.8*nrow(HomeAudio_WeeklyDatav2_distlag_scaled))
# Create the train data set
traindata = HomeAudio_WeeklyDatav2_distlag_scaled[trainindices,]
# Create test dataset
testdata = HomeAudio_WeeklyDatav2_distlag_scaled[-trainindices,]

#Model_1 
model_dlag<-lm(gmv ~ ., data = traindata)
summary(model_dlag)  
#Check StepAIC
library(MASS)
step<-stepAIC(model_dlag, direction="both")
step
#Model2 from StepAIC output

model_dlag2<-lm(formula = gmv ~ UniqueItemQty + PaymentType_COD + MarkUp_Prods + 
                  MarkDown_Prods + NPS_week + ListPricevsLastWeek_InflationPrcnt + 
                  TV_adstockv1 + Digital_adstockv1 + Sponsorship_adstockv1 + 
                  ContentMarketing_adstockv1 + OnlineMarketing_adstockv1 + 
                  Affiliates_adstockv1 + SEM_adstockv1 + Radio_adstockv1 + 
                  Other_adstockv1 + ListPricevsLast2Week_InflationPrcnt + gmv_lastweek2_lag + 
                  gmv_lastweek3_lag + PaymentType_COD_lastweek1_lag + PaymentType_COD_lastweek2_lag + 
                  PaymentType_Prepaid_lastweek1_lag + PaymentType_Prepaid_lastweek2_lag + 
                  PremiumProduct_lastweek2_lag + PremiumProduct_lastweek1_lag + 
                  MarkUp_Prods_lastweek1_lag + MarkUp_Prods_lastweek2_lag + 
                  MarkDown_Prods_lastweek1_lag + MarkDown_Prods_lastweek2_lag + 
                  Dummy_Special_sale_week, data = traindata)




summary(model_dlag2)
#Too high adjusted R-squared - 0.9945
#Check VIF
as.data.frame(vif(model_dlag2))

#Remove MarkDown_Prods_lastweek1_lag
#Iterate the Process till a suitable model is found

#Model Iteration
model_dlag<- lm(formula = gmv ~ #UniqueItemQty + 
                  PaymentType_COD + MarkUp_Prods + 
                  #MarkDown_Prods + 
                  #NPS_week + 
                  ListPricevsLastWeek_InflationPrcnt + 
                  TV_adstockv1 + #Digital_adstockv1 + 
                  #Sponsorship_adstockv1 + 
                  #ContentMarketing_adstockv1 + #OnlineMarketing_adstockv1 + 
                  Affiliates_adstockv1 + #SEM_adstockv1 + 
                  #Radio_adstockv1 + 
                  #Other_adstockv1 + 
                  #ListPricevsLast2Week_InflationPrcnt + #gmv_lastweek2_lag + 
                  #gmv_lastweek3_lag + 
                  PaymentType_COD_lastweek1_lag +# PaymentType_COD_lastweek2_lag + 
                  PaymentType_Prepaid_lastweek1_lag #+# PaymentType_Prepaid_lastweek2_lag + 
                  #PremiumProduct_lastweek2_lag + #PremiumProduct_lastweek1_lag + 
                  #MarkUp_Prods_lastweek1_lag + #MarkUp_Prods_lastweek2_lag + 
                  #MarkDown_Prods_lastweek1_lag + 
                  #MarkDown_Prods_lastweek2_lag + 
                #  Dummy_Special_sale_week
                , data = traindata)

#log_ha_model 
#plot(traindata$gmv,traindata$OnlineMarketing_adstockv1)
summary(model_dlag)

#Adjusted R-squared - 0.9509
#Check VIF
as.data.frame(vif(model_dlag))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(model_dlag,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.76
#Model R-squared: 0.95
#Will check using cross validation
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#898866.9
# plot Residuals vs Fitted
plot(model_dlag,pch=16,which=1,col="green")


#Seems random
#Try Bootstrapping

# bootstrapping with 1000 replications 
bootresults <- boot(data=HomeAudio_WeeklyDatav2_distlag_scaled, statistic=rsq, 
                    R=1000, formula = gmv ~ 
                      PaymentType_COD + MarkUp_Prods + 
                      ListPricevsLastWeek_InflationPrcnt + 
                      TV_adstockv1 + 
                      Affiliates_adstockv1 +  
                      PaymentType_COD_lastweek1_lag +
                      PaymentType_Prepaid_lastweek1_lag
)

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#      original  bias    std. error
# t1*  0.9523924 0.002912816  0.02705509

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ 
                       PaymentType_COD + MarkUp_Prods + 
                       ListPricevsLastWeek_InflationPrcnt + 
                       TV_adstockv1 + 
                       Affiliates_adstockv1 +  
                       PaymentType_COD_lastweek1_lag +
                       PaymentType_Prepaid_lastweek1_lag ,   # model to fit
                     data = HomeAudio_WeeklyDatav2_distlag_scaled,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#     RMSE    Rsquared  MAE   
#  822972.4  0.8686173  614856.3  

#Final model result
summary(model_dlag)
#Call:
#lm(formula = gmv ~ PaymentType_COD + MarkUp_Prods + ListPricevsLastWeek_InflationPrcnt + 
#    TV_adstockv1 + Affiliates_adstockv1 + PaymentType_COD_lastweek1_lag + 
#    PaymentType_Prepaid_lastweek1_lag, data = traindata)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1445185  -353992   -74674   271480  1461042 
#
#Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                         -427816     425387  -1.006  0.32260    
#PaymentType_COD                     5568771     265032  21.012  < 2e-16 ***
#MarkUp_Prods                         665657     308783   2.156  0.03925 *  
#ListPricevsLastWeek_InflationPrcnt   382169     121372   3.149  0.00369 ** 
#TV_adstockv1                        1286544     356841   3.605  0.00112 ** 
#Affiliates_adstockv1               -1289387     518870  -2.485  0.01876 *  
#PaymentType_COD_lastweek1_lag       -666464     283444  -2.351  0.02547 *  
#PaymentType_Prepaid_lastweek1_lag    570633     233504   2.444  0.02063 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 643100 on 30 degrees of freedom
#Multiple R-squared:  0.9602,	Adjusted R-squared:  0.9509 
#F-statistic: 103.5 on 7 and 30 DF,  p-value: < 2.2e-16
#

#-----------------------HomeAudio Distributed Lag MOdel-------------------------#


#-----------------------HomeAudio Mutiplicative Distributed Lag MOdel-------------------------#

HomeAudio_WeeklyDatav2_distlag_ln<-cbind(log(HomeAudio_WeeklyDatav2_distlag[,-9]),HomeAudio_WeeklyDatav2_distlag[,9])

colnames(HomeAudio_WeeklyDatav2_distlag_ln)[which(names(HomeAudio_WeeklyDatav2_distlag_ln) == "HomeAudio_WeeklyDatav2_distlag[, 9]")] <- "Dummy_SpecialSale"
#Fix Nan and Infs

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
HomeAudio_WeeklyDatav2_distlag_ln[is.nan(HomeAudio_WeeklyDatav2_distlag_ln)] <- 1
which(HomeAudio_WeeklyDatav2_distlag_ln==-Inf)
which(HomeAudio_WeeklyDatav2_distlag_ln==Inf)
HomeAudio_WeeklyDatav2_distlag_ln<-replace(HomeAudio_WeeklyDatav2_distlag_ln,HomeAudio_WeeklyDatav2_distlag_ln==-Inf,1)
HomeAudio_WeeklyDatav2_distlag_ln<-na.omit(HomeAudio_WeeklyDatav2_distlag_ln)

# Create row indices for train dataset
trainindices= sample(1:nrow(HomeAudio_WeeklyDatav2_distlag_ln), 0.8*nrow(HomeAudio_WeeklyDatav2_distlag_ln))
# Create the train data set
traindata = HomeAudio_WeeklyDatav2_distlag_ln[trainindices,]
# Create test dataset
testdata = HomeAudio_WeeklyDatav2_distlag_ln[-trainindices,]




#Model_1 
Dlagln_ha_model1<-lm(gmv ~ .,na.action = na.exclude, data = traindata)
summary(Dlagln_ha_model1) 
#Check StepAIC

step<-stepAIC(Dlagln_ha_model1, direction="both")
step
#Model2 from StepAIC output

Dlagln_ha_model2<-lm(formula = gmv ~ UniqueItemQty + PaymentType_Prepaid + PremiumProduct + 
                       MarkUp_Prods + NPS_week + ListPricevsLastWeek_InflationPrcnt + 
                       Sponsorship_adstockv1 + ContentMarketing_adstockv1 + OnlineMarketing_adstockv1 + 
                       Affiliates_adstockv1 + SEM_adstockv1 + ListPricevsLast2Week_InflationPrcnt + 
                       gmv_lastweek1_lag + gmv_lastweek2_lag + gmv_lastweek3_lag + 
                       PaymentType_Prepaid_lastweek1_lag + PaymentType_Prepaid_lastweek2_lag + 
                       MarkUp_Prods_lastweek1_lag + MarkUp_Prods_lastweek2_lag + 
                       MarkDown_Prods_lastweek1_lag + MarkDown_Prods_lastweek2_lag + 
                       Dummy_SpecialSale, data = traindata, na.action = na.exclude)



summary(Dlagln_ha_model2)
#Too high adjusted R-squared - 0.9989
#Check VIF
as.data.frame(vif(Dlagln_ha_model2))

#Remove MarkDown_Prods_lastweek2_lag
#Iterate the Process till a suitable model is found

#Model Iteration
Dlagln_ha_model<-  lm(formula = gmv ~ #UniqueItemQty + 
                        PaymentType_Prepaid + #PremiumProduct + 
                        MarkUp_Prods + #NPS_week + #ListPricevsLastWeek_InflationPrcnt + 
                        #Sponsorship_adstockv1 + 
                        ContentMarketing_adstockv1 + #OnlineMarketing_adstockv1 + 
                        #Affiliates_adstockv1 + #SEM_adstockv1 + #ListPricevsLast2Week_InflationPrcnt + 
                        #gmv_lastweek1_lag + #gmv_lastweek2_lag + 
                        #gmv_lastweek3_lag + 
                        #PaymentType_Prepaid_lastweek1_lag + 
                        #PaymentType_Prepaid_lastweek2_lag + 
                        #MarkUp_Prods_lastweek1_lag + #MarkUp_Prods_lastweek2_lag + 
                        #MarkDown_Prods_lastweek1_lag +# MarkDown_Prods_lastweek2_lag + 
                        Dummy_SpecialSale, data = traindata, na.action = na.exclude)
#Dlagln_ha_model 
#plot(traindata$gmv,traindata$OnlineMarketing_adstockv1)
summary(Dlagln_ha_model)
#Adjusted R-squared - 0.9158
#Check VIF
as.data.frame(vif(Dlagln_ha_model))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(Dlagln_ha_model,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.7151
#Model R-squared: 0.9158
#quite low let's check by bootstrapping and CV
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#0.43
# plot Residuals vs Fitted
plot(Dlagln_ha_model,pch=16,which=1,col="green")


#Seems random

#Try Bootstrapping

# bootstrapping with 1000 replications 
bootresults <- boot(data=HomeAudio_WeeklyDatav2_distlag_ln, statistic=rsq, 
                    R=1000, formula = gmv ~ 
                      PaymentType_Prepaid + 
                      MarkUp_Prods + 
                      ContentMarketing_adstockv1 + 
                      Dummy_SpecialSale  )

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#      original  bias    std. error
# t1*  0.9023697 -0.05174576    0.119313

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ 
                       PaymentType_Prepaid + 
                       MarkUp_Prods + 
                       ContentMarketing_adstockv1 + 
                       Dummy_SpecialSale  ,   # model to fit
                     data = HomeAudio_WeeklyDatav2_distlag_ln,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#       RMSE    Rsquared  MAE   
#    0.5069273  0.560154  0.3386135

#Final model result
summary(Dlagln_ha_model)
#Call:
#lm(formula = gmv ~ PaymentType_Prepaid + MarkUp_Prods + ContentMarketing_adstockv1 + 
#    Dummy_SpecialSale, data = traindata, na.action = na.exclude)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-0.88360 -0.16272  0.01645  0.17678  0.70475 
#
#Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                 7.63744    0.43377  17.607  < 2e-16 ***
#PaymentType_Prepaid         0.73160    0.11192   6.537 2.01e-07 ***
#MarkUp_Prods                0.65134    0.18772   3.470 0.001472 ** 
#ContentMarketing_adstockv1  0.22722    0.05736   3.961 0.000376 ***
#Dummy_SpecialSale           0.33122    0.14620   2.265 0.030173 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.3709 on 33 degrees of freedom
#Multiple R-squared:  0.9249,	Adjusted R-squared:  0.9158 
#F-statistic: 101.6 on 4 and 33 DF,  p-value: < 2.2e-16

#-----------------------HomeAudio Mutiplicative Distributed Lag MOdel-------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#----------------------------------------HomeAudio--------------------------------------------#



#----------------------------------------CameraAccessory--------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#-----------------------CameraAccessory Dataset Build-------------------------#
#Adding Weekly List Price inflation metrics till 4 weeks
CameraAccessory_WeeklyDatav1<-as.data.frame(subset(OrderMain_Weeklyv2,OrderMain_Weeklyv2$product_analytic_sub_category.x=="CameraAccessory"))
CameraAccessory_WeeklyDatav1$ListPricevsLastWeek_value <- lag(CameraAccessory_WeeklyDatav1$ListPrice,1)
CameraAccessory_WeeklyDatav1$ListPricevsLast2Week_value <- lag(CameraAccessory_WeeklyDatav1$ListPrice,2)
CameraAccessory_WeeklyDatav1$ListPricevsLast3Week_value <- lag(CameraAccessory_WeeklyDatav1$ListPrice,3)
CameraAccessory_WeeklyDatav1$ListPricevsLast4Week_value <- lag(CameraAccessory_WeeklyDatav1$ListPrice,4)
CameraAccessory_WeeklyDatav1$ListPricevsLastWeek_value[which(is.na(CameraAccessory_WeeklyDatav1$ListPricevsLastWeek_value))]<-0
CameraAccessory_WeeklyDatav1$ListPricevsLast2Week_value[which(is.na(CameraAccessory_WeeklyDatav1$ListPricevsLast2Week_value))]<-0
CameraAccessory_WeeklyDatav1$ListPricevsLast3Week_value[which(is.na(CameraAccessory_WeeklyDatav1$ListPricevsLast3Week_value))]<-0
CameraAccessory_WeeklyDatav1$ListPricevsLast4Week_value[which(is.na(CameraAccessory_WeeklyDatav1$ListPricevsLast4Week_value))]<-0

CameraAccessory_WeeklyDatav1$ListPricevsLastWeek_InflationPrcnt<-((CameraAccessory_WeeklyDatav1$ListPrice-CameraAccessory_WeeklyDatav1$ListPricevsLastWeek_value)/CameraAccessory_WeeklyDatav1$ListPricevsLastWeek_value)*100
CameraAccessory_WeeklyDatav1$ListPricevsLast2Week_InflationPrcnt<-((CameraAccessory_WeeklyDatav1$ListPrice-CameraAccessory_WeeklyDatav1$ListPricevsLast2Week_value)/CameraAccessory_WeeklyDatav1$ListPricevsLast2Week_value)*100
CameraAccessory_WeeklyDatav1$ListPricevsLast3Week_InflationPrcnt<-((CameraAccessory_WeeklyDatav1$ListPrice-CameraAccessory_WeeklyDatav1$ListPricevsLast3Week_value)/CameraAccessory_WeeklyDatav1$ListPricevsLast3Week_value)*100
CameraAccessory_WeeklyDatav1$ListPricevsLast4Week_InflationPrcnt<-((CameraAccessory_WeeklyDatav1$ListPrice-CameraAccessory_WeeklyDatav1$ListPricevsLast4Week_value)/CameraAccessory_WeeklyDatav1$ListPricevsLast4Week_value)*100

CameraAccessory_WeeklyDatav1$ListPricevsLastWeek_InflationPrcnt[which(CameraAccessory_WeeklyDatav1$ListPricevsLastWeek_InflationPrcnt=="Inf")]<-NA
CameraAccessory_WeeklyDatav1$ListPricevsLast2Week_InflationPrcnt[which(CameraAccessory_WeeklyDatav1$ListPricevsLast2Week_InflationPrcnt=="Inf")]<-NA
CameraAccessory_WeeklyDatav1$ListPricevsLast3Week_InflationPrcnt[which(CameraAccessory_WeeklyDatav1$ListPricevsLast3Week_InflationPrcnt=="Inf")]<-NA
CameraAccessory_WeeklyDatav1$ListPricevsLast4Week_InflationPrcnt[which(CameraAccessory_WeeklyDatav1$ListPricevsLast4Week_InflationPrcnt=="Inf")]<-NA
#Weekly Investment Calculation
#We are Calculating weekly investment using MOnthly Investment as explained below:
#Break Monthly investment to daily by diving MOnthly Investment by Number of Days in month
#Calculate Number of days in all the week
#Daily Investment * Number of Days in week * Percent_InvestmentIn_GameAccessory = Total investment for the week
CameraAccessory_WeeklyDatav1$Weekly_TotalInvestment=Invest_CameraAccessory*CameraAccessory_WeeklyDatav1$Daily_TotalInvestment*CameraAccessory_WeeklyDatav1$DaysInWeek
CameraAccessory_WeeklyDatav1$Weekly_TV=Invest_CameraAccessory*CameraAccessory_WeeklyDatav1$Daily_TV*CameraAccessory_WeeklyDatav1$DaysInWeek
CameraAccessory_WeeklyDatav1$Weekly_Digital=Invest_CameraAccessory*CameraAccessory_WeeklyDatav1$Daily_Digital*CameraAccessory_WeeklyDatav1$DaysInWeek
CameraAccessory_WeeklyDatav1$Weekly_Sponsorship=Invest_CameraAccessory*CameraAccessory_WeeklyDatav1$Daily_Sponsorship*CameraAccessory_WeeklyDatav1$DaysInWeek
CameraAccessory_WeeklyDatav1$Weekly_ContentMarketing=Invest_CameraAccessory*CameraAccessory_WeeklyDatav1$Daily_ContentMarketing*CameraAccessory_WeeklyDatav1$DaysInWeek
CameraAccessory_WeeklyDatav1$Weekly_OnlineMarketing=Invest_CameraAccessory*CameraAccessory_WeeklyDatav1$Daily_OnlineMarketing*CameraAccessory_WeeklyDatav1$DaysInWeek
CameraAccessory_WeeklyDatav1$Weekly_Affiliates=Invest_CameraAccessory*CameraAccessory_WeeklyDatav1$Daily_Affiliates*CameraAccessory_WeeklyDatav1$DaysInWeek
CameraAccessory_WeeklyDatav1$Weekly_SEM=Invest_CameraAccessory*CameraAccessory_WeeklyDatav1$Daily_SEM*CameraAccessory_WeeklyDatav1$DaysInWeek
CameraAccessory_WeeklyDatav1$Weekly_Radio=Invest_CameraAccessory*CameraAccessory_WeeklyDatav1$Daily_Radio*CameraAccessory_WeeklyDatav1$DaysInWeek
CameraAccessory_WeeklyDatav1$Weekly_Other=Invest_CameraAccessory*CameraAccessory_WeeklyDatav1$Daily_Other*CameraAccessory_WeeklyDatav1$DaysInWeek
#Add Id column 
CameraAccessory_WeeklyDatav1$ID <- seq.int(nrow(CameraAccessory_WeeklyDatav1))


#Plot GMV to check the trend
ggplot(CameraAccessory_WeeklyDatav1)+labs(title = "Weekly GMV", x="Weeks", y="GMV") + 
  geom_bar(aes(x=ID, y=(gmv)),stat="identity", fill="#B2EBF2", colour="#E0F7FA")+theme_minimal()




#Plot GMV with Total Investments to check the trend
##Total Investments is in dotted line
ggplot(CameraAccessory_WeeklyDatav1) + labs(title = "Total Investment vs GMV", x="Weeks", y="GMV & Total Investment") + 
  geom_bar(aes(x=ID, y=(gmv/10000)),stat="identity", fill="#B2EBF2", colour="#E0F7FA")+
  geom_line(aes(x=ID, y=Weekly_TotalInvestment),stat="identity",color="#9C27B0",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_TotalInvestment,color="#E64A19"))+theme_minimal()+theme(legend.position="none")

#Plot GMV with TV Investments to check the trend
##TV Investments are in dotted line
ggplot(CameraAccessory_WeeklyDatav1) + labs(title = "TV Investment vs GMV", x="Weeks", y="GMV & TV Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_TV),stat="identity",color="#E64A19",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_TV,color="#E64A19"))+theme_minimal()+theme(legend.position="none")
#We consider that TV ad effect remains 5%  till 6th weeks, with percentage decay rates for 1-6 week given below:
#0.60	0.36	0.22	0.13	0.08	0.05 
CameraAccessory_WeeklyDatav1$TV_adstockv1 <- stats::filter(CameraAccessory_WeeklyDatav1$Weekly_TV,0.6,method="recursive")
ggplot(CameraAccessory_WeeklyDatav1)  + labs(title = "TV: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_TV),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=TV_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Plot GMV with Digital Investments to check the trend
##Digital Investments are in dotted line
ggplot(CameraAccessory_WeeklyDatav1) + labs(title = "Digital Investment vs GMV", x="Weeks", y="GMV & Digital Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Digital),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Digital,color="#006064"))+theme_minimal()+theme(legend.position="none")
#We consider that Digital ad effect remains 10%  till 1st week with percentage decay rate given below:
#0.1
CameraAccessory_WeeklyDatav1$Digital_adstockv1 <- stats::filter(CameraAccessory_WeeklyDatav1$Weekly_Digital,0.1,method="recursive")
ggplot(CameraAccessory_WeeklyDatav1)  + labs(title = "Digital: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Digital),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Digital_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Plot GMV with Sponsorship Investments to check the trend
##Sponsorship Investments are in dotted line
ggplot(CameraAccessory_WeeklyDatav1) + labs(title = "Sponsorship Investment vs GMV", x="Weeks", y="GMV & Sponsorship Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Sponsorship),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Sponsorship,color="#006064"))+theme_minimal()+theme(legend.position="none")
#We consider that Sponsorship ad effect remains 10%  till 4th week with percentage decay rates given below:
#0.4	0.40	0.16	0.06
CameraAccessory_WeeklyDatav1$Sponsorship_adstockv1 <- stats::filter(CameraAccessory_WeeklyDatav1$Weekly_Sponsorship,0.4,method="recursive")
ggplot(CameraAccessory_WeeklyDatav1)  + labs(title = "Sponsorship: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Sponsorship),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Sponsorship_adstockv1),stat="identity",color="blue")+
  theme_minimal()


#Plot GMV with ContentMarketing Investments to check the trend
##ContentMarketing Investments are in dotted line
ggplot(CameraAccessory_WeeklyDatav1) + labs(title = "ContentMarketing Investment vs GMV", x="Weeks", y="GMV & ContentMarketing Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_ContentMarketing),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_ContentMarketing,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that ContentMarketing ad effect remains 10%  till 4th week with percentage decay rates given below:
#0.4	0.40	0.16	0.06
CameraAccessory_WeeklyDatav1$ContentMarketing_adstockv1 <- stats::filter(CameraAccessory_WeeklyDatav1$Weekly_ContentMarketing,0.4,method="recursive")
ggplot(CameraAccessory_WeeklyDatav1)  + labs(title = "ContentMarketing: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_ContentMarketing),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=ContentMarketing_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Plot GMV with OnlineMarketing Investments to check the trend
##OnlineMarketing Investments are in dotted line
ggplot(CameraAccessory_WeeklyDatav1) + labs(title = "OnlineMarketing Investment vs GMV", x="Weeks", y="GMV & OnlineMarketing Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_OnlineMarketing),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_OnlineMarketing,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that OnlineMarketing ad effect remains 9%  till 2nd week with percentage decay rates given below:
#0.3  0.09
CameraAccessory_WeeklyDatav1$OnlineMarketing_adstockv1 <- stats::filter(CameraAccessory_WeeklyDatav1$Weekly_OnlineMarketing,0.3,method="recursive")
ggplot(CameraAccessory_WeeklyDatav1)  + labs(title = "OnlineMarketing: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_OnlineMarketing),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=OnlineMarketing_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Plot GMV with Affiliates Investments to check the trend
##Affiliates Investments are in dotted line
ggplot(CameraAccessory_WeeklyDatav1) + labs(title = "Affiliates Investment vs GMV", x="Weeks", y="GMV & Affiliates Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Affiliates),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Affiliates,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that Affiliates ad effect remains 6%  till 3rd week with percentage decay rates given below:
#0.40	0.16	0.06
CameraAccessory_WeeklyDatav1$Affiliates_adstockv1 <- stats::filter(CameraAccessory_WeeklyDatav1$Weekly_Affiliates,0.4,method="recursive")
ggplot(CameraAccessory_WeeklyDatav1)  + labs(title = "Affiliates: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Affiliates),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Affiliates_adstockv1),stat="identity",color="blue")+
  theme_minimal()


#Plot GMV with SEM Investments to check the trend
##SEM Investments are in dotted line
ggplot(CameraAccessory_WeeklyDatav1) + labs(title = "SEM Investment vs GMV", x="Weeks", y="GMV & SEM Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_SEM),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_SEM,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that SEM ad effect remains 9%  till 2nd week with percentage decay rates given below:
#0.3  0.09
CameraAccessory_WeeklyDatav1$SEM_adstockv1 <- stats::filter(CameraAccessory_WeeklyDatav1$Weekly_SEM,0.3,method="recursive")
ggplot(CameraAccessory_WeeklyDatav1)  + labs(title = "SEM: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_SEM),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=SEM_adstockv1),stat="identity",color="blue")+
  theme_minimal()


#Plot GMV with Radio Investments to check the trend
##Radio Investments are in dotted line
ggplot(CameraAccessory_WeeklyDatav1) + labs(title = "Radio Investment vs GMV", x="Weeks", y="GMV & Radio Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Radio),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Radio,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that Radio ad effect remains 6%  till 3rd week with percentage decay rates given below:
#0.40	0.16	0.06
CameraAccessory_WeeklyDatav1$Radio_adstockv1 <- stats::filter(CameraAccessory_WeeklyDatav1$Weekly_Radio,0.4,method="recursive")
ggplot(CameraAccessory_WeeklyDatav1)  + labs(title = "Radio: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Radio),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Radio_adstockv1),stat="identity",color="blue")+
  theme_minimal()  


#Plot GMV with Other Investments to check the trend
##Other Investments are in dotted line
ggplot(CameraAccessory_WeeklyDatav1) + labs(title = "Other Investment vs GMV", x="Weeks", y="GMV & Other Investment") + 
  geom_bar(aes(x=ID, y=(gmv/100000)),stat="identity", fill="#B2EBF2", colour="white")+
  geom_line(aes(x=ID, y=Weekly_Other),stat="identity",color="#006064",linetype="dotted")+ 
  geom_point(aes(x=ID, y=Weekly_Other,color="#006064"))+theme_minimal()+theme(legend.position="none")

#We consider that Other ad effect remains 9%  till 2nd week with percentage decay rates given below:
#0.3  0.09
CameraAccessory_WeeklyDatav1$Other_adstockv1 <- stats::filter(CameraAccessory_WeeklyDatav1$Weekly_Other,0.3,method="recursive")
ggplot(CameraAccessory_WeeklyDatav1)  + labs(title = "Other: Adstock vs Investment", x="Weeks", y="Value") + 
  geom_line(aes(x=ID, y=Weekly_Other),stat="identity",color="red")+ 
  geom_line(aes(x=ID, y=Other_adstockv1),stat="identity",color="blue")+
  theme_minimal()

#Let us reduce the number of variables below 40 to create a dataset for regression
#Le us remove the unnecessaryvariables first
CameraAccessory_WeeklyDatav2<-CameraAccessory_WeeklyDatav1[,c(6:9,11:45,60,75:83)]
#We will take correlatio for determining the Avg, Mean and Max type derived KPI relevance
library(corrplot)

corrplot(cor(CameraAccessory_WeeklyDatav2[,c(3,5:13,18:22)]),method="square")
#only the variables with good correlation will be retained
CameraAccessory_WeeklyDatav2<-CameraAccessory_WeeklyDatav2[,-c(5,6,8,9,11,12,19,21,22)]
#Let's check for the complete data frame
corrplot(cor(CameraAccessory_WeeklyDatav2,use = "complete.obs"),method="square")
#Take a closer look at first 12 columns for very high co-relation
corrplot(cor(CameraAccessory_WeeklyDatav2[,1:15],use = "complete.obs"),method="number")
#Remove Order Count, Total_custId, Total_PinCode,OrderQty for now
CameraAccessory_WeeklyDatav2<-CameraAccessory_WeeklyDatav2[,-c(1,4,9,11)]
#We will remove the rest using VIF

#Scale the quantitative variables 

CameraAccessory_WeeklyDatav2_scaled<-cbind(scale(CameraAccessory_WeeklyDatav2[,c(-2,-21)]  , center = FALSE, scale = TRUE),CameraAccessory_WeeklyDatav2[,c(2,21)])
CameraAccessory_WeeklyDatav2_scaled<-na.omit(CameraAccessory_WeeklyDatav2_scaled)

#-----------------------CameraAccessory Dataset Build-------------------------#

#-----------------------CameraAccessory Additive MOdel-------------------------#
#set the seed to 100 
set.seed(123)
# Create row indices for train dataset
trainindices= sample(1:nrow(CameraAccessory_WeeklyDatav2_scaled), 0.8*nrow(CameraAccessory_WeeklyDatav2_scaled))
# Create the train data set
traindata = CameraAccessory_WeeklyDatav2_scaled[trainindices,]
# Create test dataset
testdata = CameraAccessory_WeeklyDatav2_scaled[-trainindices,]

#Model_1 
model1<-lm(gmv ~ ., data = traindata,na.action = na.exclude)
summary(model1)  
#Check StepAIC
library(MASS)
step<-stepAIC(model1, direction="both")
step
#Model2 from StepAIC output
model2<-lm(formula = gmv ~ UniqueItemQty + Max_deliverybdays + Max_sla + 
             Dist_CustIds + Dist_pincode + Avg_product_mrp + Max_product_procurement_sla + 
             MarkUp_Prods + Disc_Category_Low + Disc_Category_Medium + 
             Disc_Category_High + NPS_week + ME_InternetUsers + Avg_frequency + 
             ListPricevsLastWeek_InflationPrcnt + Digital_adstockv1 + 
             Sponsorship_adstockv1 + Affiliates_adstockv1 + Radio_adstockv1 + 
             Other_adstockv1, data = traindata, na.action = na.exclude)
summary(model2)
#Adjusted R-squared - 0.9974
#Check VIF
as.data.frame(vif(model2))
#Remove Max_sla

#Iterate the Process till a suitable model is found

#Model Iteration
ca_model<-  lm(formula = gmv ~ #UniqueItemQty + 
                 #Max_deliverybdays + #Max_sla + 
                 #Dist_CustIds + 
                 Dist_pincode + #Avg_product_mrp + 
                 #Max_product_procurement_sla + 
                 MarkUp_Prods + #Disc_Category_Low + Disc_Category_Medium + 
                 #Disc_Category_High + 
                 #NPS_week + 
                # ME_InternetUsers + #Avg_frequency + 
                 #ListPricevsLastWeek_InflationPrcnt + 
                 Digital_adstockv1# + 
                 #Sponsorship_adstockv1 + 
                 #Affiliates_adstockv1 + #Radio_adstockv1 + 
               #  Other_adstockv1
               , data = traindata, na.action = na.exclude)

#ca_model 

summary(ca_model)
#Too high adjusted R-squared - 0.7965
#Check VIF
as.data.frame(vif(ca_model))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(ca_model,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.840
#Model R-squared: 0.7965
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#993639.3
# plot Residuals vs Fitted
plot(ca_model,pch=16,which=1,col="green")


#Try Bootstrapping
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 
# bootstrapping with 1000 replications 
bootresults <- boot(data=CameraAccessory_WeeklyDatav2_scaled, statistic=rsq, 
                    R=1000, formula = gmv ~ 
                      Dist_pincode + 
                      MarkUp_Prods + 
                      Digital_adstockv1)

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#       original     bias         std. error
# t1*   0.8087147 0.02356116  0.04730105

# get 95% confidence interval 
boot.ci(bootresults, type="bca")
#library(caret)
#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ 
                       Dist_pincode + 
                       MarkUp_Prods + 
                       Digital_adstockv1,   # model to fit
                     data = CameraAccessory_WeeklyDatav2_scaled,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#   RMSE    Rsquared  MAE   
#  969513.9  0.7572014  720318.9  
#Final Model Summary
summary(ca_model)  
#Call:
#lm(formula = gmv ~ Dist_pincode + MarkUp_Prods + Digital_adstockv1, 
#    data = traindata, na.action = na.exclude)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1576077  -477014  -137885   251638  3777827 
#
#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       -1098929     513559  -2.140    0.039 *  
#Dist_pincode       3704479     582158   6.363 2.03e-07 ***
#MarkUp_Prods       2454102     529719   4.633 4.36e-05 ***
#Digital_adstockv1   393206     181698   2.164    0.037 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 952600 on 37 degrees of freedom
#Multiple R-squared:  0.8117,	Adjusted R-squared:  0.7965 
#F-statistic: 53.18 on 3 and 37 DF,  p-value: 1.717e-13
#-----------------------CameraAccessory Additive MOdel-------------------------#



#-----------------------CameraAccessory Mutiplicative MOdel-------------------------#
CameraAccessory_WeeklyDatav2_ln<-CameraAccessory_WeeklyDatav2[,-c(3:4,7:9,16:19,20,22:26)]
CameraAccessory_WeeklyDatav2_ln<-cbind(log(CameraAccessory_WeeklyDatav2_ln[,-11]),CameraAccessory_WeeklyDatav2_ln[,11])

colnames(CameraAccessory_WeeklyDatav2_ln)[which(names(CameraAccessory_WeeklyDatav2_ln) == "CameraAccessory_WeeklyDatav2_ln[, 11]")] <- "Dummy_SpecialSale"
#Fix Nan and Infs

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
CameraAccessory_WeeklyDatav2_ln[is.nan(CameraAccessory_WeeklyDatav2_ln)] <- 1
which(CameraAccessory_WeeklyDatav2_ln==-Inf)
which(CameraAccessory_WeeklyDatav2_ln==Inf)
CameraAccessory_WeeklyDatav2_ln<-replace(CameraAccessory_WeeklyDatav2_ln,CameraAccessory_WeeklyDatav2_ln==-Inf,1)
CameraAccessory_WeeklyDatav2_ln<-na.omit(CameraAccessory_WeeklyDatav2_ln)

# Create row indices for train dataset
trainindices= sample(1:nrow(CameraAccessory_WeeklyDatav2_ln), 0.8*nrow(CameraAccessory_WeeklyDatav2_ln))
# Create the train data set
traindata = CameraAccessory_WeeklyDatav2_ln[trainindices,]
# Create test dataset
testdata = CameraAccessory_WeeklyDatav2_ln[-trainindices,]




#Model_1 
ln_ca_model1<-lm(gmv ~ .,na.action = na.exclude, data = traindata)
summary(ln_ca_model1) 
#Check StepAIC

step<-stepAIC(ln_ca_model1, direction="both")
step
#Model2 from StepAIC output

ln_ca_model2<-lm(formula = gmv ~ Max_sla + Dist_CustIds + PaymentType_COD + 
                   PaymentType_Prepaid + PremiumProduct + MassProduct + MarkUp_Prods + 
                   MarkDown_Prods + ListPricevsLastWeek_InflationPrcnt + OnlineMarketing_adstockv1 + 
                   Affiliates_adstockv1 + SEM_adstockv1 + Other_adstockv1, data = traindata, 
                 na.action = na.exclude)



summary(ln_ca_model2)
#Too high adjusted R-squared - 0.997
#Check VIF
as.data.frame(vif(ln_ca_model2))

#Remove Dist_CustIds
#Iterate the Process till a suitable model is found

#Model Iteration
ln_ca_model<-  lm(formula = lm(formula = gmv ~ #Max_sla + #Dist_CustIds + 
                                 PaymentType_COD + 
                                 #PaymentType_Prepaid + 
                                 PremiumProduct + #MassProduct + #MarkUp_Prods + 
                                 #MarkDown_Prods + 
                                # ListPricevsLastWeek_InflationPrcnt + #OnlineMarketing_adstockv1 + 
                                 Affiliates_adstockv1 # + 
                                 #SEM_adstockv1 #+ Other_adstockv1
                               , data = traindata, 
                               na.action = na.exclude)
                  , data = traindata, na.action = na.exclude)

#log_ca_model 
#plot(traindata$gmv,traindata$OnlineMarketing_adstockv1)
summary(ln_ca_model)
#Adjusted R-squared - 0.8689
#Check VIF
as.data.frame(vif(ln_ca_model))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(ln_ca_model,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.977
#Model R-squared: 0.8689
#quite low let's check by bootstrapping and CV
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#0.44
# plot Residuals vs Fitted
plot(ln_ca_model,pch=16,which=1,col="green")


#Seems random

#Try Bootstrapping

# bootstrapping with 1000 replications 
bootresults <- boot(data=CameraAccessory_WeeklyDatav2_ln, statistic=rsq, 
                    R=1000, formula = gmv ~ 
                      PaymentType_COD + 
                      PremiumProduct + 
                      Affiliates_adstockv1)

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#      original  bias    std. error
# t1*   0.9099219 -0.01134661  0.07097051

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ 
                       PaymentType_COD + 
                       PremiumProduct + 
                       Affiliates_adstockv1,   # model to fit
                     data = CameraAccessory_WeeklyDatav2_ln,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#    RMSE    Rsquared  MAE   
#  0.6428002  0.7683142  0.4431791

#Final model result
summary(ln_ca_model)
#Call:
#lm(formula = lm(formula = gmv ~ PaymentType_COD + PremiumProduct + 
#    Affiliates_adstockv1, data = traindata, na.action = na.exclude), 
#    data = traindata, na.action = na.exclude)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1.71618 -0.37797 -0.13865  0.02604  1.76971 
#
#Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            7.3235     0.5353  13.680 4.77e-16 ***
#PaymentType_COD        0.5964     0.1053   5.662 1.80e-06 ***
#PremiumProduct         0.6962     0.2999   2.321   0.0259 *  
#Affiliates_adstockv1   0.3588     0.1407   2.550   0.0151 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.6712 on 37 degrees of freedom
#Multiple R-squared:  0.8787,	Adjusted R-squared:  0.8689 
#F-statistic: 89.34 on 3 and 37 DF,  p-value: < 2.2e-16
#-----------------------CameraAccessory Mutiplicative MOdel-------------------------#


#-----------------------CameraAccessory KOYK MOdel-------------------------#

# Create row indices for train dataset
CameraAccessory_WeeklyDatav2_koyk<-CameraAccessory_WeeklyDatav2
CameraAccessory_WeeklyDatav2_koyk$gmv_lag1<- lag(CameraAccessory_WeeklyDatav1$gmv,1)

CameraAccessory_WeeklyDatav2_koykscaled<-cbind(scale(CameraAccessory_WeeklyDatav2_koyk[,c(-2,-21)]  , center = FALSE, scale = TRUE),CameraAccessory_WeeklyDatav2_koyk[,c(2,21)])
CameraAccessory_WeeklyDatav2_koykscaled<-na.omit(CameraAccessory_WeeklyDatav2_koykscaled)

trainindices= sample(1:nrow(CameraAccessory_WeeklyDatav2_koykscaled), 0.8*nrow(CameraAccessory_WeeklyDatav2_koykscaled))
# Create the train data set
traindata = CameraAccessory_WeeklyDatav2_koykscaled[trainindices,]
# Create test dataset
testdata = CameraAccessory_WeeklyDatav2_koykscaled[-trainindices,]

#Model_1 
model_koyk<-lm(gmv ~ ., data = traindata)
summary(model_koyk)  
#Check StepAIC
library(MASS)
step<-stepAIC(model_koyk, direction="both")
step
#Model2 from StepAIC output

koyk_ca_model2<-lm(formula = gmv ~ UniqueItemQty + Max_deliverybdays + Max_deliverycdays + 
                     Max_sla + Dist_pincode + Avg_product_mrp + Max_product_procurement_sla + 
                     PaymentType_Prepaid + PremiumProduct + MarkUp_Prods + MarkDown_Prods + 
                     Disc_Category_Low + Disc_Category_Medium + Disc_Category_High + 
                     NPS_week + IndiaHolidays + ME_InternetUsers + Avg_frequency + 
                     ListPricevsLastWeek_InflationPrcnt + Digital_adstockv1 + 
                     Sponsorship_adstockv1 + ContentMarketing_adstockv1 + OnlineMarketing_adstockv1 + 
                     SEM_adstockv1 + Radio_adstockv1 + Other_adstockv1 + gmv_lag1, 
                   data = traindata)



summary(koyk_ca_model2)
#Too high adjusted R-squared - 0.993
#Check VIF
as.data.frame(vif(koyk_ca_model2))

#Remove MarkDown_Prods
#Iterate the Process till a suitable model is found

#Model Iteration
koyk_ca_model<- lm(formula = gmv ~ #UniqueItemQty + 
                     #Max_deliverybdays + #Max_deliverycdays + 
                     #Max_sla + #Dist_pincode +
                     #Avg_product_mrp + 
                     #Max_product_procurement_sla + 
                     #PaymentType_Prepaid + 
                     PremiumProduct +# MarkUp_Prods +# MarkDown_Prods + 
                     #Disc_Category_Low + Disc_Category_Medium + #Disc_Category_High + 
                     #NPS_week + 
                     #IndiaHolidays + ME_InternetUsers + Avg_frequency + 
                     #ListPricevsLastWeek_InflationPrcnt +
                     #Digital_adstockv1 + 
                     #Sponsorship_adstockv1 + #ContentMarketing_adstockv1 + 
                     OnlineMarketing_adstockv1 #+ gmv_lag1
                     #SEM_adstockv1 + 
                     #Radio_adstockv1 + #Other_adstockv1 + 
                     , 
                   data = traindata)

#log_ca_model 
#plot(traindata$gmv,traindata$OnlineMarketing_adstockv1)
summary(koyk_ca_model)

#Adjusted R-squared - 0.7461
#Check VIF
as.data.frame(vif(koyk_ca_model))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(koyk_ca_model,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.7743
#Model R-squared: 0.7491

# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#1457531
# plot Residuals vs Fitted
plot(koyk_ca_model,pch=16,which=1,col="green")


#Seems random
#Try Bootstrapping

# bootstrapping with 1000 replications 
bootresults <- boot(data=CameraAccessory_WeeklyDatav2_koykscaled, statistic=rsq, 
                    R=1000, formula = gmv ~ 
                      PremiumProduct +
                      OnlineMarketing_adstockv1 )

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#      original  bias    std. error
# t1*  0.751094 0.005333446  0.06655884

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train( gmv ~ 
                        PremiumProduct +
                        OnlineMarketing_adstockv1,   # model to fit
                     data = CameraAccessory_WeeklyDatav2_koykscaled,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#     RMSE    Rsquared  MAE   
#  1068048  0.7538623  867021.8

#Final model result
summary(koyk_ca_model)
#Call:
#lm(formula = gmv ~ PremiumProduct + OnlineMarketing_adstockv1, 
#    data = traindata)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-2184766  -519149   -73460   482169  2076696 
#
#Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                 910835     396034   2.300 0.027040 *  
#PremiumProduct             2860715     421791   6.782 4.85e-08 ***
#OnlineMarketing_adstockv1  1570540     420080   3.739 0.000608 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 973200 on 38 degrees of freedom
#Multiple R-squared:  0.7588,	Adjusted R-squared:  0.7461 
#F-statistic: 59.78 on 2 and 38 DF,  p-value: 1.837e-12

#-----------------------CameraAccessory KOYK MOdel-------------------------#

#-----------------------CameraAccessory Distributed Lag MOdel-------------------------#
str(CameraAccessory_WeeklyDatav2_distlag) 
# Create row indices for train dataset
CameraAccessory_WeeklyDatav2_distlag<-CameraAccessory_WeeklyDatav2[,-c(3:7,8,9,16:19,20,23:26)]
CameraAccessory_WeeklyDatav2_distlag<-cbind(CameraAccessory_WeeklyDatav2_distlag,CameraAccessory_WeeklyDatav1[,c(60:61)])

#Calculate lag for Distributed lag model
CameraAccessory_WeeklyDatav2_distlag$gmv_lastweek1_lag=lag(CameraAccessory_WeeklyDatav2_distlag$gmv,1) 
CameraAccessory_WeeklyDatav2_distlag$gmv_lastweek2_lag=lag(CameraAccessory_WeeklyDatav2_distlag$gmv,2) 
CameraAccessory_WeeklyDatav2_distlag$gmv_lastweek3_lag=lag(CameraAccessory_WeeklyDatav2_distlag$gmv,3) 

CameraAccessory_WeeklyDatav2_distlag$PaymentType_COD_lastweek1_lag=lag(CameraAccessory_WeeklyDatav2_distlag$PaymentType_COD,1) 
CameraAccessory_WeeklyDatav2_distlag$PaymentType_COD_lastweek2_lag=lag(CameraAccessory_WeeklyDatav2_distlag$PaymentType_COD,2) 

CameraAccessory_WeeklyDatav2_distlag$PaymentType_Prepaid_lastweek1_lag=lag(CameraAccessory_WeeklyDatav2_distlag$PaymentType_Prepaid,1) 
CameraAccessory_WeeklyDatav2_distlag$PaymentType_Prepaid_lastweek2_lag=lag(CameraAccessory_WeeklyDatav2_distlag$PaymentType_Prepaid,2) 

CameraAccessory_WeeklyDatav2_distlag$PremiumProduct_lastweek1_lag=lag(CameraAccessory_WeeklyDatav2_distlag$PremiumProduct,1) 
CameraAccessory_WeeklyDatav2_distlag$PremiumProduct_lastweek2_lag=lag(CameraAccessory_WeeklyDatav2_distlag$PremiumProduct,2) 

CameraAccessory_WeeklyDatav2_distlag$MarkUp_Prods_lastweek1_lag=lag(CameraAccessory_WeeklyDatav2_distlag$MarkUp_Prods,1) 
CameraAccessory_WeeklyDatav2_distlag$MarkUp_Prods_lastweek2_lag=lag(CameraAccessory_WeeklyDatav2_distlag$MarkUp_Prods,2) 

CameraAccessory_WeeklyDatav2_distlag$MarkDown_Prods_lastweek1_lag=lag(CameraAccessory_WeeklyDatav2_distlag$MarkDown_Prods,1) 
CameraAccessory_WeeklyDatav2_distlag$MarkDown_Prods_lastweek2_lag=lag(CameraAccessory_WeeklyDatav2_distlag$MarkDown_Prods,2) 

CameraAccessory_WeeklyDatav2_distlag$TV_adstockv1_lastweek1_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$TV_adstockv1,1) 
CameraAccessory_WeeklyDatav2_distlag$TV_adstockv1_lastweek2_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$TV_adstockv1,2) 

CameraAccessory_WeeklyDatav2_distlag$Digital_adstockv1_lastweek1_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$Digital_adstockv1,1) 
CameraAccessory_WeeklyDatav2_distlag$Digital_adstockv1_lastweek2_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$Digital_adstockv1,2) 

CameraAccessory_WeeklyDatav2_distlag$Sponsorship_adstockv1_lastweek1_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$Sponsorship_adstockv1,1) 
CameraAccessory_WeeklyDatav2_distlag$Sponsorship_adstockv1_lastweek2_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$Sponsorship_adstockv1,2) 

CameraAccessory_WeeklyDatav2_distlag$ContentMarketing_adstockv1_lastweek1_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$ContentMarketing_adstockv1,1) 
CameraAccessory_WeeklyDatav2_distlag$ContentMarketing_adstockv1_lastweek2_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$ContentMarketing_adstockv1,2) 

CameraAccessory_WeeklyDatav2_distlag$OnlineMarketing_adstockv1_lastweek1_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$OnlineMarketing_adstockv1,1) 
CameraAccessory_WeeklyDatav2_distlag$OnlineMarketing_adstockv1_lastweek2_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$OnlineMarketing_adstockv1,2) 

CameraAccessory_WeeklyDatav2_distlag$Affiliates_adstockv1_lastweek1_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$Affiliates_adstockv1,1) 
CameraAccessory_WeeklyDatav2_distlag$Affiliates_adstockv1_lastweek2_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$Affiliates_adstockv1,2) 

CameraAccessory_WeeklyDatav2_distlag$SEM_adstockv1_lastweek1_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$SEM_adstockv1,1) 
CameraAccessory_WeeklyDatav2_distlag$SEM_adstockv1_lastweek2_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$SEM_adstockv1,2) 

CameraAccessory_WeeklyDatav2_distlag$Radio_adstockv1_lastweek1_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$Radio_adstockv1,1) 
CameraAccessory_WeeklyDatav2_distlag$Radio_adstockv1_lastweek2_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$Radio_adstockv1,2) 

CameraAccessory_WeeklyDatav2_distlag$Other_adstockv1_lastweek1_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$Other_adstockv1,1) 
CameraAccessory_WeeklyDatav2_distlag$Other_adstockv1_lastweek2_lag=stats::lag(CameraAccessory_WeeklyDatav2_distlag$Other_adstockv1,2) 



CameraAccessory_WeeklyDatav2_distlag_scaled<-cbind(scale(CameraAccessory_WeeklyDatav2_distlag[,c(-2,-9)]  , center = FALSE, scale = TRUE),CameraAccessory_WeeklyDatav2_distlag[,c(2,9)])
CameraAccessory_WeeklyDatav2_distlag_scaled<-na.omit(CameraAccessory_WeeklyDatav2_distlag_scaled)
CameraAccessory_WeeklyDatav2_distlag_scaled<-na.omit(CameraAccessory_WeeklyDatav2_distlag_scaled)
trainindices= sample(1:nrow(CameraAccessory_WeeklyDatav2_distlag_scaled), 0.8*nrow(CameraAccessory_WeeklyDatav2_distlag_scaled))
# Create the train data set
traindata = CameraAccessory_WeeklyDatav2_distlag_scaled[trainindices,]
# Create test dataset
testdata = CameraAccessory_WeeklyDatav2_distlag_scaled[-trainindices,]

#Model_1 
model_dlag<-lm(gmv ~ ., data = traindata)
summary(model_dlag)  
#Check StepAIC
library(MASS)
step<-stepAIC(model_dlag, direction="both")
step
#Model2 from StepAIC output

model_dlag2<-lm(formula = gmv ~ UniqueItemQty + PaymentType_COD + PaymentType_Prepaid + 
                  PremiumProduct + MarkUp_Prods + MarkDown_Prods + NPS_week + 
                  ListPricevsLastWeek_InflationPrcnt + TV_adstockv1 + Digital_adstockv1 + 
                  ContentMarketing_adstockv1 + Radio_adstockv1 + Other_adstockv1 + 
                  ListPricevsLast3Week_InflationPrcnt + gmv_lastweek1_lag + 
                  gmv_lastweek2_lag + gmv_lastweek3_lag + PaymentType_COD_lastweek1_lag + 
                  PaymentType_COD_lastweek2_lag + PaymentType_Prepaid_lastweek1_lag + 
                  PaymentType_Prepaid_lastweek2_lag + PremiumProduct_lastweek1_lag + 
                  PremiumProduct_lastweek2_lag + MarkUp_Prods_lastweek1_lag + 
                  MarkDown_Prods_lastweek2_lag + Dummy_Special_sale_week, data = traindata)



summary(model_dlag2)
#Too high adjusted R-squared - 0.9927
#Check VIF
as.data.frame(vif(model_dlag2))

#Remove PaymentType_COD
#Iterate the Process till a suitable model is found

#Model Iteration
model_dlag<- lm(formula = gmv ~ #UniqueItemQty + #PaymentType_COD + 
                 # PaymentType_Prepaid + 
                  PremiumProduct + #MarkUp_Prods + 
                  MarkDown_Prods + #NPS_week + 
                  ListPricevsLastWeek_InflationPrcnt + #TV_adstockv1 + 
                  #Digital_adstockv1 + 
                  #ContentMarketing_adstockv1 + Radio_adstockv1 + 
                  #Other_adstockv1 + 
                  ListPricevsLast3Week_InflationPrcnt + #gmv_lastweek1_lag + 
                  #gmv_lastweek2_lag + 
                  #gmv_lastweek3_lag + 
                  #PaymentType_COD_lastweek1_lag + 
                  #PaymentType_COD_lastweek2_lag + 
                  #PaymentType_Prepaid_lastweek1_lag + 
                  #PaymentType_Prepaid_lastweek2_lag + 
                  PremiumProduct_lastweek1_lag + 
                  PremiumProduct_lastweek2_lag #+ MarkUp_Prods_lastweek1_lag 
                  #MarkDown_Prods_lastweek2_lag + 
                #  Dummy_Special_sale_week
                , data = traindata)


#log_ca_model 
#plot(traindata$gmv,traindata$OnlineMarketing_adstockv1)
summary(model_dlag)

#Adjusted R-squared - 0.9667
#Check VIF
as.data.frame(vif(model_dlag))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(model_dlag,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.88
#Model R-squared: 0.9667
#Will check using cross validation
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#969769.7
# plot Residuals vs Fitted
plot(model_dlag,pch=16,which=1,col="green")


#Seems random
#Try Bootstrapping

# bootstrapping with 1000 replications 
bootresults <- boot(data=CameraAccessory_WeeklyDatav2_distlag_scaled, statistic=rsq, 
                    R=1000, formula = gmv ~ 
                      PremiumProduct + 
                      MarkDown_Prods + 
                      ListPricevsLastWeek_InflationPrcnt + 
                      ListPricevsLast3Week_InflationPrcnt + 
                      PremiumProduct_lastweek1_lag + 
                      PremiumProduct_lastweek2_lag 
)

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#      original  bias    std. error
# t1*   0.9633755 0.005410034  0.01109202

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train(gmv ~ 
                       PremiumProduct + 
                       MarkDown_Prods + 
                       ListPricevsLastWeek_InflationPrcnt + 
                       ListPricevsLast3Week_InflationPrcnt + 
                       PremiumProduct_lastweek1_lag + 
                       PremiumProduct_lastweek2_lag ,   # model to fit
                     data = CameraAccessory_WeeklyDatav2_distlag_scaled,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#     RMSE    Rsquared  MAE   
#   504080.3  0.9604432  391216.7

#Final model result
summary(model_dlag)
#Call:
#lm(formula = gmv ~ PremiumProduct + MarkDown_Prods + ListPricevsLastWeek_InflationPrcnt + 
#    ListPricevsLast3Week_InflationPrcnt + PremiumProduct_lastweek1_lag + 
#    PremiumProduct_lastweek2_lag, data = traindata)
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-776696 -242177  -45905  195777  931106 
#
#Coefficients:
#                                    Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                          -807405     270668  -2.983 0.005335 ** 
#PremiumProduct                       1506921     192913   7.811 5.30e-09 ***
#MarkDown_Prods                       3868267     210921  18.340  < 2e-16 ***
#ListPricevsLastWeek_InflationPrcnt    588960     121483   4.848 2.88e-05 ***
#ListPricevsLast3Week_InflationPrcnt   307536      94942   3.239 0.002734 ** 
#PremiumProduct_lastweek1_lag          410506     160078   2.564 0.015072 *  
#PremiumProduct_lastweek2_lag          574070     157360   3.648 0.000902 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 386900 on 33 degrees of freedom
#Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9667 
#F-statistic: 189.6 on 6 and 33 DF,  p-value: < 2.2e-16
#

#-----------------------CameraAccessory Distributed Lag MOdel-------------------------#


#-----------------------CameraAccessory Mutiplicative Distributed Lag MOdel-------------------------#
str(CameraAccessory_WeeklyDatav2_distlag_ln)
CameraAccessory_WeeklyDatav2_distlag_ln<-cbind(log(CameraAccessory_WeeklyDatav2_distlag[,-9]),CameraAccessory_WeeklyDatav2_distlag[,9])

colnames(CameraAccessory_WeeklyDatav2_distlag_ln)[which(names(CameraAccessory_WeeklyDatav2_distlag_ln) == "CameraAccessory_WeeklyDatav2_distlag[, 9]")] <- "Dummy_SpecialSale"
#Fix Nan and Infs

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
CameraAccessory_WeeklyDatav2_distlag_ln[is.nan(CameraAccessory_WeeklyDatav2_distlag_ln)] <- 1
which(CameraAccessory_WeeklyDatav2_distlag_ln==-Inf)
which(CameraAccessory_WeeklyDatav2_distlag_ln==Inf)
CameraAccessory_WeeklyDatav2_distlag_ln<-replace(CameraAccessory_WeeklyDatav2_distlag_ln,CameraAccessory_WeeklyDatav2_distlag_ln==-Inf,1)
CameraAccessory_WeeklyDatav2_distlag_ln<-na.omit(CameraAccessory_WeeklyDatav2_distlag_ln)

# Create row indices for train dataset
trainindices= sample(1:nrow(CameraAccessory_WeeklyDatav2_distlag_ln), 0.8*nrow(CameraAccessory_WeeklyDatav2_distlag_ln))
# Create the train data set
traindata = CameraAccessory_WeeklyDatav2_distlag_ln[trainindices,]
# Create test dataset
testdata = CameraAccessory_WeeklyDatav2_distlag_ln[-trainindices,]




#Model_1 
Dlagln_ca_model1<-lm(gmv ~ .,na.action = na.exclude, data = traindata)
summary(Dlagln_ca_model1) 
#Check StepAIC

step<-stepAIC(Dlagln_ca_model1, direction="both")
step
#Model2 from StepAIC output

Dlagln_ca_model2<-lm(formula = gmv ~ UniqueItemQty + PaymentType_COD + PaymentType_Prepaid + 
                       PremiumProduct + MassProduct + MarkUp_Prods + MarkDown_Prods + 
                       NPS_week + ListPricevsLastWeek_InflationPrcnt + TV_adstockv1 + 
                       Digital_adstockv1 + Sponsorship_adstockv1 + ContentMarketing_adstockv1 + 
                       OnlineMarketing_adstockv1 + Affiliates_adstockv1 + SEM_adstockv1 + 
                       Radio_adstockv1 + Other_adstockv1 + ListPricevsLast3Week_InflationPrcnt + 
                       gmv_lastweek1_lag + gmv_lastweek2_lag + gmv_lastweek3_lag + 
                       PaymentType_COD_lastweek1_lag + PaymentType_COD_lastweek2_lag + 
                       PremiumProduct_lastweek2_lag + MarkUp_Prods_lastweek1_lag + 
                       MarkUp_Prods_lastweek2_lag, data = traindata, na.action = na.exclude)



summary(Dlagln_ca_model2)
#Too high adjusted R-squared - 0.9989
#Check VIF
as.data.frame(vif(Dlagln_ca_model2))

#Remove MarkDown_Prods
#Iterate the Process till a suitable model is found

#Model Iteration
Dlagln_ca_model<-  lm(formula = gmv ~ #UniqueItemQty + 
                        PaymentType_COD + 
                        PaymentType_Prepaid + 
                        #PremiumProduct + #MassProduct + 
                        #MarkUp_Prods + #MarkDown_Prods + 
                        NPS_week + #ListPricevsLastWeek_InflationPrcnt +# TV_adstockv1 + 
                        #Digital_adstockv1 + 
                        Sponsorship_adstockv1 + #ContentMarketing_adstockv1 + 
                        #OnlineMarketing_adstockv1 + #Affiliates_adstockv1 + 
                        #SEM_adstockv1 + 
                        #Radio_adstockv1 + 
                        Other_adstockv1 + ListPricevsLast3Week_InflationPrcnt #+ 
                        #gmv_lastweek1_lag +
                        #gmv_lastweek2_lag + 
                       # gmv_lastweek3_lag 
                        #PaymentType_COD_lastweek1_lag + 
                        #PaymentType_COD_lastweek2_lag + 
                        #PremiumProduct_lastweek2_lag #+ MarkUp_Prods_lastweek1_lag 
                      #  MarkUp_Prods_lastweek2_lag
                      , data = traindata, na.action = na.exclude)

#Dlagln_ca_model 
#plot(traindata$gmv,traindata$OnlineMarketing_adstockv1)
summary(Dlagln_ca_model)
#Adjusted R-squared - 0.9916
#Check VIF
as.data.frame(vif(Dlagln_ca_model))
#Remove Max_deliverybdays


#Now that all variables are somewhat significant, let us check the error spread
Predict <- predict(Dlagln_ca_model,testdata)
testdata$test_gmv <- (Predict)
testdata$error<-testdata$gmv-testdata$test_gmv
cor(testdata$gmv,testdata$test_gmv)^2
#predicted R-squared: 0.9969
#Model R-squared: 0.9916
#quite low let's check by bootstrapping and CV
# compute RMSE
rmse <- sqrt(mean(testdata$error^2))
rmse
#0.26
# plot Residuals vs Fitted
plot(Dlagln_ca_model,pch=16,which=1,col="green")


#Seems random

#Try Bootstrapping

# bootstrapping with 1000 replications 
bootresults <- boot(data=CameraAccessory_WeeklyDatav2_distlag_ln, statistic=rsq, 
                    R=1000, formula = gmv ~ 
                      PaymentType_COD + 
                      PaymentType_Prepaid + 
                      NPS_week + 
                      Sponsorship_adstockv1 + 
                      Other_adstockv1 + 
                      ListPricevsLast3Week_InflationPrcnt  )

# view results
bootresults 
plot(bootresults)
# Bootstrap Statistics :
#      original  bias    std. error
# t1*  0.9928144 -0.002993932   0.0167629

# get 95% confidence interval 
boot.ci(bootresults, type="bca")

#Try Cross Validation too
#data_ctrl <- trainControl(method = "cv", number = 10)
model_caret <- train( gmv ~ 
                        PaymentType_COD + 
                        PaymentType_Prepaid + 
                        NPS_week + 
                        Sponsorship_adstockv1 + 
                        Other_adstockv1 + 
                        ListPricevsLast3Week_InflationPrcnt ,   # model to fit
                     data = CameraAccessory_WeeklyDatav2_distlag_ln,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this
model_caret
#    RMSE    Rsquared  MAE   
#  0.2030611  0.9365685  0.1581026

#Final model result
summary(Dlagln_ca_model)
#Call:
#lm(formula = gmv ~ PaymentType_COD + PaymentType_Prepaid + NPS_week + 
#    Sponsorship_adstockv1 + Other_adstockv1 + ListPricevsLast3Week_InflationPrcnt, 
#    data = traindata, na.action = na.exclude)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-0.50839 -0.06080  0.00729  0.09474  0.27842 
#
#Coefficients:
#                                    Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                         15.50008    2.30296   6.731 1.14e-07 ***
#PaymentType_COD                      0.40486    0.02906  13.930 2.24e-15 ***
#PaymentType_Prepaid                  0.72881    0.03465  21.033  < 2e-16 ***
#NPS_week                            -2.06026    0.54898  -3.753 0.000675 ***
#Sponsorship_adstockv1               -0.09931    0.04648  -2.137 0.040136 *  
#Other_adstockv1                     -0.03757    0.01776  -2.116 0.041998 *  
#ListPricevsLast3Week_InflationPrcnt  0.08345    0.01995   4.183 0.000199 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.1685 on 33 degrees of freedom
#Multiple R-squared:  0.9929,	Adjusted R-squared:  0.9916 
#F-statistic: 768.7 on 6 and 33 DF,  p-value: < 2.2e-16

#-----------------------CameraAccessory Mutiplicative Distributed Lag MOdel-------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#----------------------------------------CameraAccessory--------------------------------------------#

