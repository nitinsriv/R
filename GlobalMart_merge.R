
#------------------- Global Sales Case-Study ---------------------#

# "Global Mart" is a giant online super store having worldwide operations #
# The major product categories that it deals with are consumer, corporate & home office #
# The store caters to 7 different market segments and in 3 major categories #
# As we have to forecast at this granular level, we need to subset the data into 21 (7*3) #
# buckets before analysing the data #

# All of these 21 market buckets are not important from the store's point of view #
# We need to find out 2 most profitable (and consistent) segment from these 21 and #
# forecast the sales and demand for these segments #

#----------------------------------------------------------------#

library(dplyr)
library(ggplot2)
library(cowplot)
library(forecast)
library(tseries)

#----------------------------------------------------------------#

# read data from excel file
input_df <- read.csv("Global Superstore.csv")

# Examining the structure of the dataset #
str(input_df)

# Exploring the dataset #
summary(input_df)

#----------------------------------------------------------------#

# Dropping or removing the first columns that has information about row ids #
input_df <- input_df[,-1]


# Finding the number of columns containing NA with number of NA
# Postal.Code is the only NA column with 41296 NA observations
# Postal.Code ignored for analysis
sapply(input_df, function(x) sum(is.na(x)))


# format dates into month year format

input_df$Order.Date <- as.Date(input_df$Order.Date, "%d-%m-%Y")
input_df <- input_df[order(input_df$Order.Date),]
  
input_df$Ship.Date <- as.Date(input_df$Ship.Date, "%d-%m-%Y")
input_df <- input_df[order(input_df$Ship.Date),]


# sort dates for timeseries
input_df$Order.Date <- format(input_df$Order.Date,"%m-%Y")
input_df$Ship.Date <- format(input_df$Ship.Date,"%m-%Y")


# plot segment wise aggregate profit for all markets
# Consumer segment is the segment giving high profits in all markets.
profitable_df <- input_df[which(input_df$Profit >= 0),]
ggplot(input_df, aes(x=Market))+ geom_col(aes(y=sum(Profit),fill= Segment))+
  labs(x="Market",y="Total Profit",title="Market Segment Profit",fill="Segment")


# segment orders as per segment, market and profit
market_segment_profit <- aggregate(input_df[,c(2,7,12,21)]$Profit, by=list(input_df[,12] ,input_df[,7],input_df[,2]), FUN=sum)
colnames(market_segment_profit)<- c("Market","Segment","Order.Date","Profit")

# filter profit for Consumer Segment
market_segment_profit_Consumer <- market_segment_profit[which(market_segment_profit$Segment == "Consumer"),]

# plot quantile distibution for Market and Profit across Consumer segment
boxplot(market_segment_profit_Consumer$Profit ~ market_segment_profit_Consumer$Market)
title("Profit Distribution - Consumer Segment")



# segment orders as per segment, market and sales
market_segment_sales <- aggregate(input_df[,c(2,7,12,18)]$Sales, by=list(input_df[,12],input_df[,7],input_df[,2]), FUN=sum)
colnames(market_segment_sales)<- c("Market","Segment","Order.Date","Sales")


# segment orders as per segment, market and quantity
market_segment_quantity <- aggregate(input_df[,c(2,7,12,19)]$Quantity, by=list(input_df[,12],input_df[,7],input_df[,2]), FUN=sum)
colnames(market_segment_quantity)<- c("Market","Segment","Order.Date","Quantity")



####################### Profit #############################

#---------------------------------------------------------------#


# segment 'Consumer'for APAC market
APAC_Consumer_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="APAC") & (market_segment_profit$Segment=="Consumer"))
APAC_Consumer_profit$Order.Date <- paste("01-",APAC_Consumer_profit$Order.Date)
APAC_Consumer_profit$Order.Date <- as.Date(APAC_Consumer_profit$Order.Date,"%d-%m-%Y")
APAC_Consumer_profit <- APAC_Consumer_profit[order(APAC_Consumer_profit$Order.Date),]
APAC_Consumer_profit$Order.Date <- format(APAC_Consumer_profit$Order.Date,"%m-%Y")

# segment 'Consumer'for Africa market
Africa_Consumer_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="Africa") & (market_segment_profit$Segment=="Consumer"))
Africa_Consumer_profit$Order.Date <- paste("01-",Africa_Consumer_profit$Order.Date)
Africa_Consumer_profit$Order.Date <- as.Date(Africa_Consumer_profit$Order.Date,"%d-%m-%Y")
Africa_Consumer_profit <- Africa_Consumer_profit[order(Africa_Consumer_profit$Order.Date),]
Africa_Consumer_profit$Order.Date <- format(Africa_Consumer_profit$Order.Date,"%m-%Y")

# segment 'Consumer'for EU market
EU_Consumer_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="EU") & (market_segment_profit$Segment=="Consumer"))
EU_Consumer_profit$Order.Date <- paste("01-",EU_Consumer_profit$Order.Date)
EU_Consumer_profit$Order.Date <- as.Date(EU_Consumer_profit$Order.Date,"%d-%m-%Y")
EU_Consumer_profit <- EU_Consumer_profit[order(EU_Consumer_profit$Order.Date),]
EU_Consumer_profit$Order.Date <- format(EU_Consumer_profit$Order.Date,"%m-%Y")

# segment 'Consumer'for Canada market
Canada_Consumer_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="Canada") & (market_segment_profit$Segment=="Consumer"))
Canada_Consumer_profit$Order.Date <- paste("01-",Canada_Consumer_profit$Order.Date)
Canada_Consumer_profit$Order.Date <- as.Date(Canada_Consumer_profit$Order.Date,"%d-%m-%Y")
Canada_Consumer_profit <- Canada_Consumer_profit[order(Canada_Consumer_profit$Order.Date),]
Canada_Consumer_profit$Order.Date <- format(Canada_Consumer_profit$Order.Date,"%m-%Y")

# segment 'Consumer'for EMEA market
EMEA_Consumer_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="EMEA") & (market_segment_profit$Segment=="Consumer"))
EMEA_Consumer_profit$Order.Date <- paste("01-",EMEA_Consumer_profit$Order.Date)
EMEA_Consumer_profit$Order.Date <- as.Date(EMEA_Consumer_profit$Order.Date,"%d-%m-%Y")
EMEA_Consumer_profit <- EMEA_Consumer_profit[order(EMEA_Consumer_profit$Order.Date),]
EMEA_Consumer_profit$Order.Date <- format(EMEA_Consumer_profit$Order.Date,"%m-%Y")

# segment 'Consumer'for LATAM market
LATAM_Consumer_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="LATAM") & (market_segment_profit$Segment=="Consumer"))
LATAM_Consumer_profit$Order.Date <- paste("01-",LATAM_Consumer_profit$Order.Date)
LATAM_Consumer_profit$Order.Date <- as.Date(LATAM_Consumer_profit$Order.Date,"%d-%m-%Y")
LATAM_Consumer_profit <- LATAM_Consumer_profit[order(LATAM_Consumer_profit$Order.Date),]
LATAM_Consumer_profit$Order.Date <- format(LATAM_Consumer_profit$Order.Date,"%m-%Y")

# segment 'Consumer'for US market
US_Consumer_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="US") & (market_segment_profit$Segment=="Consumer"))
US_Consumer_profit$Order.Date <- paste("01-",US_Consumer_profit$Order.Date)
US_Consumer_profit$Order.Date <- as.Date(US_Consumer_profit$Order.Date,"%d-%m-%Y")
US_Consumer_profit <- US_Consumer_profit[order(US_Consumer_profit$Order.Date),]
US_Consumer_profit$Order.Date <- format(US_Consumer_profit$Order.Date,"%m-%Y")

#---------------------------------------------------------------#


# segment 'Corporate'for APAC market
APAC_Corporate_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="APAC") & (market_segment_profit$Segment=="Corporate"))
APAC_Corporate_profit$Order.Date <- paste("01-",APAC_Corporate_profit$Order.Date)
APAC_Corporate_profit$Order.Date <- as.Date(APAC_Corporate_profit$Order.Date,"%d-%m-%Y")
APAC_Corporate_profit <- APAC_Corporate_profit[order(APAC_Corporate_profit$Order.Date),]
APAC_Corporate_profit$Order.Date <- format(APAC_Corporate_profit$Order.Date,"%m-%Y")

# segment 'Corporate'for Africa market
Africa_Corporate_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="Africa") & (market_segment_profit$Segment=="Corporate"))
Africa_Corporate_profit$Order.Date <- paste("01-",Africa_Corporate_profit$Order.Date)
Africa_Corporate_profit$Order.Date <- as.Date(Africa_Corporate_profit$Order.Date,"%d-%m-%Y")
Africa_Corporate_profit <- Africa_Corporate_profit[order(Africa_Corporate_profit$Order.Date),]
Africa_Corporate_profit$Order.Date <- format(Africa_Corporate_profit$Order.Date,"%m-%Y")

# segment 'Corporate'for EU market
EU_Corporate_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="EU") & (market_segment_profit$Segment=="Corporate"))
EU_Corporate_profit$Order.Date <- paste("01-",EU_Corporate_profit$Order.Date)
EU_Corporate_profit$Order.Date <- as.Date(EU_Corporate_profit$Order.Date,"%d-%m-%Y")
EU_Corporate_profit <- EU_Corporate_profit[order(EU_Corporate_profit$Order.Date),]
EU_Corporate_profit$Order.Date <- format(EU_Corporate_profit$Order.Date,"%m-%Y")

# segment 'Corporate'for Canada market
Canada_Corporate_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="Canada") & (market_segment_profit$Segment=="Corporate"))
Canada_Corporate_profit$Order.Date <- paste("01-",Canada_Corporate_profit$Order.Date)
Canada_Corporate_profit$Order.Date <- as.Date(Canada_Corporate_profit$Order.Date,"%d-%m-%Y")
Canada_Corporate_profit <- Canada_Corporate_profit[order(Canada_Corporate_profit$Order.Date),]
Canada_Corporate_profit$Order.Date <- format(Canada_Corporate_profit$Order.Date,"%m-%Y")

# segment 'Corporate'for EMEA market
EMEA_Corporate_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="EMEA") & (market_segment_profit$Segment=="Corporate"))
EMEA_Corporate_profit$Order.Date <- paste("01-",EMEA_Corporate_profit$Order.Date)
EMEA_Corporate_profit$Order.Date <- as.Date(EMEA_Corporate_profit$Order.Date,"%d-%m-%Y")
EMEA_Corporate_profit <- EMEA_Corporate_profit[order(EMEA_Corporate_profit$Order.Date),]
EMEA_Corporate_profit$Order.Date <- format(EMEA_Corporate_profit$Order.Date,"%m-%Y")

# segment 'Corporate'for LATAM market
LATAM_Corporate_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="LATAM") & (market_segment_profit$Segment=="Corporate"))
LATAM_Corporate_profit$Order.Date <- paste("01-",LATAM_Corporate_profit$Order.Date)
LATAM_Corporate_profit$Order.Date <- as.Date(LATAM_Corporate_profit$Order.Date,"%d-%m-%Y")
LATAM_Corporate_profit <- LATAM_Corporate_profit[order(LATAM_Corporate_profit$Order.Date),]
LATAM_Corporate_profit$Order.Date <- format(LATAM_Corporate_profit$Order.Date,"%m-%Y")

# segment 'Corporate'for US market
US_Corporate_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="US") & (market_segment_profit$Segment=="Corporate"))
US_Corporate_profit$Order.Date <- paste("01-",US_Corporate_profit$Order.Date)
US_Corporate_profit$Order.Date <- as.Date(US_Corporate_profit$Order.Date,"%d-%m-%Y")
US_Corporate_profit <- US_Corporate_profit[order(US_Corporate_profit$Order.Date),]
US_Corporate_profit$Order.Date <- format(US_Corporate_profit$Order.Date,"%m-%Y")

#---------------------------------------------------------------#


# segment 'Home Office'for APAC market
APAC_HomeOffice_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="APAC") & (market_segment_profit$Segment=="Home Office"))
APAC_HomeOffice_profit$Order.Date <- paste("01-",APAC_HomeOffice_profit$Order.Date)
APAC_HomeOffice_profit$Order.Date <- as.Date(APAC_HomeOffice_profit$Order.Date,"%d-%m-%Y")
APAC_HomeOffice_profit <- APAC_HomeOffice_profit[order(APAC_HomeOffice_profit$Order.Date),]
APAC_HomeOffice_profit$Order.Date <- format(APAC_HomeOffice_profit$Order.Date,"%m-%Y")

# segment 'Home Office'for Africa market
Africa_HomeOffice_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="Africa") & (market_segment_profit$Segment=="Home Office"))
Africa_HomeOffice_profit$Order.Date <- paste("01-",Africa_HomeOffice_profit$Order.Date)
Africa_HomeOffice_profit$Order.Date <- as.Date(Africa_HomeOffice_profit$Order.Date,"%d-%m-%Y")
Africa_HomeOffice_profit <- Africa_HomeOffice_profit[order(Africa_HomeOffice_profit$Order.Date),]
Africa_HomeOffice_profit$Order.Date <- format(Africa_HomeOffice_profit$Order.Date,"%m-%Y")

# segment 'Home Office'for EU market
EU_HomeOffice_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="EU") & (market_segment_profit$Segment=="Home Office"))
EU_HomeOffice_profit$Order.Date <- paste("01-",EU_HomeOffice_profit$Order.Date)
EU_HomeOffice_profit$Order.Date <- as.Date(EU_HomeOffice_profit$Order.Date,"%d-%m-%Y")
EU_HomeOffice_profit <- EU_HomeOffice_profit[order(EU_HomeOffice_profit$Order.Date),]
EU_HomeOffice_profit$Order.Date <- format(EU_HomeOffice_profit$Order.Date,"%m-%Y")

# segment 'Home Office'for Canada market
Canada_HomeOffice_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="Canada") & (market_segment_profit$Segment=="Home Office"))
Canada_HomeOffice_profit$Order.Date <- paste("01-",Canada_HomeOffice_profit$Order.Date)
Canada_HomeOffice_profit$Order.Date <- as.Date(Canada_HomeOffice_profit$Order.Date,"%d-%m-%Y")
Canada_HomeOffice_profit <- Canada_HomeOffice_profit[order(Canada_HomeOffice_profit$Order.Date),]
Canada_HomeOffice_profit$Order.Date <- format(Canada_HomeOffice_profit$Order.Date,"%m-%Y")

# segment 'Home Office'for EMEA market
EMEA_HomeOffice_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="EMEA") & (market_segment_profit$Segment=="Home Office"))
EMEA_HomeOffice_profit$Order.Date <- paste("01-",EMEA_HomeOffice_profit$Order.Date)
EMEA_HomeOffice_profit$Order.Date <- as.Date(EMEA_HomeOffice_profit$Order.Date,"%d-%m-%Y")
EMEA_HomeOffice_profit <- EMEA_HomeOffice_profit[order(EMEA_HomeOffice_profit$Order.Date),]
EMEA_HomeOffice_profit$Order.Date <- format(EMEA_HomeOffice_profit$Order.Date,"%m-%Y")

# segment 'Home Office'for LATAM market
LATAM_HomeOffice_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="LATAM") & (market_segment_profit$Segment=="Home Office"))
LATAM_HomeOffice_profit$Order.Date <- paste("01-",LATAM_HomeOffice_profit$Order.Date)
LATAM_HomeOffice_profit$Order.Date <- as.Date(LATAM_HomeOffice_profit$Order.Date,"%d-%m-%Y")
LATAM_HomeOffice_profit <- LATAM_HomeOffice_profit[order(LATAM_HomeOffice_profit$Order.Date),]
LATAM_HomeOffice_profit$Order.Date <- format(LATAM_HomeOffice_profit$Order.Date,"%m-%Y")

# segment 'Home Office'for US market
US_HomeOffice_profit <- market_segment_profit %>% filter((market_segment_profit$Market=="US") & (market_segment_profit$Segment=="Home Office"))
US_HomeOffice_profit$Order.Date <- paste("01-",US_HomeOffice_profit$Order.Date)
US_HomeOffice_profit$Order.Date <- as.Date(US_HomeOffice_profit$Order.Date,"%d-%m-%Y")
US_HomeOffice_profit <- US_HomeOffice_profit[order(US_HomeOffice_profit$Order.Date),]
US_HomeOffice_profit$Order.Date <- format(US_HomeOffice_profit$Order.Date,"%m-%Y")

#---------------------------------------------------------------#


####################### Sales #############################

#---------------------------------------------------------------#

# segment 'Consumer'for APAC market
APAC_Consumer_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="APAC") & (market_segment_sales$Segment=="Consumer"))
APAC_Consumer_sales$Order.Date <- paste("01-",APAC_Consumer_sales$Order.Date)
APAC_Consumer_sales$Order.Date <- as.Date(APAC_Consumer_sales$Order.Date,"%d-%m-%Y")
APAC_Consumer_sales <- APAC_Consumer_sales[order(APAC_Consumer_sales$Order.Date),]
APAC_Consumer_sales$Order.Date <- format(APAC_Consumer_sales$Order.Date,"%m-%Y")

# segment 'Consumer'for Africa market
Africa_Consumer_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="Africa") & (market_segment_sales$Segment=="Consumer"))
Africa_Consumer_sales$Order.Date <- paste("01-",Africa_Consumer_sales$Order.Date)
Africa_Consumer_sales$Order.Date <- as.Date(Africa_Consumer_sales$Order.Date,"%d-%m-%Y")
Africa_Consumer_sales <- Africa_Consumer_sales[order(Africa_Consumer_sales$Order.Date),]
Africa_Consumer_sales$Order.Date <- format(Africa_Consumer_sales$Order.Date,"%m-%Y")

# segment 'Consumer'for EU market
EU_Consumer_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="EU") & (market_segment_sales$Segment=="Consumer"))
EU_Consumer_sales$Order.Date <- paste("01-",EU_Consumer_sales$Order.Date)
EU_Consumer_sales$Order.Date <- as.Date(EU_Consumer_sales$Order.Date,"%d-%m-%Y")
EU_Consumer_sales <- EU_Consumer_sales[order(EU_Consumer_sales$Order.Date),]
EU_Consumer_sales$Order.Date <- format(EU_Consumer_sales$Order.Date,"%m-%Y")

# segment 'Consumer'for Canada market
Canada_Consumer_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="Canada") & (market_segment_sales$Segment=="Consumer"))
Canada_Consumer_sales$Order.Date <- paste("01-",Canada_Consumer_sales$Order.Date)
Canada_Consumer_sales$Order.Date <- as.Date(Canada_Consumer_sales$Order.Date,"%d-%m-%Y")
Canada_Consumer_sales <- Canada_Consumer_sales[order(Canada_Consumer_sales$Order.Date),]
Canada_Consumer_sales$Order.Date <- format(Canada_Consumer_sales$Order.Date,"%m-%Y")

# segment 'Consumer'for EMEA market
EMEA_Consumer_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="EMEA") & (market_segment_sales$Segment=="Consumer"))
EMEA_Consumer_sales$Order.Date <- paste("01-",EMEA_Consumer_sales$Order.Date)
EMEA_Consumer_sales$Order.Date <- as.Date(EMEA_Consumer_sales$Order.Date,"%d-%m-%Y")
EMEA_Consumer_sales <- EMEA_Consumer_sales[order(EMEA_Consumer_sales$Order.Date),]
EMEA_Consumer_sales$Order.Date <- format(EMEA_Consumer_sales$Order.Date,"%m-%Y")

# segment 'Consumer'for LATAM market
LATAM_Consumer_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="LATAM") & (market_segment_sales$Segment=="Consumer"))
LATAM_Consumer_sales$Order.Date <- paste("01-",LATAM_Consumer_sales$Order.Date)
LATAM_Consumer_sales$Order.Date <- as.Date(LATAM_Consumer_sales$Order.Date,"%d-%m-%Y")
LATAM_Consumer_sales <- LATAM_Consumer_sales[order(LATAM_Consumer_sales$Order.Date),]
LATAM_Consumer_sales$Order.Date <- format(LATAM_Consumer_sales$Order.Date,"%m-%Y")

# segment 'Consumer'for US market
US_Consumer_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="US") & (market_segment_sales$Segment=="Consumer"))
US_Consumer_sales$Order.Date <- paste("01-",US_Consumer_sales$Order.Date)
US_Consumer_sales$Order.Date <- as.Date(US_Consumer_sales$Order.Date,"%d-%m-%Y")
US_Consumer_sales <- US_Consumer_sales[order(US_Consumer_sales$Order.Date),]
US_Consumer_sales$Order.Date <- format(US_Consumer_sales$Order.Date,"%m-%Y")

#---------------------------------------------------------------#


# segment 'Corporate'for APAC market
APAC_Corporate_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="APAC") & (market_segment_sales$Segment=="Corporate"))
APAC_Corporate_sales$Order.Date <- paste("01-",APAC_Corporate_sales$Order.Date)
APAC_Corporate_sales$Order.Date <- as.Date(APAC_Corporate_sales$Order.Date,"%d-%m-%Y")
APAC_Corporate_sales <- APAC_Corporate_sales[order(APAC_Corporate_sales$Order.Date),]
APAC_Corporate_sales$Order.Date <- format(APAC_Corporate_sales$Order.Date,"%m-%Y")

# segment 'Corporate'for Africa market
Africa_Corporate_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="Africa") & (market_segment_sales$Segment=="Corporate"))
Africa_Corporate_sales$Order.Date <- paste("01-",Africa_Corporate_sales$Order.Date)
Africa_Corporate_sales$Order.Date <- as.Date(Africa_Corporate_sales$Order.Date,"%d-%m-%Y")
Africa_Corporate_sales <- Africa_Corporate_sales[order(Africa_Corporate_sales$Order.Date),]
Africa_Corporate_sales$Order.Date <- format(Africa_Corporate_sales$Order.Date,"%m-%Y")

# segment 'Corporate'for EU market
EU_Corporate_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="EU") & (market_segment_sales$Segment=="Corporate"))
EU_Corporate_sales$Order.Date <- paste("01-",EU_Corporate_sales$Order.Date)
EU_Corporate_sales$Order.Date <- as.Date(EU_Corporate_sales$Order.Date,"%d-%m-%Y")
EU_Corporate_sales <- EU_Corporate_sales[order(EU_Corporate_sales$Order.Date),]
EU_Corporate_sales$Order.Date <- format(EU_Corporate_sales$Order.Date,"%m-%Y")

# segment 'Corporate'for Canada market
Canada_Corporate_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="Canada") & (market_segment_sales$Segment=="Corporate"))
Canada_Corporate_sales$Order.Date <- paste("01-",Canada_Corporate_sales$Order.Date)
Canada_Corporate_sales$Order.Date <- as.Date(Canada_Corporate_sales$Order.Date,"%d-%m-%Y")
Canada_Corporate_sales <- Canada_Corporate_sales[order(Canada_Corporate_sales$Order.Date),]
Canada_Corporate_sales$Order.Date <- format(Canada_Corporate_sales$Order.Date,"%m-%Y")

# segment 'Corporate'for EMEA market
EMEA_Corporate_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="EMEA") & (market_segment_sales$Segment=="Corporate"))
EMEA_Corporate_sales$Order.Date <- paste("01-",EMEA_Corporate_sales$Order.Date)
EMEA_Corporate_sales$Order.Date <- as.Date(EMEA_Corporate_sales$Order.Date,"%d-%m-%Y")
EMEA_Corporate_sales <- EMEA_Corporate_sales[order(EMEA_Corporate_sales$Order.Date),]
EMEA_Corporate_sales$Order.Date <- format(EMEA_Corporate_sales$Order.Date,"%m-%Y")

# segment 'Corporate'for LATAM market
LATAM_Corporate_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="LATAM") & (market_segment_sales$Segment=="Corporate"))
LATAM_Corporate_sales$Order.Date <- paste("01-",LATAM_Corporate_sales$Order.Date)
LATAM_Corporate_sales$Order.Date <- as.Date(LATAM_Corporate_sales$Order.Date,"%d-%m-%Y")
LATAM_Corporate_sales <- LATAM_Corporate_sales[order(LATAM_Corporate_sales$Order.Date),]
LATAM_Corporate_sales$Order.Date <- format(LATAM_Corporate_sales$Order.Date,"%m-%Y")

# segment 'Corporate'for US market
US_Corporate_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="US") & (market_segment_sales$Segment=="Corporate"))
US_Corporate_sales$Order.Date <- paste("01-",US_Corporate_sales$Order.Date)
US_Corporate_sales$Order.Date <- as.Date(US_Corporate_sales$Order.Date,"%d-%m-%Y")
US_Corporate_sales <- US_Corporate_sales[order(US_Corporate_sales$Order.Date),]
US_Corporate_sales$Order.Date <- format(US_Corporate_sales$Order.Date,"%m-%Y")

#---------------------------------------------------------------#


# segment 'Home Office'for APAC market
APAC_HomeOffice_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="APAC") & (market_segment_sales$Segment=="Home Office"))
APAC_HomeOffice_sales$Order.Date <- paste("01-",APAC_HomeOffice_sales$Order.Date)
APAC_HomeOffice_sales$Order.Date <- as.Date(APAC_HomeOffice_sales$Order.Date,"%d-%m-%Y")
APAC_HomeOffice_sales <- APAC_HomeOffice_sales[order(APAC_HomeOffice_sales$Order.Date),]
APAC_HomeOffice_sales$Order.Date <- format(APAC_HomeOffice_sales$Order.Date,"%m-%Y")

# segment 'Home Office'for Africa market
Africa_HomeOffice_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="Africa") & (market_segment_sales$Segment=="Home Office"))
Africa_HomeOffice_sales$Order.Date <- paste("01-",Africa_HomeOffice_sales$Order.Date)
Africa_HomeOffice_sales$Order.Date <- as.Date(Africa_HomeOffice_sales$Order.Date,"%d-%m-%Y")
Africa_HomeOffice_sales <- Africa_HomeOffice_sales[order(Africa_HomeOffice_sales$Order.Date),]
Africa_HomeOffice_sales$Order.Date <- format(Africa_HomeOffice_sales$Order.Date,"%m-%Y")

# segment 'Home Office'for EU market
EU_HomeOffice_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="EU") & (market_segment_sales$Segment=="Home Office"))
EU_HomeOffice_sales$Order.Date <- paste("01-",EU_HomeOffice_sales$Order.Date)
EU_HomeOffice_sales$Order.Date <- as.Date(EU_HomeOffice_sales$Order.Date,"%d-%m-%Y")
EU_HomeOffice_sales <- EU_HomeOffice_sales[order(EU_HomeOffice_sales$Order.Date),]
EU_HomeOffice_sales$Order.Date <- format(EU_HomeOffice_sales$Order.Date,"%m-%Y")

# segment 'Home Office'for Canada market
Canada_HomeOffice_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="Canada") & (market_segment_sales$Segment=="Home Office"))
Canada_HomeOffice_sales$Order.Date <- paste("01-",Canada_HomeOffice_sales$Order.Date)
Canada_HomeOffice_sales$Order.Date <- as.Date(Canada_HomeOffice_sales$Order.Date,"%d-%m-%Y")
Canada_HomeOffice_sales <- Canada_HomeOffice_sales[order(Canada_HomeOffice_sales$Order.Date),]
Canada_HomeOffice_sales$Order.Date <- format(Canada_HomeOffice_sales$Order.Date,"%m-%Y")

# segment 'Home Office'for EMEA market
EMEA_HomeOffice_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="EMEA") & (market_segment_sales$Segment=="Home Office"))
EMEA_HomeOffice_sales$Order.Date <- paste("01-",EMEA_HomeOffice_sales$Order.Date)
EMEA_HomeOffice_sales$Order.Date <- as.Date(EMEA_HomeOffice_sales$Order.Date,"%d-%m-%Y")
EMEA_HomeOffice_sales <- EMEA_HomeOffice_sales[order(EMEA_HomeOffice_sales$Order.Date),]
EMEA_HomeOffice_sales$Order.Date <- format(EMEA_HomeOffice_sales$Order.Date,"%m-%Y")

# segment 'Home Office'for LATAM market
LATAM_HomeOffice_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="LATAM") & (market_segment_sales$Segment=="Home Office"))
LATAM_HomeOffice_sales$Order.Date <- paste("01-",LATAM_HomeOffice_sales$Order.Date)
LATAM_HomeOffice_sales$Order.Date <- as.Date(LATAM_HomeOffice_sales$Order.Date,"%d-%m-%Y")
LATAM_HomeOffice_sales <- LATAM_HomeOffice_sales[order(LATAM_HomeOffice_sales$Order.Date),]
LATAM_HomeOffice_sales$Order.Date <- format(LATAM_HomeOffice_sales$Order.Date,"%m-%Y")

# segment 'Home Office'for US market
US_HomeOffice_sales <- market_segment_sales %>% filter((market_segment_sales$Market=="US") & (market_segment_sales$Segment=="Home Office"))
US_HomeOffice_sales$Order.Date <- paste("01-",US_HomeOffice_sales$Order.Date)
US_HomeOffice_sales$Order.Date <- as.Date(US_HomeOffice_sales$Order.Date,"%d-%m-%Y")
US_HomeOffice_sales <- US_HomeOffice_sales[order(US_HomeOffice_sales$Order.Date),]
US_HomeOffice_sales$Order.Date <- format(US_HomeOffice_sales$Order.Date,"%m-%Y")

#---------------------------------------------------------------#


####################### Quantity #############################


#---------------------------------------------------------------#

# segment 'Consumer'for APAC market
APAC_Consumer_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="APAC") & (market_segment_quantity$Segment=="Consumer"))
APAC_Consumer_quantity$Order.Date <- paste("01-",APAC_Consumer_quantity$Order.Date)
APAC_Consumer_quantity$Order.Date <- as.Date(APAC_Consumer_quantity$Order.Date,"%d-%m-%Y")
APAC_Consumer_quantity <- APAC_Consumer_quantity[order(APAC_Consumer_quantity$Order.Date),]
APAC_Consumer_quantity$Order.Date <- format(APAC_Consumer_quantity$Order.Date,"%m-%Y")

# segment 'Consumer'for Africa market
Africa_Consumer_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="Africa") & (market_segment_quantity$Segment=="Consumer"))
Africa_Consumer_quantity$Order.Date <- paste("01-",Africa_Consumer_quantity$Order.Date)
Africa_Consumer_quantity$Order.Date <- as.Date(Africa_Consumer_quantity$Order.Date,"%d-%m-%Y")
Africa_Consumer_quantity <- Africa_Consumer_quantity[order(Africa_Consumer_quantity$Order.Date),]
Africa_Consumer_quantity$Order.Date <- format(Africa_Consumer_quantity$Order.Date,"%m-%Y")

# segment 'Consumer'for EU market
EU_Consumer_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="EU") & (market_segment_quantity$Segment=="Consumer"))
EU_Consumer_quantity$Order.Date <- paste("01-",EU_Consumer_quantity$Order.Date)
EU_Consumer_quantity$Order.Date <- as.Date(EU_Consumer_quantity$Order.Date,"%d-%m-%Y")
EU_Consumer_quantity <- EU_Consumer_quantity[order(EU_Consumer_quantity$Order.Date),]
EU_Consumer_quantity$Order.Date <- format(EU_Consumer_quantity$Order.Date,"%m-%Y")

# segment 'Consumer'for Canada market
Canada_Consumer_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="Canada") & (market_segment_quantity$Segment=="Consumer"))
Canada_Consumer_quantity$Order.Date <- paste("01-",Canada_Consumer_quantity$Order.Date)
Canada_Consumer_quantity$Order.Date <- as.Date(Canada_Consumer_quantity$Order.Date,"%d-%m-%Y")
Canada_Consumer_quantity <- Canada_Consumer_quantity[order(Canada_Consumer_quantity$Order.Date),]
Canada_Consumer_quantity$Order.Date <- format(Canada_Consumer_quantity$Order.Date,"%m-%Y")

# segment 'Consumer'for EMEA market
EMEA_Consumer_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="EMEA") & (market_segment_quantity$Segment=="Consumer"))
EMEA_Consumer_quantity$Order.Date <- paste("01-",EMEA_Consumer_quantity$Order.Date)
EMEA_Consumer_quantity$Order.Date <- as.Date(EMEA_Consumer_quantity$Order.Date,"%d-%m-%Y")
EMEA_Consumer_quantity <- EMEA_Consumer_quantity[order(EMEA_Consumer_quantity$Order.Date),]
EMEA_Consumer_quantity$Order.Date <- format(EMEA_Consumer_quantity$Order.Date,"%m-%Y")

# segment 'Consumer'for LATAM market
LATAM_Consumer_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="LATAM") & (market_segment_quantity$Segment=="Consumer"))
LATAM_Consumer_quantity$Order.Date <- paste("01-",LATAM_Consumer_quantity$Order.Date)
LATAM_Consumer_quantity$Order.Date <- as.Date(LATAM_Consumer_quantity$Order.Date,"%d-%m-%Y")
LATAM_Consumer_quantity <- LATAM_Consumer_quantity[order(LATAM_Consumer_quantity$Order.Date),]
LATAM_Consumer_quantity$Order.Date <- format(LATAM_Consumer_quantity$Order.Date,"%m-%Y")

# segment 'Consumer'for US market
US_Consumer_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="US") & (market_segment_quantity$Segment=="Consumer"))
US_Consumer_quantity$Order.Date <- paste("01-",US_Consumer_quantity$Order.Date)
US_Consumer_quantity$Order.Date <- as.Date(US_Consumer_quantity$Order.Date,"%d-%m-%Y")
US_Consumer_quantity <- US_Consumer_quantity[order(US_Consumer_quantity$Order.Date),]
US_Consumer_quantity$Order.Date <- format(US_Consumer_quantity$Order.Date,"%m-%Y")

#---------------------------------------------------------------#


# segment 'Corporate'for APAC market
APAC_Corporate_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="APAC") & (market_segment_quantity$Segment=="Corporate"))
APAC_Corporate_quantity$Order.Date <- paste("01-",APAC_Corporate_quantity$Order.Date)
APAC_Corporate_quantity$Order.Date <- as.Date(APAC_Corporate_quantity$Order.Date,"%d-%m-%Y")
APAC_Corporate_quantity <- APAC_Corporate_quantity[order(APAC_Corporate_quantity$Order.Date),]
APAC_Corporate_quantity$Order.Date <- format(APAC_Corporate_quantity$Order.Date,"%m-%Y")

# segment 'Corporate'for Africa market
Africa_Corporate_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="Africa") & (market_segment_quantity$Segment=="Corporate"))
Africa_Corporate_quantity$Order.Date <- paste("01-",Africa_Corporate_quantity$Order.Date)
Africa_Corporate_quantity$Order.Date <- as.Date(Africa_Corporate_quantity$Order.Date,"%d-%m-%Y")
Africa_Corporate_quantity <- Africa_Corporate_quantity[order(Africa_Corporate_quantity$Order.Date),]
Africa_Corporate_quantity$Order.Date <- format(Africa_Corporate_quantity$Order.Date,"%m-%Y")

# segment 'Corporate'for EU market
EU_Corporate_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="EU") & (market_segment_quantity$Segment=="Corporate"))
EU_Corporate_quantity$Order.Date <- paste("01-",EU_Corporate_quantity$Order.Date)
EU_Corporate_quantity$Order.Date <- as.Date(EU_Corporate_quantity$Order.Date,"%d-%m-%Y")
EU_Corporate_quantity <- EU_Corporate_quantity[order(EU_Corporate_quantity$Order.Date),]
EU_Corporate_quantity$Order.Date <- format(EU_Corporate_quantity$Order.Date,"%m-%Y")

# segment 'Corporate'for Canada market
Canada_Corporate_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="Canada") & (market_segment_quantity$Segment=="Corporate"))
Canada_Corporate_quantity$Order.Date <- paste("01-",Canada_Corporate_quantity$Order.Date)
Canada_Corporate_quantity$Order.Date <- as.Date(Canada_Corporate_quantity$Order.Date,"%d-%m-%Y")
Canada_Corporate_quantity <- Canada_Corporate_quantity[order(Canada_Corporate_quantity$Order.Date),]
Canada_Corporate_quantity$Order.Date <- format(Canada_Corporate_quantity$Order.Date,"%m-%Y")

# segment 'Corporate'for EMEA market
EMEA_Corporate_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="EMEA") & (market_segment_quantity$Segment=="Corporate"))
EMEA_Corporate_quantity$Order.Date <- paste("01-",EMEA_Corporate_quantity$Order.Date)
EMEA_Corporate_quantity$Order.Date <- as.Date(EMEA_Corporate_quantity$Order.Date,"%d-%m-%Y")
EMEA_Corporate_quantity <- EMEA_Corporate_quantity[order(EMEA_Corporate_quantity$Order.Date),]
EMEA_Corporate_quantity$Order.Date <- format(EMEA_Corporate_quantity$Order.Date,"%m-%Y")

# segment 'Corporate'for LATAM market
LATAM_Corporate_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="LATAM") & (market_segment_quantity$Segment=="Corporate"))
LATAM_Corporate_quantity$Order.Date <- paste("01-",LATAM_Corporate_quantity$Order.Date)
LATAM_Corporate_quantity$Order.Date <- as.Date(LATAM_Corporate_quantity$Order.Date,"%d-%m-%Y")
LATAM_Corporate_quantity <- LATAM_Corporate_quantity[order(LATAM_Corporate_quantity$Order.Date),]
LATAM_Corporate_quantity$Order.Date <- format(LATAM_Corporate_quantity$Order.Date,"%m-%Y")

# segment 'Corporate'for US market
US_Corporate_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="US") & (market_segment_quantity$Segment=="Corporate"))
US_Corporate_quantity$Order.Date <- paste("01-",US_Corporate_quantity$Order.Date)
US_Corporate_quantity$Order.Date <- as.Date(US_Corporate_quantity$Order.Date,"%d-%m-%Y")
US_Corporate_quantity <- US_Corporate_quantity[order(US_Corporate_quantity$Order.Date),]
US_Corporate_quantity$Order.Date <- format(US_Corporate_quantity$Order.Date,"%m-%Y")

#---------------------------------------------------------------#


# segment 'Home Office'for APAC market
APAC_HomeOffice_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="APAC") & (market_segment_quantity$Segment=="Home Office"))
APAC_HomeOffice_quantity$Order.Date <- paste("01-",APAC_HomeOffice_quantity$Order.Date)
APAC_HomeOffice_quantity$Order.Date <- as.Date(APAC_HomeOffice_quantity$Order.Date,"%d-%m-%Y")
APAC_HomeOffice_quantity <- APAC_HomeOffice_quantity[order(APAC_HomeOffice_quantity$Order.Date),]
APAC_HomeOffice_quantity$Order.Date <- format(APAC_HomeOffice_quantity$Order.Date,"%m-%Y")

# segment 'Home Office'for Africa market
Africa_HomeOffice_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="Africa") & (market_segment_quantity$Segment=="Home Office"))
Africa_HomeOffice_quantity$Order.Date <- paste("01-",Africa_HomeOffice_quantity$Order.Date)
Africa_HomeOffice_quantity$Order.Date <- as.Date(Africa_HomeOffice_quantity$Order.Date,"%d-%m-%Y")
Africa_HomeOffice_quantity <- Africa_HomeOffice_quantity[order(Africa_HomeOffice_quantity$Order.Date),]
Africa_HomeOffice_quantity$Order.Date <- format(Africa_HomeOffice_quantity$Order.Date,"%m-%Y")

# segment 'Home Office'for EU market
EU_HomeOffice_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="EU") & (market_segment_quantity$Segment=="Home Office"))
EU_HomeOffice_quantity$Order.Date <- paste("01-",EU_HomeOffice_quantity$Order.Date)
EU_HomeOffice_quantity$Order.Date <- as.Date(EU_HomeOffice_quantity$Order.Date,"%d-%m-%Y")
EU_HomeOffice_quantity <- EU_HomeOffice_quantity[order(EU_HomeOffice_quantity$Order.Date),]
EU_HomeOffice_quantity$Order.Date <- format(EU_HomeOffice_quantity$Order.Date,"%m-%Y")

# segment 'Home Office'for Canada market
Canada_HomeOffice_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="Canada") & (market_segment_quantity$Segment=="Home Office"))
Canada_HomeOffice_quantity$Order.Date <- paste("01-",Canada_HomeOffice_quantity$Order.Date)
Canada_HomeOffice_quantity$Order.Date <- as.Date(Canada_HomeOffice_quantity$Order.Date,"%d-%m-%Y")
Canada_HomeOffice_quantity <- Canada_HomeOffice_quantity[order(Canada_HomeOffice_quantity$Order.Date),]
Canada_HomeOffice_quantity$Order.Date <- format(Canada_HomeOffice_quantity$Order.Date,"%m-%Y")

# segment 'Home Office'for EMEA market
EMEA_HomeOffice_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="EMEA") & (market_segment_quantity$Segment=="Home Office"))
EMEA_HomeOffice_quantity$Order.Date <- paste("01-",EMEA_HomeOffice_quantity$Order.Date)
EMEA_HomeOffice_quantity$Order.Date <- as.Date(EMEA_HomeOffice_quantity$Order.Date,"%d-%m-%Y")
EMEA_HomeOffice_quantity <- EMEA_HomeOffice_quantity[order(EMEA_HomeOffice_quantity$Order.Date),]
EMEA_HomeOffice_quantity$Order.Date <- format(EMEA_HomeOffice_quantity$Order.Date,"%m-%Y")

# segment 'Home Office'for LATAM market
LATAM_HomeOffice_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="LATAM") & (market_segment_quantity$Segment=="Home Office"))
LATAM_HomeOffice_quantity$Order.Date <- paste("01-",LATAM_HomeOffice_quantity$Order.Date)
LATAM_HomeOffice_quantity$Order.Date <- as.Date(LATAM_HomeOffice_quantity$Order.Date,"%d-%m-%Y")
LATAM_HomeOffice_quantity <- LATAM_HomeOffice_quantity[order(LATAM_HomeOffice_quantity$Order.Date),]
LATAM_HomeOffice_quantity$Order.Date <- format(LATAM_HomeOffice_quantity$Order.Date,"%m-%Y")

# segment 'Home Office'for US market
US_HomeOffice_quantity <- market_segment_quantity %>% filter((market_segment_quantity$Market=="US") & (market_segment_quantity$Segment=="Home Office"))
US_HomeOffice_quantity$Order.Date <- paste("01-",US_HomeOffice_quantity$Order.Date)
US_HomeOffice_quantity$Order.Date <- as.Date(US_HomeOffice_quantity$Order.Date,"%d-%m-%Y")
US_HomeOffice_quantity <- US_HomeOffice_quantity[order(US_HomeOffice_quantity$Order.Date),]
US_HomeOffice_quantity$Order.Date <- format(US_HomeOffice_quantity$Order.Date,"%m-%Y")

#---------------------------------------------------------------#


# this fuction computes and return Coefficient of Variance
calculateCoV <- function(dataframe){
  std <- sd(dataframe$Profit)
  avg <- mean(dataframe$Profit)
  cv <- (std/avg)*100
  return(cv)
}


####################### Mean and CoV computation for Consumer segment##################


#---------------------------------------------------------------#
# Profit CV for Consumer segment in Africa: 131.96%
Africa_Consumer_profit_cov <- calculateCoV(Africa_Consumer_profit)
# Mean Profit:995.25
Africa_Consumer_profit_mean <- mean(Africa_Consumer_profit$Profit)

#Profit CV for Consumer segment in Africa: 63.21%
APAC_Consumer_profit_cov <- calculateCoV(APAC_Consumer_profit)
# Mean Profit: 4642.03
APAC_Consumer_profit_mean <- mean(APAC_Consumer_profit$Profit)

#Profit CV for Consumer segment in EU: 62.43%
EU_Consumer_profit_cov <- calculateCoV(EU_Consumer_profit)
# Mean Profit: 3930.99
EU_Consumer_profit_mean <- mean(EU_Consumer_profit$Profit)

#Profit CV for Consumer segment in EMEA: 218.83%
EMEA_Consumer_profit_cov <- calculateCoV(EMEA_Consumer_profit)
# Mean Profit:531.93
EMEA_Consumer_profit_mean <- mean(EMEA_Consumer_profit$Profit)

#Profit CV for Consumer segment in Canada: 139.53
Canada_Consumer_profit_COV <- calculateCoV(Canada_Consumer_profit)
# Mean Profit: 230.42
Canada_Consumer_profit_mean <- mean(Canada_Consumer_profit$Profit)

#Profit CV for Consumer segment in LATAM: 66.15%
LATAM_Consumer_profit_cov <- calculateCoV(LATAM_Consumer_profit)
# Mean Profit: 2513.19
LATAM_Consumer_profit_mean <- mean(LATAM_Consumer_profit$Profit)

#Profit CV for Consumer segment in US: 101.24%
US_Consumer_profit_cov <- calculateCoV(US_Consumer_profit)
# Mean Profit: 2794.15
US_Consumer_profit_mean <- mean(US_Consumer_profit$Profit)

#---------------------------------------------------------------#


####################### Mean and CoV computation for Home Office segment##################


#---------------------------------------------------------------#

# Profit CV for Home Office segment in Africa: 179%
Africa_HomeOffice_profit_cov <- calculateCoV(Africa_HomeOffice_profit)
# Mean Profit: 425.26
Africa_HomeOffice_profit_mean <- mean(Africa_HomeOffice_profit$Profit)

#Profit CV for Home Office segment in Africa: 104.6%
APAC_HomeOffice_profit_cov <- calculateCoV(APAC_HomeOffice_profit)
# Mean Profit:1738.45
APAC_HomeOffice_profit_mean <- mean(APAC_HomeOffice_profit$Profit)

#Profit CV for Home Office segment in EU: 111.65%
EU_HomeOffice_profit_cov <- calculateCoV(EU_HomeOffice_profit)
# Mean Profit : 1265.59
EU_HomeOffice_profit_mean <- mean(EU_HomeOffice_profit$Profit)

#Profit CV for Home Office segment in EMEA: 588.08%
EMEA_HomeOffice_profit_cov <- calculateCoV(EMEA_HomeOffice_profit)
# Mean Profit: 122.21
EMEA_HomeOffice_profit_mean <- mean(EMEA_HomeOffice_profit$Profit)

#Profit CV for Home Office segment in Canada: 224.35%
Canada_HomeOffice_profit_COV <- calculateCoV(Canada_HomeOffice_profit)
# Mean Profit: 124.13
Canada_HomeOffice_profit_mean <- mean(Canada_HomeOffice_profit$Profit)

#Profit CV for Home Office segment in LATAM: 117.57%
LATAM_HomeOffice_profit_cov <- calculateCoV(LATAM_HomeOffice_profit)
# Mean Profit: 898.65
LATAM_HomeOffice_profit_mean <- mean(LATAM_HomeOffice_profit$Profit)

#Profit CV for Home Office segment in US: 109.64%
US_HomeOffice_profit_cov <- calculateCoV(US_HomeOffice_profit)
# Mean Profit: 1256.22
US_HomeOffice_profit_mean <- mean(US_HomeOffice_profit$Profit)

#---------------------------------------------------------------#


####################### Mean and CoV computation for Corporate segment##################


#---------------------------------------------------------------#

# Profit CV for Home Office segment in Africa: 177.61%
Africa_Corporate_profit_cov <- calculateCoV(Africa_Corporate_profit)
# Mean Profit:430.98
Africa_Corporate_profit_mean <- mean(Africa_Corporate_profit$Profit)

#Profit CV for Home Office segment in Africa: 69.81%
APAC_Corporate_profit_cov <- calculateCoV(APAC_Corporate_profit)
# Mean Profit: 2702.86
APAC_Corporate_profit_mean <- mean(APAC_Corporate_profit$Profit)

#Profit CV for Home Office segment in EU: 76.38%
EU_Corporate_profit_cov <- calculateCoV(EU_Corporate_profit)
# Mean Profit:2570.71
EU_Corporate_profit_mean <- mean(EU_Corporate_profit$Profit)

#Profit CV for Home Office segment in EMEA: 446.71%
EMEA_Corporate_profit_cov <- calculateCoV(EMEA_Corporate_profit)
# Mean Profit:260.4
EMEA_Corporate_profit_mean <- mean(EMEA_Corporate_profit$Profit)

#Profit CV for Home Office segment in Canada: 155.28%
Canada_Corporate_profit_COV <- calculateCoV(Canada_Corporate_profit)
# Mean Profit: 148.13
Canada_Corporate_profit_mean <- mean(Canada_Corporate_profit$Profit)

#Profit CV for Home Office segment in LATAM: 81.11%
LATAM_Corporate_profit_cov <- calculateCoV(LATAM_Corporate_profit)
# Mean Profit: 1205.74
LATAM_Corporate_profit_mean <- mean(LATAM_Corporate_profit$Profit)

#Profit CV for Home Office segment in US: 100.24%
US_Corporate_profit_cov <- calculateCoV(US_Corporate_profit)
# Mean Profit: 1916.23
US_Corporate_profit_mean <- mean(US_Corporate_profit$Profit)

#---------------------------------------------------------------#

# EU consumer segment and APAC consumer segment shows more consitent and good profit,
# LATAM consumer segment and US consumer segment has low CoV compared to EU consumer segment.
# This implies EU Consumer segment is more consistent profit market segment.
# Also the mean profit earned in EU Consumer Segment Market is more compared to LATAM and US Consumer segment.

# CoV and Mean together suggest APAC and EU Consumer market segment conbination
# is consistent and profitable.

# MARKET_SEGMENT combination selected are:- #
# APAC_CONSUMER
# EU_CONSUMER


#---------------------------------------------------------------#


# Forecasting sales and quantity for APAC and EU consumer market segment
# Split data into test and train
# keep last 6 months as test data.
train_APAC_Consumer_sales <- APAC_Consumer_sales[1:which(APAC_Consumer_sales$Order.Date=="05-2014"),]
test_APAC_Consumer_sales <- APAC_Consumer_sales[which(APAC_Consumer_sales$Order.Date=="06-2014"):nrow(APAC_Consumer_sales),]
train_APAC_Consumer_quantity <- APAC_Consumer_quantity[1:which(APAC_Consumer_quantity$Order.Date=="05-2014"),]
test_APAC_Consumer_quantity <- APAC_Consumer_quantity[which(APAC_Consumer_quantity$Order.Date=="06-2014"):nrow(APAC_Consumer_quantity),]

train_EU_Consumer_sales <- EU_Consumer_sales[1:which(EU_Consumer_sales$Order.Date=="05-2014"),]
test_EU_Consumer_sales <- EU_Consumer_sales[which(EU_Consumer_sales$Order.Date=="06-2014"):nrow(EU_Consumer_sales),]
train_EU_Consumer_quantity <- EU_Consumer_quantity[1:which(EU_Consumer_quantity$Order.Date=="05-2014"),]
test_EU_Consumer_quantity <- EU_Consumer_quantity[which(EU_Consumer_quantity$Order.Date=="06-2014"):nrow(EU_Consumer_quantity),]

#---------------------------------------------------------------#


################## Sales forecast for APAC Consumet segment ##############


#---------------------------------------------------------------#

# create and plot timeseries
# no seasonality but increasing trend detected.
timeser_APAC_Consumer_sales <- ts(train_APAC_Consumer_sales$Sales)
tsdisplay(timeser_APAC_Consumer_sales,lag.max = nrow(train_APAC_Consumer_sales))

#---------------------------------------------------------------#

# Use Classical Decomposition 

#---------------------------------------------------------------#

# As filter function of tseries was getting affected due to dplyr filter function #
# hence detaching the dplyr package here #
detach("package:dplyr", unload=TRUE)


# Smoothing the APAC_Consumer_sales_timeser using Moving Average Smoothing #
w <- 1
smoothed_APAC_Con_Sales <- filter(timeser_APAC_Consumer_sales, filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

# Smoothing left end of the time series #
diff_APAC_con_sales_left <- smoothed_APAC_Con_Sales[w+2] - smoothed_APAC_Con_Sales[w+1]

for (i in seq(w,1,-1)) {
  smoothed_APAC_Con_Sales[i] <- smoothed_APAC_Con_Sales[i+1] - diff_APAC_con_sales_left
}

#  Smoothing right end of the time series #
n <- length(timeser_APAC_Consumer_sales)

diff_APAC_con_sales_right <- smoothed_APAC_Con_Sales[n-w] - smoothed_APAC_Con_Sales[n-w-1]

for (i in seq(n-w+1, n)) {
  smoothed_APAC_Con_Sales[i] <- smoothed_APAC_Con_Sales[i-1] + diff_APAC_con_sales_right
}

# Checking the smoothness curve over the original line graph # 
plot(timeser_APAC_Consumer_sales, ylab="Sales" , main="APAC Consumer Sales",col="red")
lines(smoothed_APAC_Con_Sales, col="blue", lwd=2)

timeser_APAC_Consumer_sales <- smoothed_APAC_Con_Sales


# Find global predictable (trend) part of series
timevalsTrain_APAC_Consumer_sales <- c(1:nrow(train_APAC_Consumer_sales))
train_APAC_Consumer_sales_newDF <- as.data.frame(cbind(timevalsTrain_APAC_Consumer_sales,timeser_APAC_Consumer_sales))
colnames(train_APAC_Consumer_sales_newDF) <- c("Month","Sales")

# plot the trend
lmfit_APAC_Consumer_sales <- lm(Sales ~ Month, data=train_APAC_Consumer_sales_newDF)
globalpred_APAC_Consumer_sales <- predict(lmfit_APAC_Consumer_sales)
plot(timeser_APAC_Consumer_sales, ylab="Sales" , main="APAC Consumer Sales")
lines(timevalsTrain_APAC_Consumer_sales,globalpred_APAC_Consumer_sales,col="red",lwd=2)


# plot local predictable part
localpred_APAC_Consumer_sales <- timeser_APAC_Consumer_sales - globalpred_APAC_Consumer_sales
plot(localpred_APAC_Consumer_sales, col='red', type = "l", ylab="Sales" , main="Local Predictable")

# KPSS test
# p-value: .1, suggesting localpred series is stationary
kpss.test(localpred_APAC_Consumer_sales)

# ADF test
# p-value: .10, suggesting localpred is not stationary
adf.test(localpred_APAC_Consumer_sales, alternative="stationary")

# difference the time series to remove non stationary
localpred_APAC_Consumer_sales_diff <- diff(localpred_APAC_Consumer_sales)
plot(localpred_APAC_Consumer_sales_diff, col='red', type = "l", ylab="Sales" , main="Local Predictable")


# model localpred as ARMA series
# ARIMA(0,0,0) with zero mean 
# AIC=814.45
# this is a stationary series 
armafit_APAC_Consumer_sales <- auto.arima(localpred_APAC_Consumer_sales_diff)
tsdiag(armafit_APAC_Consumer_sales)
armafit_APAC_Consumer_sales

# compute residual series
resi_APAC_Consumer_sales <- localpred_APAC_Consumer_sales_diff - fitted(armafit_APAC_Consumer_sales)


# KPSS test
# p-value: .1, suggesting resi_APAC_Consumer_sales series is stationary
kpss.test(resi_APAC_Consumer_sales)

# ADF test
# p-value: .01, suggesting resi_APAC_Consumer_sales is not stationary
adf.test(resi_APAC_Consumer_sales, alternative="stationary")


# qqplot also suggest that localpred_APAC_Consumer_sales is white noise.

qqnorm(scale(resi_APAC_Consumer_sales))
abline(coef=c(0,1),col="red")


# Now the APAC sales timeseries has been split in:
# global predictable : lmfit_APAC_Consumer_sales (Trend)
# local predictable : armafit_APAC_Consumer_sales
# white noise 

# predict test dataset values
testvalsTrain_APAC_Consumer_sales <- c(nrow(train_APAC_Consumer_sales)+1:nrow(test_APAC_Consumer_sales))
APAC_Consumer_sales_pred <- predict(lmfit_APAC_Consumer_sales,data.frame(Month =testvalsTrain_APAC_Consumer_sales))



# forecast values for test data for local predictable
fcast_local_APAC_Consumer_sales <- predict(armafit_APAC_Consumer_sales, n.ahead = nrow(test_APAC_Consumer_sales))


# total forecast model: Additive model of global and local
total_forecast_APAC_Consumer_sales <- APAC_Consumer_sales_pred + fcast_local_APAC_Consumer_sales$pred


# evaluating MAPE: 22.21
MAPE_class_dec_APAC_Consumer_sales <- accuracy(total_forecast_APAC_Consumer_sales,test_APAC_Consumer_sales$Sales)[5]
MAPE_class_dec_APAC_Consumer_sales



# Use Auto.arima on timeseries from start and compare MAPE against MAPE of 
# Classical decomposition.
# ARIMA(0,1,1) or MA(1)series suggested (p=0, d=1, q=1)
# Coefficients: ma1 -0.7584 s.e.   0.1359
# log likelihood=-436.49  AIC=876.99

autoarima_APAC_Consumer_sales <- auto.arima(timeser_APAC_Consumer_sales)
autoarima_APAC_Consumer_sales

# ACF plot for MA(1) series 
tsdiag(autoarima_APAC_Consumer_sales)

# plot actual versus fitted timeseries 
plot(autoarima_APAC_Consumer_sales$x, col="green")
lines(fitted(autoarima_APAC_Consumer_sales), col="red")

# forecast values for test data
fcast_autoarima_APAC_Consumer_sales <- predict(autoarima_APAC_Consumer_sales, n.ahead = 7)

#evaluate the model using MAPE
MAPE_auto_arima_APAC_Consumer_sales <- accuracy(fcast_autoarima_APAC_Consumer_sales$pred,test_APAC_Consumer_sales[,4])[5]
#MAPE: 27.48
MAPE_auto_arima_APAC_Consumer_sales

# MAPE of Classical Decomposition better than using auto.arima directly on timeseries.
# Classical decomposition gives MAPE of 22.21



################## Quantiy forecast for APAC Consumet segment ##############

# create and plot timeseries
timeser_APAC_Consumer_quantity <- ts(train_APAC_Consumer_quantity$Quantity)
tsdisplay(timeser_APAC_Consumer_quantity,lag.max = nrow(train_APAC_Consumer_quantity))


# Smoothing the APAC_Consumer_sales_timeser using Moving Average Smoothing #
w <- 1
smoothed_APAC_Con_Quantity <- filter(timeser_APAC_Consumer_quantity, filter=rep(1/(2*w+1),(2*w+1)), method='convolution', sides=2)

# Smoothing left end of the time series #
diff_APAC_con_Quantity_left <- smoothed_APAC_Con_Quantity[w+2] - smoothed_APAC_Con_Quantity[w+1]

for (i in seq(w,1,-1)) {
  smoothed_APAC_Con_Quantity[i] <- smoothed_APAC_Con_Quantity[i+1] - diff_APAC_con_Quantity_left
}

#  Smoothing right end of the time series #
n <- length(timeser_APAC_Consumer_quantity)

diff_APAC_con_Quantity_right <- smoothed_APAC_Con_Quantity[n-w] - smoothed_APAC_Con_Quantity[n-w-1]

for (i in seq(n-w+1, n)) {
  smoothed_APAC_Con_Quantity[i] <- smoothed_APAC_Con_Quantity[i-1] + diff_APAC_con_Quantity_right
}


# Checking the smoothness curve over the original line graph # 
plot(timeser_APAC_Consumer_quantity, ylab="Quantity" , main="APAC Consumer Demand",col="red")
lines(smoothed_APAC_Con_Quantity, col="blue", lwd=2)

timeser_APAC_Consumer_quantity <- smoothed_APAC_Con_Quantity

# Find global predictable (trend) part of series
timevalsTrain_APAC_Consumer_quantity <- c(1:nrow(train_APAC_Consumer_quantity))
train_APAC_Consumer_quantity_newDF <- as.data.frame(cbind(timevalsTrain_APAC_Consumer_quantity,timeser_APAC_Consumer_quantity))
colnames(train_APAC_Consumer_quantity_newDF) <- c("Month","Quantity")


# plot the trend
lmfit_APAC_Consumer_quantity <- lm(Quantity ~ Month, data=train_APAC_Consumer_quantity_newDF)
globalpred_APAC_Consumer_quantity <- predict(lmfit_APAC_Consumer_quantity)
plot(timeser_APAC_Consumer_quantity, ylab="Demand" , main="APAC Consumer Demand")
lines(timevalsTrain_APAC_Consumer_quantity,globalpred_APAC_Consumer_quantity,col="red",lwd=2)


# plot local predictable part
localpred_APAC_Consumer_quantity <- timeser_APAC_Consumer_quantity - globalpred_APAC_Consumer_quantity
plot(localpred_APAC_Consumer_quantity, col='red', type = "l", ylab="Sales", main=" Local Predictable")


# KPSS test
# p-value: .1, suggesting localpred series is stationary
kpss.test(localpred_APAC_Consumer_quantity)


# ADF test
# p-value: .34, suggesting localpred is not stationary
adf.test(localpred_APAC_Consumer_quantity, alternative="stationary")


# differencine one leve localpred_APAC_Consumer_quantity to make it stationary
localpred_APAC_Consumer_quantity_diff <- diff(localpred_APAC_Consumer_quantity)


# model localpred as ARMA series
# ARIMA(3,0,0) with zero mean 
# AIC=429.64
# this is a stationary series and is white noise
armafit_APAC_Consumer_quantity <- auto.arima(localpred_APAC_Consumer_quantity_diff)
tsdiag(armafit_APAC_Consumer_quantity)
armafit_APAC_Consumer_quantity


# compute residual series
resi_APAC_Consumer_quantity <- localpred_APAC_Consumer_quantity_diff - fitted(armafit_APAC_Consumer_quantity)


# KPSS test
# p-value: .1, suggesting resi_APAC_Consumer_sales series is stationary
kpss.test(resi_APAC_Consumer_quantity)

# ADF test
# p-value: .07, suggesting resi_APAC_Consumer_sales is not stationary
adf.test(resi_APAC_Consumer_quantity, alternative="stationary")


# Differencing second leve to make series stationary
resi_APAC_Consumer_quantity_diff <- diff(resi_APAC_Consumer_quantity)


# model localpred as ARMA series
# ARIMA(1,0,0) with zero mean 
# AIC=428.37
armafit_APAC_Consumer_quantity_diff <- auto.arima(resi_APAC_Consumer_quantity_diff)
tsdiag(armafit_APAC_Consumer_quantity_diff)
armafit_APAC_Consumer_quantity_diff


# compute residual series
resi_APAC_Consumer_quantity_diff1 <- resi_APAC_Consumer_quantity_diff - fitted(armafit_APAC_Consumer_quantity_diff)
plot(resi_APAC_Consumer_quantity_diff1, col='red', type = "l", ylab="Sales", main=" White Noise")
plot(fitted(armafit_APAC_Consumer_quantity_diff), col='red', type = "l", ylab="Sales", main=" Local Predictable")


# KPSS test
# p-value: .1, suggesting resi_APAC_Consumer_sales series is stationary
kpss.test(resi_APAC_Consumer_quantity_diff1)

# ADF test
# p-value: .05, suggesting resi_APAC_Consumer_sales is not stationary
adf.test(resi_APAC_Consumer_quantity_diff1, alternative="stationary")


# qqplot also suggest that localpred_APAC_Consumer_quantity is white noise.
qqnorm(scale(resi_APAC_Consumer_quantity_diff1))
abline(coef=c(0,1),col="red")


# Now the APAC Quantity timeseries has been split in:
# global predictable : localpred_APAC_Consumer_quantity (Trend)
# local predictable: armafit_APAC_Consumer_quantity_diff
# white noise 

# predict test dataset values
testvalsTrain_APAC_Consumer_quantity <- c(nrow(train_APAC_Consumer_quantity)+1:nrow(test_APAC_Consumer_quantity))
APAC_Consumer_quantity_pred <- predict(lmfit_APAC_Consumer_quantity,data.frame(Month =testvalsTrain_APAC_Consumer_quantity))


# forecast values for test data for local predictable
fcast_local_APAC_Consumer_quantity <- predict(armafit_APAC_Consumer_quantity_diff, n.ahead = nrow(test_APAC_Consumer_quantity))


# total forecast model: Additive model of global and local
total_forecast_APAC_Consumer_quantity <- APAC_Consumer_quantity_pred + fcast_local_APAC_Consumer_quantity$pred


# evaluating MAPE: 29.01
MAPE_class_dec_APAC_Consumer_quant <- accuracy(total_forecast_APAC_Consumer_quantity,test_APAC_Consumer_quantity$Quantity)[5]
MAPE_class_dec_APAC_Consumer_quant



# Use Auto.arima on timeseries from start and compare MAPE against MAPE of 
# Classical decomposition.
# ARIMA(0,1,0) eries suggested (p=0, d=1, q=0)
autoarima_APAC_Consumer_quantity <- auto.arima(train_APAC_Consumer_quantity$Quantity)
autoarima_APAC_Consumer_quantity

# plot actual versus fitted timeseries 
plot(autoarima_APAC_Consumer_quantity$x, col="green")
lines(fitted(autoarima_APAC_Consumer_quantity), col="red")

# forecast values for test data
fcast_autoarima_APAC_Consumer_quantity <- predict(autoarima_APAC_Consumer_quantity, n.ahead = 7)

#evaluate the model using MAPE
MAPE_APAC_Consumer_quantity <- accuracy(fcast_autoarima_APAC_Consumer_quantity$pred,test_APAC_Consumer_quantity[,4])[5]
#MAPE: 28.72
MAPE_APAC_Consumer_quantity


# Both approach (auto.arima or Classical Decomposition) for APAC Consumer Quantity gives MAPE value 29.


################## Sales forecast for EU Consumer segment ##############

# create and plot timeseries

timeser_EU_Consumer_sales <- ts(train_EU_Consumer_sales$Sales)
tsdisplay(timeser_EU_Consumer_sales,lag.max = nrow(train_EU_Consumer_sales))


# Find global predictable (trend) part of series
timevalsTrain_EU_Consumer_sales <- c(1:nrow(train_EU_Consumer_sales))
train_EU_Consumer_sales_newDF <- as.data.frame(cbind(timevalsTrain_EU_Consumer_sales,timeser_EU_Consumer_sales))
colnames(train_EU_Consumer_sales_newDF) <- c("Month","Sales")


# plot the trend
lmfit_EU_Consumer_sales <- lm(Sales ~ Month, data=train_EU_Consumer_sales_newDF)
globalpred_EU_Consumer_sales <- predict(lmfit_EU_Consumer_sales)
plot(timeser_EU_Consumer_sales, ylab="Sales", main="EU Consumer Sales")
lines(timevalsTrain_EU_Consumer_sales,globalpred_EU_Consumer_sales,col="red",lwd=2)


# plot local predictable part
localpred_EU_Consumer_sales <- timeser_EU_Consumer_sales - globalpred_EU_Consumer_sales
plot(localpred_EU_Consumer_sales, col='red', type = "l", ylab="Sales", main="Local Predictable")


# KPSS test
# p-value: .1, suggesting localpred series is stationary
kpss.test(localpred_EU_Consumer_sales)


# ADF test
# p-value: .07, suggesting localpred is non stationary
adf.test(localpred_EU_Consumer_sales, alternative="stationary")

# plot local predictable part
localpred_EU_Consumer_sales_diff <- diff(localpred_EU_Consumer_sales)

# KPSS test
# p-value: .1, suggesting localpred series is stationary
kpss.test(localpred_EU_Consumer_sales_diff)


# ADF test
# p-value: .01, suggesting localpred is stationary after one differencing
adf.test(localpred_EU_Consumer_sales_diff, alternative="stationary")


# plot one level differenced series
plot(localpred_EU_Consumer_sales_diff, col='red', type = "l", ylab="Sales", main="Local Predictable")



# model localpred as ARMA series
# ARIMA(2,0,0) with zero mean 
# Coefficients: 
# ar1      ar2
#-0.5949  -0.5227
# s.e.   0.1318   0.1289
# AIC=874.15
# this is a AR(2) stationary series 
armafit_EU_Consumer_sales_diff <- auto.arima(localpred_EU_Consumer_sales_diff)
tsdiag(armafit_EU_Consumer_sales_diff)
armafit_EU_Consumer_sales_diff


# check if the residual series is white noise
resi_EU_Consumer_sales_diff <- localpred_EU_Consumer_sales_diff-fitted(armafit_EU_Consumer_sales_diff)
plot(resi_EU_Consumer_sales_diff, col="red", ylab="Sales", main="White Noise")

# qqplot also suggest that resi_EU_Consumer_sales_diff is white noise.
qqnorm(scale(resi_EU_Consumer_sales_diff))
abline(coef=c(0,1),col="red")

# Dickey-Fuller  p-value = 0.01
# KPSS test: p-value = .1

adf.test(resi_EU_Consumer_sales_diff,alternative = "stationary")
kpss.test(resi_EU_Consumer_sales_diff)



# Now the EU sales timeseries after first level of differecing has been split in:
# global predictable : (Trend)
# local predictable one level differenced AR(2) series
# white noise 

# predict test dataset values for global trend
testvalsTrain_EU_Consumer_sales <- c(nrow(train_EU_Consumer_sales)+1:nrow(test_EU_Consumer_sales))
EU_Consumer_sales_pred <- predict(lmfit_EU_Consumer_sales,data.frame(Month =testvalsTrain_EU_Consumer_sales))


# forecast values for test data for local predictable
fcast_local_EU_Consumer_sales <- predict(armafit_EU_Consumer_sales_diff, n.ahead = nrow(test_EU_Consumer_sales))


# total forecast model: Additive model of global and local
total_forecast_EU_Consumer_sales <- EU_Consumer_sales_pred + fcast_local_EU_Consumer_sales$pred

# evaluating MAP: 31.79
MAPE_class_dec_EU_Consumer_sales <- accuracy(total_forecast_EU_Consumer_sales,test_EU_Consumer_sales$Sales)[5]
MAPE_class_dec_EU_Consumer_sales



# Use Auto.arima on timeseries from start and compare MAPE against MAPE of 
# Classical decomposition.
# ARIMA(2,1,0) or AR(2)series suggested (p=2, d=1, q=0)
autoarima_EU_Consumer_sales <- auto.arima(train_EU_Consumer_sales$Sales)
autoarima_EU_Consumer_sales

# ACF plot for MA(1) series 
tsdiag(autoarima_EU_Consumer_sales)

# plot actual versus fitted timeseries 
plot(autoarima_EU_Consumer_sales$x, col="green")
lines(fitted(autoarima_EU_Consumer_sales), col="red")

# forecast values for test data
fcast_autoarima_EU_Consumer_sales <- predict(autoarima_EU_Consumer_sales, n.ahead = 7)

#evaluate the model using MAPE
MAPE_auto_arima <- accuracy(fcast_autoarima_EU_Consumer_sales$pred,test_EU_Consumer_sales[,4])[5]
#MAPE: 37.44
MAPE_auto_arima


# MAPe of Classical Decomposition better than using auto.arima directly on timeseries.
# Classical decomposition gives MAPE of 31.78


################## Quantiy forecast for EU Consumer segment ##############

# create and plot timeseries

timeser_EU_Consumer_quantity <- ts(train_EU_Consumer_quantity$Quantity)
tsdisplay(timeser_EU_Consumer_quantity,lag.max = nrow(train_EU_Consumer_quantity))


# Find global predictable (trend) part of series
timevalsTrain_EU_Consumer_quantity <- c(1:nrow(train_EU_Consumer_quantity))
train_EU_Consumer_quantity_newDF <- as.data.frame(cbind(timevalsTrain_EU_Consumer_quantity,timeser_EU_Consumer_quantity))
colnames(train_EU_Consumer_quantity_newDF) <- c("Month","Quantity")


# plot the trend
lmfit_EU_Consumer_quantity <- lm(Quantity ~ Month, data=train_EU_Consumer_quantity_newDF)
globalpred_EU_Consumer_quantity <- predict(lmfit_EU_Consumer_quantity)
plot(timeser_EU_Consumer_quantity, ylab="Quantity", main="EU Consumer Demand")
lines(timevalsTrain_EU_Consumer_quantity,globalpred_EU_Consumer_quantity,col="red",lwd=2)


# plot local predictable part
localpred_EU_Consumer_quantity <- timeser_EU_Consumer_quantity - globalpred_EU_Consumer_quantity
plot(localpred_EU_Consumer_quantity, col='red', type = "l")


# KPSS test
# p-value: .1, suggesting localpred series is stationary
kpss.test(localpred_EU_Consumer_quantity)


# ADF test
# p-value: .21, suggesting localpred is non stationary
adf.test(localpred_EU_Consumer_quantity, alternative="stationary")


# compute local predictable part: 1 differencing
localpred_EU_Consumer_quantity_diff <- diff(localpred_EU_Consumer_quantity)

# KPSS test
# p-value: .1, suggesting localpred series is stationary after one differencing
kpss.test(localpred_EU_Consumer_quantity_diff)


# ADF test
# p-value: .01, suggesting localpred is stationary after one differencing
adf.test(localpred_EU_Consumer_quantity_diff, alternative="stationary")


# qqplot shows some deviation from white noise for the points.
qqnorm(scale(localpred_EU_Consumer_quantity_diff))
abline(coef=c(0,1),col="red")


# model localpred as ARMA series
# ARIMA(2,0,0) with zero mean 
# Coefficients: 
# ar1      ar2
#-0.7461  -0.6040
# s.e.   0.1215   0.1177
# AIC=516.36
# this is a AR(2) stationary series 
armafit_EU_Consumer_quantity_diff <- auto.arima(localpred_EU_Consumer_quantity_diff)
tsdiag(armafit_EU_Consumer_quantity_diff)
armafit_EU_Consumer_quantity_diff


# check if the residual series is white noise
resi_EU_Consumer_quantity_diff <- localpred_EU_Consumer_quantity_diff-fitted(armafit_EU_Consumer_quantity_diff)
plot(resi_EU_Consumer_quantity_diff)


# Dickey-Fuller  p-value = 0.08
# KPSS test: p-value = .1
# residual (resi_EU_Consumer_quantity_diff) series is not stationary

adf.test(resi_EU_Consumer_quantity_diff,alternative = "stationary")
kpss.test(resi_EU_Consumer_quantity_diff)


# qqplot shows some deviation from white noise for the points.
qqnorm(scale(resi_EU_Consumer_quantity_diff))
abline(coef=c(0,1),col="red")


# re-differencing the differenced series
resi_EU_Consumer_quantity_diff2 <- diff(resi_EU_Consumer_quantity_diff)


# plot resi_EU_Consumer_quantity_diff
plot(resi_EU_Consumer_quantity_diff, ylab="Quantity", main="Local Predictable", col="red")



# KPSS test
# p-value: .1, suggesting localpred series is stationary after one differencing
kpss.test(resi_EU_Consumer_quantity_diff2)


# ADF test
# p-value: .01, suggesting localpred is stationary after one differencing
adf.test(resi_EU_Consumer_quantity_diff2, alternative="stationary")


# qqplot also suggest that resi_EU_Consumer_quantity_diff2 is white noise.
qqnorm(scale(resi_EU_Consumer_quantity_diff2))
abline(coef=c(0,1),col="red")


# model resi_EU_Consumer_quantity_diff2 as ARMA series
# ARIMA(2,0,0) with zero mean 
# Coefficients: 
# ar1      ar2
#-0.6258  -0.3518
# s.e.   0.1486   0.1468
# AIC=517.02
# this is a AR(2) stationary series 
armafit_EU_Consumer_quantity_diff2 <- auto.arima(resi_EU_Consumer_quantity_diff2)
tsdiag(armafit_EU_Consumer_quantity_diff2)
armafit_EU_Consumer_quantity_diff2


# check if the residual series is white noise
resi_EU_Consumer_quantity_diff_2 <- resi_EU_Consumer_quantity_diff2-fitted(armafit_EU_Consumer_quantity_diff2)
plot(resi_EU_Consumer_quantity_diff_2, ylab="Sales", col="red", main="White Noise")


# Dickey-Fuller  p-value = 0.03
# KPSS test: p-value = .1
# resi_EU_Consumer_quantity_diff_2 series is stationary

adf.test(resi_EU_Consumer_quantity_diff_2,alternative = "stationary")
kpss.test(resi_EU_Consumer_quantity_diff_2)


# qqplot also suggest that resi_EU_Consumer_quantity_diff2 is white noise.
qqnorm(scale(resi_EU_Consumer_quantity_diff_2))
abline(coef=c(0,1),col="red")


# EU Consumer quantity series can be split into:
# global predictable: lmfit_EU_Consumer_quantity
# 2 differenced local predictable series: resi_EU_Consumer_quantity_diff2
# white noise


# predict test dataset values for global trend
testvalsTrain_EU_Consumer_quantity <- c(nrow(train_EU_Consumer_quantity)+1:nrow(test_EU_Consumer_quantity))
EU_Consumer_quantity_pred <- predict(lmfit_EU_Consumer_quantity,data.frame(Month =testvalsTrain_EU_Consumer_quantity))


# forecast values for test data for local predictable
fcast_local_EU_Consumer_quantity <- predict(armafit_EU_Consumer_quantity_diff2, n.ahead = nrow(test_EU_Consumer_quantity))


# total forecast model: Additive model of global and local
total_forecast_EU_Consumer_quantity <- EU_Consumer_quantity_pred + fcast_local_EU_Consumer_quantity$pred

# evaluating MAP: 31.17
MAPE_class_dec_EU_Consumer_quantity <- accuracy(total_forecast_EU_Consumer_quantity,test_EU_Consumer_quantity$Quantity)[5]
MAPE_class_dec_EU_Consumer_quantity




# Use Auto.arima on timeseries from start and compare MAPE against MAPE of 
# Classical decomposition.
# ARIMA(2,1,0) or AR(2) series suggested (p=2, d=1, q=0)
autoarima_EU_Consumer_quantity <- auto.arima(train_EU_Consumer_quantity$Quantity)
autoarima_EU_Consumer_quantity

# plot actual versus fitted timeseries 
plot(autoarima_EU_Consumer_quantity$x, col="green")
lines(fitted(autoarima_EU_Consumer_quantity), col="red")

# forecast values for test data
fcast_autoarima_EU_Consumer_quantity <- predict(autoarima_EU_Consumer_quantity, n.ahead = 7)

# evaluate the model using MAPE
MAPE_EU_Consumer_quantity <- accuracy(fcast_autoarima_EU_Consumer_quantity$pred,test_EU_Consumer_quantity[,4])[5]
# MAPE: 36.33
MAPE_EU_Consumer_quantity


# MAPe of Classical Decomposition better than using auto.arima directly on timeseries.
# Classical decomposition gives MAPE of 31.78



###########################################################################3333

# Below timeseries are the predicted output series with consistent and more profitable market segments:

# 2 market segments with consistent and good profits: APAC Consumer and EU Consumer.

# Sales Timeseries for APAC Consumer: APAC_Consumer_sales_pred
# Sales Timeseries for EU Consumer: total_forecast_EU_Consumer_sales
# Quantity Timeseries for APAC Consumer: fcast_autoarima_APAC_Consumer_quantity
# Quantity Timeseries for EU Consumer: total_forecast_EU_Consumer_quantity



# forecast test dataset values
forecastTime <- c(49:54)


# forecast Sales for APAC Consumer Segment for next 6 months
forecast_APAC_Consumer_sales <- predict(lmfit_APAC_Consumer_sales,data.frame(Month =forecastTime))
forecast_APAC_Consumer_sales_df <- as.data.frame(cbind(forecastTime, forecast_APAC_Consumer_sales))
colnames(forecast_APAC_Consumer_sales_df) <- c("Month", "Sales")


# forecast local predictable part for next 6 months
forecastTime_local <- c(42:54)
forecast_local_APAC_Consumer_sales <- predict(armafit_APAC_Consumer_sales, n.ahead = nrow(test_APAC_Consumer_sales)+6)
forecast_local_APAC_Consumer_sales_df <- as.data.frame(cbind(forecastTime_local, forecast_local_APAC_Consumer_sales$pred))
colnames(forecast_local_APAC_Consumer_sales_df) <- c("Month", "Sales")

# applying additive model
total_forecast_APAC_Consumer_sales <- forecast_APAC_Consumer_sales_df$Sales + forecast_local_APAC_Consumer_sales_df[which(forecast_local_APAC_Consumer_sales_df$Month >= 49),][,2]
total_forecast_APAC_Consumer_sales_df <- as.data.frame(cbind(c(49:54),total_forecast_APAC_Consumer_sales))
colnames(total_forecast_APAC_Consumer_sales_df) <- c("Month", "Sales")



# forecast Demand for APAC Consumer Segment for next 6 months
forecast_APAC_Consumer_quantity <- predict(lmfit_APAC_Consumer_quantity,data.frame(Month =forecastTime))
forecast_APAC_Consumer_quantity_df <- as.data.frame(cbind(forecastTime, forecast_APAC_Consumer_quantity))
colnames(forecast_APAC_Consumer_quantity_df) <- c("Month", "Demand")


# forecast local predictable part for next 6 months
forecastTime_local <- c(42:54)
forecast_local_APAC_Consumer_quantity <- predict(armafit_APAC_Consumer_quantity_diff, n.ahead = nrow(test_APAC_Consumer_quantity)+6)
forecast_local_APAC_Consumer_quantity_df <- as.data.frame(cbind(forecastTime_local, forecast_local_APAC_Consumer_quantity$pred))
colnames(forecast_local_APAC_Consumer_quantity_df) <- c("Month", "Demand")

# applying additive model
total_forecast_APAC_Consumer_quantity <- forecast_APAC_Consumer_quantity_df$Demand + forecast_local_APAC_Consumer_quantity_df[which(forecast_local_APAC_Consumer_quantity_df$Month >= 49),][,2]
total_forecast_APAC_Consumer_quantity_df <- as.data.frame(cbind(c(49:54),total_forecast_APAC_Consumer_quantity))
colnames(total_forecast_APAC_Consumer_quantity_df) <- c("Month", "Demand")




# forecast Sales for EU Consumer Segment for next 6 months
# timeseries for EU Consumer Sales is buit using Additive Model

# forecast global predictable part for next 6 months
forecast_EU_Consumer_sales <- predict(lmfit_EU_Consumer_sales,data.frame(Month =forecastTime))
forecast_EU_Consumer_sales_df <- as.data.frame(cbind(forecastTime, forecast_EU_Consumer_sales))
colnames(forecast_EU_Consumer_sales_df) <- c("Month", "Sales")

# forecast local predictable part for next 6 months
forecastTime_local <- c(42:54)
forecast_local_EU_Consumer_sales <- predict(armafit_EU_Consumer_sales_diff, n.ahead = nrow(test_EU_Consumer_sales)+6)
forecast_local_EU_Consumer_sales_df <- as.data.frame(cbind(forecastTime_local, forecast_local_EU_Consumer_sales$pred))
colnames(forecast_local_EU_Consumer_sales_df) <- c("Month", "Sales")

# applying additive model
total_forecast_EU_Consumer_sales <- forecast_EU_Consumer_sales_df$Sales + forecast_local_EU_Consumer_sales_df[which(forecast_local_EU_Consumer_sales_df$Month >= 49),][,2]
total_forecast_EU_Consumer_sales_df <- as.data.frame(cbind(c(49:54),total_forecast_EU_Consumer_sales))
colnames(total_forecast_EU_Consumer_sales_df) <- c("Month", "Sales")




# forecast Demand for EU Consumer Segment for next 6 months
# timeseries for EU Consumer Sales is buit using Additive Model

# forecast global predictable part for next 6 months
forecast_EU_Consumer_quantity <- predict(lmfit_EU_Consumer_quantity,data.frame(Month =forecastTime))
forecast_EU_Consumer_quantity_df <- as.data.frame(cbind(forecastTime, forecast_EU_Consumer_quantity))
colnames(forecast_EU_Consumer_quantity_df) <- c("Month", "Demand")

# forecast local predictable part for next 6 months
forecast_local_EU_Consumer_quantity <- predict(armafit_EU_Consumer_quantity_diff2, n.ahead = nrow(test_EU_Consumer_quantity)+6)
forecast_local_EU_Consumer_quantity_df <- as.data.frame(cbind(forecastTime_local, forecast_local_EU_Consumer_quantity$pred))
colnames(forecast_local_EU_Consumer_quantity_df) <- c("Month", "Demand")

# applying additive model
total_forecast_EU_Consumer_quantity <- forecast_EU_Consumer_quantity_df$Demand + forecast_local_EU_Consumer_quantity_df[which(forecast_local_EU_Consumer_quantity_df$Month >= 49),][,2]
total_forecast_EU_Consumer_quantity_df <- as.data.frame(cbind(c(49:54),total_forecast_EU_Consumer_quantity))
colnames(total_forecast_EU_Consumer_quantity_df) <- c("Month", "Demand")
