
library(dplyr)
library(car)
library(MASS)
library(xlsx)
library(ggplot2)
library(DAAG)
library(dlnm)

# avoid using exp. values
options(scipen=999)


# read transactional level data 
input_df <- read.csv("ConsumerElectronics.csv", header=TRUE)
str(input_df)

# read media investment information from worksheet
med_invst_df <- read.xlsx("Media data and other information.xlsx", sheetName = "Media Investment", header = TRUE)


# assigning proper column names since worksheet doesnot report dataframe format
col_list <- as.character(unlist(med_invst_df[1,]))
med_invst_df <- med_invst_df[-1,]
colnames(med_invst_df) <- col_list
rownames(med_invst_df) <- c(1:12)

# format Month column values of media investment dataframe as per Year_Month
med_invst_df$YearMonth <- paste(med_invst_df$Year,med_invst_df$Month,sep = "_")
str(med_invst_df)

# convert factor to numeric values for each kind of investement
med_invst_df$`Total Investment` <- as.numeric(levels(med_invst_df$`Total Investment`))[med_invst_df$`Total Investment`]
med_invst_df$TV <- as.numeric(levels(med_invst_df$TV))[med_invst_df$TV]
med_invst_df$Digital <- as.numeric(levels(med_invst_df$Digital))[med_invst_df$Digital]
med_invst_df$Sponsorship <- as.numeric(levels(med_invst_df$Sponsorship))[med_invst_df$Sponsorship]
med_invst_df$`Content Marketing` <- as.numeric(levels(med_invst_df$`Content Marketing`))[med_invst_df$`Content Marketing`]
med_invst_df$`Online marketing` <- as.numeric(levels(med_invst_df$`Online marketing`))[med_invst_df$`Online marketing`]
med_invst_df$` Affiliates` <- as.numeric(levels(med_invst_df$` Affiliates`))[med_invst_df$` Affiliates`]
med_invst_df$SEM <- as.numeric(levels(med_invst_df$SEM))[med_invst_df$SEM]


# define dataframe with dates of promotion
promotions_df <- data.frame(c("2015-07-18", "2015-07-19", "2015-08-15", "2015-08-17", "2015-08-18",
                              "2015-08-28","2015-08-29","2015-08-30","2015-10-15","2015-10-16","2015-10-17",
                              "2015-11-07","2015-11-08","2015-11-09","2015-11-10","2015-11-11","2015-11-12","2015-11-13","2015-11-14",
                              "2015-12-25","2015-12-26","2015-12-26","2015-12-27","2015-12-28","2015-12-29","2015-12-30","2015-12-31",
                              "2016-01-01","2016-01-02","2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01","2016-02-02",
                              "2016-02-14","2016-02-15","2016-02-20","2016-02-21","2016-03-07","2016-03-08","2016-03-09",
                              "2016-05-25","2016-05-26","2016-05-27"))
colnames(promotions_df)<-c("Promotion Date")
promotions_df$`Promotion Date` <-as.Date(promotions_df$`Promotion Date`)

# Add Week column for each date starting from 2015-05-19
promotions_df$Week <- NA


# define dataframe for nps
nps_df <- data.frame(as.Date(c("2015-07-31", "2015-08-31","2015-09-30","2015-10-31","2015-11-30","2015-12-31","2016-01-31","2016-02-29",
                              "2016-03-31","2016-04-30","2016-05-31","2016-06-30")), c(54.6,60.0,46.9,44.4,47.0,45.8,47.1,50.3,49.0,51.8,47.3,50.5))
colnames(nps_df)<-c("Month","NPS Score")


# compute weeks of year 2015 till 2016. 
# year 2015 first week ended on 3rd January 2015.
computeWeek <-  function(x){
  dt <- as.Date(x)
  diff <- ceiling(difftime(dt,as.Date("2014-12-28"),units="weeks"))
  return(diff)
}

# compute RMSE
computeRMSE <- function(x){
  sq_x <- x^2
  rmse <- sqrt(mean(sq_x))
  return(rmse)
}
  

# compute maximum frequency value from a dataset for aggregating PaymentType over week
# If there are equal number of both types of payment for a week, the PaymentType for that week is considered to be 1
# since EDA shows major portion of orders were on COD 

computeFrequency <- function(x){
  freq <- as.numeric(names(which.max(table(x))))
  if (mean(x) == 1.5){
    freq <- 2
  }
  return(freq)
}


# given a date it computes number of days in the month
numberOfDays <- function(date) {
  m <- format(date, format="%m")
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format="%d")))
}

options(scipen=999)


nps_df$Week <- 0
for(i in 1:nrow(nps_df)){
  nps_df$Week[i] <- computeWeek(nps_df$Month[i])
}

nps_df$NumberDays <- 0
nps_df$NumberDays <- numberOfDays(nps_df$Month)

# timeperiod for investment and NPS data is same
med_invst_df <- cbind(med_invst_df,nps_df$NumberDays)
colnames(med_invst_df)[14] <- "Number of Days"

# create a dataframe with each day of year starting from 01-07-2015 till 30-06-2016
# NPS and Advertisement will be accordingly distributed
day <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),"days")
Week <- computeWeek(day)
year_weeks <- as.data.frame(cbind(format(day,"%Y-%m-%d"),Week))


# Split monthly media investment into weekly media investment with average investment
propagateAdvertisement <- function(adv,x){
  init <- 1
  for (i in 1:nrow(med_invst_df)){
    numdays <- (init + med_invst_df$`Number of Days`[i]) - 1
    adv[init:numdays] <- (x[i]/med_invst_df$`Number of Days`[i])*10000000
    init <- numdays + 1
  }
  return(adv)
}

# Assumption: NPS scores will be same for all the weeks in a given month
propagateNPS <- function(df){
  init <- 1
  for (i in 1:nrow(nps_df)){
    numdays <- (init + nps_df$NumberDays[i]) - 1
    df$NPS[init:numdays] <- nps_df$`NPS Score`[i]
    init <- numdays + 1
  }
  return(df$NPS)
}

# propagate NPS values 
# Assumption: NPS of each day is same per month
year_weeks$NPS <- 0
year_weeks$NPS <- propagateNPS(year_weeks)

# propagate Advertisement values
# Assumption: Investment of each month splitted per day equally
year_weeks$Advertisement <- 0
year_weeks$Advertisement <- propagateAdvertisement(year_weeks$Advertisement,med_invst_df$`Total Investment`)

# propagate TV Advertisement values
# Assumption: Investment of each month splitted per day equally
year_weeks$TV <- 0
year_weeks$TV <- propagateAdvertisement(year_weeks$TV,med_invst_df$TV)


# propagate TV Advertisement values
# Assumption: Investment of each month splitted per day equally
year_weeks$Digital <- 0
year_weeks$Digital <- propagateAdvertisement(year_weeks$Digital,med_invst_df$Digital)


# propagate TV Advertisement values
# Assumption: Investment of each month splitted per day equally
year_weeks$Sponsorship <- 0
year_weeks$Sponsorship <- propagateAdvertisement(year_weeks$Sponsorship,med_invst_df$Sponsorship)


# propagate TV Advertisement values
# Assumption: Investment of each month splitted per day equally
year_weeks$ContentMarketing <- 0
year_weeks$ContentMarketing <- propagateAdvertisement(year_weeks$ContentMarketing,med_invst_df$`Content Marketing`)


# propagate TV Advertisement values
# Assumption: Investment of each month splitted per day equally
year_weeks$Online <- 0
year_weeks$Online <- propagateAdvertisement(year_weeks$Online,med_invst_df$`Online marketing`)


# propagate TV Advertisement values
# Assumption: Investment of each month splitted per day equally
year_weeks$SEM <- 0
year_weeks$SEM <- propagateAdvertisement(year_weeks$SEM,med_invst_df$SEM)

# propagate TV Advertisement values
# Assumption: Investment of each month splitted per day equally
year_weeks$Affliates <- 0
year_weeks$Affliates <- propagateAdvertisement(year_weeks$Affliates,med_invst_df$` Affiliates`)


# Aggregation of NPS scores and Investment per week
wk_nps_invst <- year_weeks %>% group_by(Week) %>% summarise(NPS = mean(NPS), TV = sum(TV), Digital = sum(Digital), Sponsorship = sum(Sponsorship),
                                                          ContentMarketing = sum(ContentMarketing), Online = sum(Online), SEM = sum(SEM), Affliates = sum(Affliates), Investment = sum(Advertisement))

# find promotion weeks
for(i in 1:nrow(promotions_df)){
  promotions_df$Week[i] <- computeWeek(promotions_df$`Promotion Date`[i])
}

# find all columns which have all values as NA
# gmv, cust_id and pincode have 4904 NA values.
input_df_na <- as.data.frame(sapply(input_df, function(x) sum(is.na(x))))

# check for NA rows for GMV
input_gmv_na <- input_df[which(is.na(input_df$gmv)),]

# All transactions with NA for gmv have also NA for cust id and pincode
chk_na <- as.data.frame(sapply(input_gmv_na, function(x) sum(is.na(x))))

# remove NA gmv value transactions
input_df_nonNA <- input_df[-which(is.na(input_df$gmv)),]

# order input_df_nonNA by time
input_df_nonNA <- input_df_nonNA[order(input_df_nonNA$order_date),]

# find duplicate rows for same orders recorded more than once
duplicate <- input_df_nonNA[which(duplicated(input_df_nonNA)),]
input_df_nonNA <-  distinct(input_df_nonNA)

# 
# check for sub-categories: no typos detected
unique(input_df_nonNA$product_analytic_sub_category)

 
# camera accessory dataframe
cam_Access_df <- input_df_nonNA %>% filter(input_df_nonNA$product_analytic_sub_category == 'CameraAccessory')

# home audio dataframe
hom_Audio_df <- input_df_nonNA %>% filter(input_df_nonNA$product_analytic_sub_category == 'HomeAudio')

# gmaing accessory dataframe
gam_access_df <- input_df_nonNA %>% filter(input_df_nonNA$product_analytic_sub_category == 'GamingAccessory')



################## EDA  ############################


# product_mrp has some values as zero, these will be removed.
# camera accessories
cam_Access_df <- cam_Access_df[which(cam_Access_df$product_mrp != 0), ]

# product_mrp has some values as zero, these will be removed.
# camera accessories
hom_Audio_df <- hom_Audio_df[which(hom_Audio_df$product_mrp != 0), ]

# product_mrp gas some values as zero, these will be removed.
# camera accessories
gam_access_df <- gam_access_df[which(gam_access_df$product_mrp != 0), ]



# deliverybdays have some values in negatives. These transactions will be removed
# add another column delbdays,remove negative values for Camera Accessories
cam_Access_df$delbdays <- as.character(cam_Access_df$deliverybdays)

# assiging '//N' values to some arbitrary value for conversion of column to numeric
cam_Access_df[which(cam_Access_df$delbdays == "\\N"),]$delbdays <- 0
cam_Access_df$delbdays <- as.numeric(cam_Access_df$delbdays)

# There are 6 rows with negative values,removing rows with negative values due to their no impact on overall dataset
cam_Access_df <- cam_Access_df[-which(cam_Access_df$delbdays < 0),]



# deliveryddays have some values in negatives. These transactions will be removed
# add another column delbdays,remove negative values for Camera Accessories
cam_Access_df$delcdays <- as.character(cam_Access_df$deliverycdays)

# assiging '//N' values to some arbitrary value for conversion of column to numeric
cam_Access_df[which(cam_Access_df$delcdays == "\\N"),]$delcdays <- 0
cam_Access_df$delcdays <- as.numeric(cam_Access_df$delcdays)

# There are 0 rows with negative values
nrow(cam_Access_df[-which(cam_Access_df$delcdays < 0),])

# assigning '1000' values to '-1' 
# now deliverybdays will have values in '-1, 0 and more than 0 values. '-1' represent '//N' va;ues
# cam_Access_df[which(cam_Access_df$delbdays == 1000),]$delbdays <- -1 

# add another column delbdays, remove negative values for Gaming Accessories
gam_access_df$delbdays <- as.character(gam_access_df$deliverybdays)

# No row detected wit 1000 as value of deliverybdays. replace all '//N' with 1000 to make the column numeric. 
gam_access_df[which(gam_access_df$delbdays == "\\N"),]$delbdays <- 0
gam_access_df$delbdays <- as.numeric(gam_access_df$delbdays)

# There is 1 rows with negative values,removing rows with negative values due to their no impact on overall dataset
gam_access_df <- gam_access_df[-which(gam_access_df$delbdays < 0),]


# add another column delbcays, remove negative values for Gaming Accessories
gam_access_df$delcdays <- as.character(gam_access_df$deliverycdays)

# No row detected wit 1000 as value of deliverybdays. replace all '//N' with 1000 to make the column numeric. 
gam_access_df[which(gam_access_df$delcdays == "\\N"),]$delcdays <- 0
gam_access_df$delcdays <- as.numeric(gam_access_df$delcdays)

# There are 0 rows with negative values,removing rows with negative values due to their no impact on overall dataset
gam_access_df[-which(gam_access_df$delcdays < 0),]


# add another columnt delbdays, remove negative values for Home Audio
hom_Audio_df$delbdays <- as.character(hom_Audio_df$deliverybdays)

# No row detected wit 1000 as value of deliverybdays. replace all '//N' with 1000 to make the column numeric. 
hom_Audio_df[which(hom_Audio_df$delbdays == "\\N"),]$delbdays <- 0
hom_Audio_df$delbdays <- as.numeric(hom_Audio_df$delbdays)

# There are 3 rows with negative values,removing rows with negative values due to their no impact on overall dataset
hom_Audio_df <- hom_Audio_df[-which(hom_Audio_df$delbdays < 0),]


# add another columnt delcdays, remove negative values for Home Audio
hom_Audio_df$delcdays <- as.character(hom_Audio_df$deliverycdays)

# No row detected wit 1000 as value of deliverybdays. replace all '//N' with 1000 to make the column numeric. 
hom_Audio_df[which(hom_Audio_df$delcdays == "\\N"),]$delcdays <- 0
hom_Audio_df$delcdays <- as.numeric(hom_Audio_df$delcdays)

# There are o rows with negative values,removing rows with negative values due to their no impact on overall dataset
nrow(hom_Audio_df[which(hom_Audio_df$delcdays < 0),])

# check negative values for procurement sla values 

# 4073 transactions (~.01 %)
cam_pro_proc_sla_fil <- cam_Access_df[which(cam_Access_df$product_procurement_sla < 0),]

# remove transactions with negative sla values
cam_Access_df <- cam_Access_df[-which(cam_Access_df$product_procurement_sla < 0),]

# 3470 transactions
gam_pro_proc_sla_fil <- gam_access_df[which(gam_access_df$product_procurement_sla < 0),]

# 3470 transactions
gam_access_df <- gam_access_df[-which(gam_access_df$product_procurement_sla < 0),]

# 4202 transactions
hom_pro_proc_sla_fil <- hom_Audio_df[which(hom_Audio_df$product_procurement_sla < 0),]

# 4202 transactions
hom_Audio_df <- hom_Audio_df[-which(hom_Audio_df$product_procurement_sla < 0),]


###################################### Sales Graphs #####################################

################### Sales - Camera Accessories ########################


# order the sales data for camera accessories by time
cam_Access_df <- cam_Access_df[order(cam_Access_df$order_date),]
cam_Access_df$week <- "NA"

for(i in 1:nrow(cam_Access_df)){
    cam_Access_df$week[i] <- computeWeek(cam_Access_df$order_date[i])
}


# convert order payment type into dummy variables
levels(cam_Access_df$s1_fact.order_payment_type) <-c(2,1)
cam_Access_df$s1_fact.order_payment_type <- as.numeric(levels(cam_Access_df$s1_fact.order_payment_type))[cam_Access_df$s1_fact.order_payment_type]

# Evaluate total cost = no. of units * MRP
cam_Access_df$TotalCostPrice <- cam_Access_df$product_mrp * cam_Access_df$units

# Calculate discount offered on each order
cam_Access_df$discount <- (1 - (cam_Access_df$gmv/cam_Access_df$TotalCostPrice))*100


# summarize sales data per week for camera accessories
wk_cam_access <- cam_Access_df %>% group_by(cam_Access_df$week) %>% summarize(Sales = sum(gmv), PaymentType=computeFrequency(s1_fact.order_payment_type), Discount = median(discount), delbdays = median(delbdays), delcdays = median(delcdays))
# wk_cam_access[which(wk_cam_access$PaymentType > 0),]$PaymentType <- 1
colnames(wk_cam_access) <- c("Week","Sales","PaymentType","Discount","delbdays","delcdays")

# order weekly sales data by week numbers
wk_cam_access$Week <- as.numeric(wk_cam_access$Week)
wk_cam_access <- wk_cam_access[order(wk_cam_access$Week),]


# tag weeks with Promotion as 2 and rest with promotion as 1 in new column
# these values will help in logarithmic transformation since selecting 0,1 results in log(0) = Inf
wk_cam_access$PromotionWeek <- 1
wk_cam_access[which(wk_cam_access$Week %in% promotions_df$Week),]$PromotionWeek <- 2


# add Advertisement and NPS columns to weekly data
wk_cam_access <- merge(wk_cam_access,wk_nps_invst,by.x="Week",all=TRUE)

# Assumption: replace NPS and Advertisement for week 26, 80 and 81 equal one
wk_cam_access$NPS[1] <-  1
wk_cam_access$NPS[55] <-  1
wk_cam_access$NPS[56] <-  1

wk_cam_access[1,c(9:15)] <- 1
wk_cam_access[(which(wk_cam_access$Week == "80" | wk_cam_access$Week == "81")),c(9:15)] <- 1

# wk_cam_access$Investment[55] <- 1
# wk_cam_access$Investment[56] <- 1

# remove week 34 since there was no Sales reported for this week, neither it was a promotion week
wk_cam_access <- wk_cam_access[-9,]


# sales graph for camera accessory
ggplot(wk_cam_access, aes(x=Week, y=Sales, group=1))+ geom_point() + geom_line(color="blue") +labs(title = "Camera Accessories")
        
# Outliner treatment - identify outliners
boxplot(wk_cam_access$Discount, col="green",outcol="red",main="Weekly Cam Accessories Sales Distribution", ylab="Discount")
wk_cam_access <- wk_cam_access[-c(1,7,8),]

boxplot(wk_cam_access$NPS, col="green",outcol="red",main="Weekly Cam Accessories Sales Distribution", ylab="NPS")
summary(wk_cam_access$NPS)

# replace NPS greater than 55.77257 with 55.77257 for outer fence outliers
quantile(wk_cam_access$NPS,seq(0,1,.01))
wk_cam_access$NPS[which(wk_cam_access$NPS>55.32514)] <- 55.32514


boxplot(wk_cam_access$Investment, col="green",outcol="red",main="Weekly Cam Accessories Sales Distribution", ylab="Investment")

# summarize sales data per month for camera accessories
mnth_cam_access <- cam_Access_df %>% group_by(cam_Access_df$Year,cam_Access_df$Month) %>% summarize(WeeklySales = sum(gmv))
colnames(mnth_cam_access) <- c("Year","Month","Monthly Sales")
mnth_cam_access$Month <- paste(mnth_cam_access$Year,mnth_cam_access$Month,sep = "_")
mnth_cam_access <- mnth_cam_access[,-1]


combined_cam_acces <- merge(mnth_cam_access,med_invst_df,by.x="Month",by.y = "YearMonth",all=TRUE)
combined_cam_acces <- combined_cam_acces[,c(1,2,5)]
combined_cam_acces$`Total Investment` <- as.numeric(as.character(combined_cam_acces$`Total Investment`))*10000000


# new dataframe with ordered month and year data
combined_cam_acces_ord <- rbind(combined_cam_acces[c(4:7),], combined_cam_acces[c(1,2,3),],combined_cam_acces[c(8:14),])
row.names(combined_cam_acces_ord) <- 1:14
combined_cam_acces_ord$Month <- factor(combined_cam_acces_ord$Month,levels = c("2015_6","2015_7","2015_8","2015_9","2015_10","2015_11",
                                                                               "2015_12","2016_1","2016_2","2016_3","2016_4","2016_5","2016_6","2016_7"))

# remove NA rows for investment
combined_cam_acces_ord <- combined_cam_acces_ord[-which(is.na(combined_cam_acces_ord$`Total Investment`)),]

camAccess_salesMedia_plot <- ggplot(combined_cam_acces_ord, aes(x = Month))
camAccess_salesMedia_plot <- camAccess_salesMedia_plot + geom_path(aes(y = combined_cam_acces_ord$`Monthly Sales`, colour = "Monthly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
camAccess_salesMedia_plot <- camAccess_salesMedia_plot + geom_path(aes(y = combined_cam_acces_ord$`Total Investment`/10, colour = "Total Investment",group=1),size=1)

camAccess_salesMedia_plot <- camAccess_salesMedia_plot + scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Total Investment"))

# modifying colours and theme options
camAccess_salesMedia_plot <- camAccess_salesMedia_plot + scale_colour_manual(values = c("orange", "green"))
camAccess_salesMedia_plot <- camAccess_salesMedia_plot + labs(y = "Monthly Sales",
              x = "Month",
              colour = "Legend",title="Camera Accessories")
camAccess_salesMedia_plot <- camAccess_salesMedia_plot + theme(legend.position = c(0.75, 0.82))
camAccess_salesMedia_plot


################### Sales - Gaming Accessories ########################


# order the sales data for gaming accessories by time
gam_access_df <- gam_access_df[order(gam_access_df$order_date),]
gam_access_df$week <- "NA"

for(i in 1:nrow(gam_access_df)){
  gam_access_df$week[i] <- computeWeek(gam_access_df$order_date[i])
}

# convert order payment type into dummy variables
levels(gam_access_df$s1_fact.order_payment_type) <-c(2,1)
gam_access_df$s1_fact.order_payment_type <- as.numeric(levels(gam_access_df$s1_fact.order_payment_type))[gam_access_df$s1_fact.order_payment_type]

# Evaluate total cost = no. of units * MRP
gam_access_df$TotalCostPrice <- gam_access_df$product_mrp * gam_access_df$units


# Calculate discount offered on each order
gam_access_df$discount <- (1 - (gam_access_df$gmv/gam_access_df$TotalCostPrice))*100


# summarize sales data per week for gaming accessories
wk_gam_access <- gam_access_df %>% group_by(gam_access_df$week) %>% summarize(Sales = sum(gmv), PaymentType=computeFrequency(s1_fact.order_payment_type), 
                                                                              Discount = median(discount), delbdays = median(delbdays), delcdays = median(delcdays))
#[which(wk_gam_access$PaymentType > 0),]$PaymentType <- 1
colnames(wk_gam_access) <- c("Week","Sales","PaymentType","Discount","delbdays","delcdays") 

# order weekly sales data by week numbers
wk_gam_access$Week <- as.numeric(wk_gam_access$Week)
wk_gam_access <- wk_gam_access[order(wk_gam_access$Week),]


# tag weeks with Promotion as 100 and rest with promotion as 10 in new column
# these values will help in logarithmic transformation since selecting 0,1 results in log(0) = Inf
wk_gam_access$PromotionWeek <- 1
wk_gam_access[which(wk_gam_access$Week %in% promotions_df$Week),]$PromotionWeek <- 2


# add Advertisement and NPS columns to weekly data
wk_gam_access <- merge(wk_gam_access,wk_nps_invst,by.x="Week",all=TRUE)


# Assumption: replace NPS and Advertisement for week 80 and 81 equal zero
wk_gam_access$NPS[54] <-  1
wk_gam_access$NPS[55] <-  1

# Assume all media investment with value 1
wk_gam_access[(which(wk_gam_access$Week == "80" | wk_gam_access$Week == "81")),c(9:15)] <- 1


ggplot(wk_gam_access, aes(x=Week, y=Sales, group=1))+ geom_point() + geom_line(, color = "red") +labs(title = "Gaming Accessories")


# Outliner treatment - identify outliners
boxplot(wk_gam_access$Discount, col="green",outcol="red",main="Weekly Gaming Accessories Sales Distribution", ylab="Discount")
wk_gam_access <- wk_gam_access[-55,]


# NPS Outliner treatment - identify outliners
boxplot(wk_gam_access$NPS, col="green",outcol="red",main="Weekly Gaming Accessories Sales Distribution", ylab="NPS")
quantile(wk_gam_access$NPS,seq(0,1,.01))

wk_gam_access$NPS[which(wk_gam_access$NPS>55.77257)] <- 55.77257

# summarize sales data per month for gaming accessories
mnth_gam_access <- gam_access_df %>% group_by(gam_access_df$Year,gam_access_df$Month) %>% summarize(WeeklySales = sum(gmv))
colnames(mnth_gam_access) <- c("Year","Month","Monthly Sales")
mnth_gam_access$Month <- paste(mnth_gam_access$Year,mnth_gam_access$Month,sep = "_")
mnth_gam_access <- mnth_gam_access[,-1]

# NA values for total investment for july 2016 - ignored
combined_gam_acces <- merge(mnth_gam_access,med_invst_df,by.x="Month",by.y = "YearMonth",sort = TRUE)
combined_gam_acces <- combined_gam_acces[,c(1,2,5)]
combined_gam_acces$`Total Investment` <-  as.numeric(as.character(combined_gam_acces$`Total Investment`))*10000000

# new dataframe with ordered month and year data
combined_gam_acces_ord <- rbind(combined_gam_acces[c(4,5,6),], combined_gam_acces[c(1,2,3),],combined_gam_acces[c(7,8,9,10,11,12),])
row.names(combined_gam_acces_ord) <- 1:12
combined_gam_acces_ord$Month <- factor(combined_gam_acces_ord$Month,levels = c("2015_7","2015_8","2015_9","2015_10","2015_11",
                                                                               "2015_12","2016_1","2016_2","2016_3","2016_4","2016_5","2016_6"))

gamAccess_salesMedia_plot <- ggplot(combined_gam_acces_ord, aes(x = Month))
gamAccess_salesMedia_plot <- gamAccess_salesMedia_plot + geom_path(aes(y = combined_gam_acces_ord$`Monthly Sales`, colour = "Monthly Sales",group=1),size=1)

# adding the Total Media Investment data
gamAccess_salesMedia_plot <- gamAccess_salesMedia_plot + geom_path(aes(y = combined_gam_acces_ord$`Total Investment`/10, colour = "Total Investment",group=1),size=1)

gamAccess_salesMedia_plot <- gamAccess_salesMedia_plot + scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Total Investment"))

# modifying colours and theme options
gamAccess_salesMedia_plot <- gamAccess_salesMedia_plot + scale_colour_manual(values = c("orange", "green"))
gamAccess_salesMedia_plot <- gamAccess_salesMedia_plot + labs(y = "Monthly Sales",
                                                              x = "Month",
                                                              colour = "Legend",title = "Gaming Accessories")
gamAccess_salesMedia_plot <- gamAccess_salesMedia_plot + theme(legend.position = c(0.75, 0.82))
gamAccess_salesMedia_plot

############################################################


# order the sales data for homing audio by time
hom_Audio_df <- hom_Audio_df[order(hom_Audio_df$order_date),]
hom_Audio_df$week <- "NA"

for(i in 1:nrow(hom_Audio_df)){
  hom_Audio_df$week[i] <- computeWeek(hom_Audio_df$order_date[i])
}

# convert order payment type into dummy variables
levels(hom_Audio_df$s1_fact.order_payment_type) <-c(2,1)
hom_Audio_df$s1_fact.order_payment_type <- as.numeric(levels(hom_Audio_df$s1_fact.order_payment_type))[hom_Audio_df$s1_fact.order_payment_type]

# Evaluate total cost = no. of units * MRP
hom_Audio_df$TotalCostPrice <- hom_Audio_df$product_mrp * hom_Audio_df$units

# Calculate discount offered on each order
hom_Audio_df$discount <- (1 - (hom_Audio_df$gmv/hom_Audio_df$TotalCostPrice))*100


# summarize sales data per week for gaming accessories
wk_hom_Audio <- hom_Audio_df %>% group_by(hom_Audio_df$week) %>% summarize(Sales = sum(gmv), PaymentType=computeFrequency(s1_fact.order_payment_type), 
                                                                           Discount = median(discount), delbdays = median(delbdays), delcdays = median(delcdays))
wk_hom_Audio[which(wk_hom_Audio$PaymentType > 0),]$PaymentType <- 1
colnames(wk_hom_Audio) <- c("Week","Sales","PaymentType","Discount","delbdays","delcdays")


# order weekly sales data by week numbers
wk_hom_Audio$Week <- as.numeric(wk_hom_Audio$Week)
wk_hom_Audio <- wk_hom_Audio[order(wk_hom_Audio$Week),]


# tag weeks with Promotion as 100 and rest with promotion as 10 in new column
# these values will help in logarithmic transformation since selecting 0,1 results in log(0) = Inf
wk_hom_Audio$PromotionWeek <- 1
wk_hom_Audio[which(wk_hom_Audio$Week %in% promotions_df$Week),]$PromotionWeek <- 2


# add Advertisement and NPS columns to weekly data
wk_hom_Audio <- merge(wk_hom_Audio,wk_nps_invst,by.x="Week",all=TRUE)


# Assumption: replace NPS and media investments for missing values

wk_hom_Audio[c(1,2),c(8:15)] <- 1
wk_hom_Audio[(which(wk_hom_Audio$Week == "80" | wk_hom_Audio$Week == "81")),c(8:15)] <- 1

# wk_cam_access$Investment[55] <- 1
# wk_cam_access$Investment[56] <- 1

# remove week 34 since there was no Sales reported for this week, neither it was a promotion week
wk_hom_Audio <- wk_hom_Audio[-c(9,10,11),]


# sales graph for camera accessory
ggplot(wk_hom_Audio, aes(x=Week, y=Sales, group=1))+ geom_point() + geom_line(color="blue") +labs(title = "Home Audio Accessories")

# Outliner treatment - identify outliners
boxplot(wk_hom_Audio$Discount, col="green",outcol="red",main="Weekly Home Audio Accessories Sales Distribution", ylab="Discount")
quantile(wk_hom_Audio$Discount,seq(0,1,.01))
wk_hom_Audio <- wk_hom_Audio[-2,]

boxplot(wk_hom_Audio$NPS, col="green",outcol="red",main="Weekly Home Audio Accessories Sales Distribution", ylab="NPS")
summary(wk_hom_Audio$NPS)

# replace NPS greater than 56.08114 with 56.08114 for outer fence outliers
quantile(wk_hom_Audio$NPS,seq(0,1,.01))
wk_hom_Audio$NPS[which(wk_hom_Audio$NPS>56.08114)] <- 56.08114


boxplot(wk_hom_Audio$Investment, col="green",outcol="red",main="Weekly Cam Accessories Sales Distribution", ylab="Investment")

# summarize sales data per month for home audio
mnth_hom_audio <- hom_Audio_df %>% group_by(hom_Audio_df$Year,hom_Audio_df$Month) %>% summarize(WeeklySales = sum(gmv))
colnames(mnth_hom_audio) <- c("Year","Month","Monthly Sales")
mnth_hom_audio$Month <- paste(mnth_hom_audio$Year,mnth_hom_audio$Month,sep = "_")
mnth_hom_audio <- mnth_hom_audio[,-1]

# NA values for total investment for May, June 2015 and july 2016 - ignored
combined_hom_audio <- merge(mnth_hom_audio,med_invst_df,by.x="Month",by.y = "YearMonth",sort = TRUE)
combined_hom_audio <- combined_hom_audio[,c(1,2,5)]
combined_hom_audio$`Total Investment` <- as.numeric(as.character(combined_hom_audio$`Total Investment`))*10000000

# new dataframe with ordered month and year data
combined_hom_audio_ord <- rbind(combined_hom_audio[c(4,5,6),], combined_hom_audio[c(1,2,3),],combined_hom_audio[c(7,8,9,10,11,12),])
row.names(combined_hom_audio_ord) <- 1:12
combined_hom_audio_ord$Month <- factor(combined_hom_audio_ord$Month,levels = c("2015_7","2015_8","2015_9","2015_10","2015_11",
                                                                               "2015_12","2016_1","2016_2","2016_3","2016_4","2016_5","2016_6"))

homAudio_salesMedia_plot <- ggplot(combined_hom_audio_ord, aes(x = Month))
homAudio_salesMedia_plot <- homAudio_salesMedia_plot + geom_path(aes(y = combined_hom_audio_ord$`Monthly Sales`, colour = "Monthly Sales",group=1),size=1)

# adding the Total Media Investment data
homAudio_salesMedia_plot <- homAudio_salesMedia_plot + geom_path(aes(y = combined_hom_audio_ord$`Total Investment`/10, colour = "Total Investment",group=1),size=1)

homAudio_salesMedia_plot <- homAudio_salesMedia_plot + scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Total Investment"))

# modifying colours and theme options
homAudio_salesMedia_plot <- homAudio_salesMedia_plot + scale_colour_manual(values = c("orange", "green"))
homAudio_salesMedia_plot <- homAudio_salesMedia_plot + labs(title = "Home Audio", y = "Monthly Sales",
                                                              x = "Month",
                                                              colour = "Legend")
homAudio_salesMedia_plot <- homAudio_salesMedia_plot + theme(legend.position = c(0.75, 0.82))
homAudio_salesMedia_plot



# Category - PaymentType Sales Distribution
# Sales with Payment Type of COD is high for all categories
ggplot(wk_cam_access,aes(as.factor(PaymentType), fill = PaymentType)) + geom_bar(aes(weight = Sales)) + labs(title = "Camera Accessories - Payment Type", x= 'Payment Type', y = 'Sales')

# Game Accessories orders are all COD
ggplot(wk_gam_access,aes(as.factor(PaymentType), fill = 'PaymentType')) + geom_bar(aes(weight = Sales)) + labs(title = "Game Accessories - Payment Type", x= 'Payment Type', y = 'Sales')

# Home Audio orders are all COD
ggplot(wk_hom_Audio,aes(as.factor(PaymentType), fill = 'PaymentType')) + geom_bar(aes(weight = Sales)) + labs(title = "Home Audio - Payment Type", x= 'Payment Type', y = 'Sales') 



################## Building Advertising Impact ##########################

# med_invst_df$`Total Investment` <- as.numeric(as.character(med_invst_df$`Total Investment`))

# computeLags <- function(n,df){
#  for (i in 1:n){
#    if(i == 1){
#      adimpact<- data.frame(df[1:(nrow(df)-1),length(names(df))] * adstock)
#      adimpact <- rbind(c(0),adimpact) 
#    } else{
#      adimpact<- data.frame(df[1:nrow(df),length(names(df))] * adstock)
#    }
#    colnames(adimpact) <- paste("adimpact",i,sep="_")
#    df <- cbind(df,adimpact)
#  }
#  return(df)
#}
  
################################ Model - Camera Accessories #############################

#########################

wk_cam_access$Week <- as.numeric(wk_cam_access$Week)

wk_cam_inves_plot <- ggplot(wk_cam_access, aes(x = Week))
wk_cam_inves_plot <- wk_cam_inves_plot + geom_line(aes(y = wk_cam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_cam_inves_plot <- wk_cam_inves_plot + geom_line(aes(y = wk_cam_access$Investment/10, colour = "Adv Investment",group=1),size=1)

wk_cam_inves_plot <- wk_cam_inves_plot + scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Adv Investment"))

# modifying colours and theme options
wk_cam_inves_plot <- wk_cam_inves_plot + scale_colour_manual(values = c("orange", "green"))
wk_cam_inves_plot <- wk_cam_inves_plot + labs(y = "Weekly Sales",
                                              x = "Week",
                                              colour = "Legend",title="Camera Accessories")
wk_cam_inves_plot <- wk_cam_inves_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_cam_inves_plot <- wk_cam_inves_plot + geom_vline(xintercept = wk_cam_access[which(wk_cam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_cam_inves_plot

############# TV Lag: less likely impact

wk_cam_TV_plot <- ggplot(wk_cam_access, aes(x = Week))
wk_cam_TV_plot <- wk_cam_TV_plot + geom_line(aes(y = wk_cam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_cam_TV_plot <- wk_cam_TV_plot + geom_line(aes(y = wk_cam_access$TV, colour = "TV Investment",group=1),size=1)

wk_cam_TV_plot <- wk_cam_TV_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "TV Investment"))

# modifying colours and theme options
wk_cam_TV_plot <- wk_cam_TV_plot + scale_colour_manual(values = c("orange", "green"))
wk_cam_TV_plot <- wk_cam_TV_plot + labs(y = "Weekly Sales",
                                              x = "Week",
                                              colour = "Legend",title="Camera Accessories")
wk_cam_TV_plot <- wk_cam_TV_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_cam_TV_plot <- wk_cam_TV_plot + geom_vline(xintercept = wk_cam_access[which(wk_cam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_cam_TV_plot



################### Digital Investment: lag of 2 weeks

wk_cam_Digital_plot <- ggplot(wk_cam_access, aes(x = Week))
wk_cam_Digital_plot <- wk_cam_Digital_plot + geom_line(aes(y = wk_cam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_cam_Digital_plot <- wk_cam_Digital_plot + geom_line(aes(y = wk_cam_access$Digital, colour = "Digital Investment",group=1),size=1)

wk_cam_Digital_plot <- wk_cam_Digital_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Digital Investment"))

# modifying colours and theme options
wk_cam_Digital_plot <- wk_cam_Digital_plot + scale_colour_manual(values = c("orange", "green"))
wk_cam_Digital_plot <- wk_cam_Digital_plot + labs(y = "Weekly Sales",
                                        x = "Week",
                                        colour = "Legend",title="Camera Accessories")
wk_cam_Digital_plot <- wk_cam_Digital_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_cam_Digital_plot <- wk_cam_Digital_plot + geom_vline(xintercept = wk_cam_access[which(wk_cam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_cam_Digital_plot


################### Sponsorship Investment: less lag impact

wk_cam_Sponsorship_plot <- ggplot(wk_cam_access, aes(x = Week))
wk_cam_Sponsorship_plot <- wk_cam_Sponsorship_plot + geom_line(aes(y = wk_cam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_cam_Sponsorship_plot <- wk_cam_Sponsorship_plot + geom_line(aes(y = wk_cam_access$Sponsorship/10, colour = "Sponsorship Investment",group=1),size=1)

wk_cam_Sponsorship_plot <- wk_cam_Sponsorship_plot + scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Sponsorship Investment"))

# modifying colours and theme options
wk_cam_Sponsorship_plot <- wk_cam_Sponsorship_plot + scale_colour_manual(values = c("orange", "green"))
wk_cam_Sponsorship_plot <- wk_cam_Sponsorship_plot + labs(y = "Weekly Sales",
                                                  x = "Week",
                                                  colour = "Legend",title="Camera Accessories")
wk_cam_Sponsorship_plot <- wk_cam_Sponsorship_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_cam_Sponsorship_plot <- wk_cam_Sponsorship_plot + geom_vline(xintercept = wk_cam_access[which(wk_cam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_cam_Sponsorship_plot


################### Content Marketing Investment : less lag impact

wk_cam_ContMarketing_plot <- ggplot(wk_cam_access, aes(x = Week))
wk_cam_ContMarketing_plot <- wk_cam_ContMarketing_plot + geom_line(aes(y = wk_cam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_cam_ContMarketing_plot <- wk_cam_ContMarketing_plot + geom_line(aes(y = wk_cam_access$ContentMarketing, colour = "Content Marketing Investment",group=1),size=1)

wk_cam_ContMarketing_plot <- wk_cam_ContMarketing_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Content Marketing Investment"))

# modifying colours and theme options
wk_cam_ContMarketing_plot <- wk_cam_ContMarketing_plot + scale_colour_manual(values = c("orange", "green"))
wk_cam_ContMarketing_plot <- wk_cam_ContMarketing_plot + labs(y = "Weekly Sales",
                                                          x = "Week",
                                                          colour = "Legend",title="Camera Accessories")
wk_cam_ContMarketing_plot <- wk_cam_ContMarketing_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_cam_ContMarketing_plot <- wk_cam_ContMarketing_plot + geom_vline(xintercept = wk_cam_access[which(wk_cam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_cam_ContMarketing_plot


################### Online Investment :less lag impact

wk_cam_Online_plot <- ggplot(wk_cam_access, aes(x = Week))
wk_cam_Online_plot <- wk_cam_Online_plot + geom_line(aes(y = wk_cam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_cam_Online_plot <- wk_cam_Online_plot + geom_line(aes(y = wk_cam_access$Online, colour = "Online Investment",group=1),size=1)

wk_cam_Online_plot <- wk_cam_Online_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Online Investment"))

# modifying colours and theme options
wk_cam_Online_plot <- wk_cam_Online_plot + scale_colour_manual(values = c("orange", "green"))
wk_cam_Online_plot <- wk_cam_Online_plot + labs(y = "Weekly Sales",
                                                              x = "Week",
                                                              colour = "Legend",title="Camera Accessories")
wk_cam_Online_plot <- wk_cam_Online_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_cam_Online_plot <- wk_cam_Online_plot + geom_vline(xintercept = wk_cam_access[which(wk_cam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_cam_Online_plot


################### SEM Investment : less lag impact

wk_cam_SEM_plot <- ggplot(wk_cam_access, aes(x = Week))
wk_cam_SEM_plot <- wk_cam_SEM_plot + geom_line(aes(y = wk_cam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_cam_SEM_plot <- wk_cam_SEM_plot + geom_line(aes(y = wk_cam_access$SEM, colour = "SEM Investment",group=1),size=1)

wk_cam_SEM_plot <- wk_cam_SEM_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "SEM Investment"))

# modifying colours and theme options
wk_cam_SEM_plot <- wk_cam_SEM_plot + scale_colour_manual(values = c("orange", "green"))
wk_cam_SEM_plot <- wk_cam_SEM_plot + labs(y = "Weekly Sales",
                                                x = "Week",
                                                colour = "Legend",title="Camera Accessories")
wk_cam_SEM_plot <- wk_cam_SEM_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_cam_SEM_plot <- wk_cam_SEM_plot + geom_vline(xintercept = wk_cam_access[which(wk_cam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_cam_SEM_plot


################### Affliates Investment: less lag impact

wk_cam_Affliates_plot <- ggplot(wk_cam_access, aes(x = Week))
wk_cam_Affliates_plot <- wk_cam_Affliates_plot + geom_line(aes(y = wk_cam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_cam_Affliates_plot <- wk_cam_Affliates_plot + geom_line(aes(y = wk_cam_access$Affliates, colour = "Affliates Investment",group=1),size=1)

wk_cam_Affliates_plot <- wk_cam_Affliates_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Affliates Investment"))

# modifying colours and theme options
wk_cam_Affliates_plot <- wk_cam_Affliates_plot + scale_colour_manual(values = c("orange", "green"))
wk_cam_Affliates_plot <- wk_cam_Affliates_plot + labs(y = "Weekly Sales",
                                          x = "Week",
                                          colour = "Legend",title="Camera Accessories")
wk_cam_Affliates_plot <- wk_cam_Affliates_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_cam_Affliates_plot <- wk_cam_Affliates_plot + geom_vline(xintercept = wk_cam_access[which(wk_cam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_cam_Affliates_plot

###############################################




wk_cam_access$adImpact_TV <- stats::filter(wk_cam_access$TV,c(.001),method="recursive")
wk_cam_access$adImpact_Digital <- stats::filter(wk_cam_access$Digital,c(.10,.10),method="recursive")
wk_cam_access$adImpact_Sponsorship <- stats::filter(wk_cam_access$Sponsorship,c(.001),method="recursive")
wk_cam_access$adImpact_ContentMarketing<- stats::filter(wk_cam_access$ContentMarketing,c(.001),method="recursive")
wk_cam_access$adImpact_Online <- stats::filter(wk_cam_access$Online,c(.001),method="recursive")
wk_cam_access$adImpact_SEM <- stats::filter(wk_cam_access$SEM,c(.001),method="recursive")
wk_cam_access$adImpact_Affliates <- stats::filter(wk_cam_access$Affliates,c(.001),method="recursive")


# Sales (GMV) will also have a lag effect 
#wk_cam_access$impactedSales <- stats::filter(wk_cam_access$Sales,c(.15,.10,.05),method="recursive")
#wk_cam_access$salesLag <- wk_cam_access$impactedSales - wk_cam_access$Sales

#wk_cam_access$salesLag <- 0
wk_cam_access$salesLag <- lag(wk_cam_access$Sales,1)
# put NA equals zero in first row for sales LAG 
wk_cam_access$salesLag[1] <- 0

# calculate media lags
wk_cam_access$TVLag <- wk_cam_access$adImpact_TV -wk_cam_access$TV
wk_cam_access$DigitalLag <- wk_cam_access$adImpact_Digital - wk_cam_access$Digital
wk_cam_access$SponsorshipLag <- wk_cam_access$adImpact_Sponsorship - wk_cam_access$Sponsorship
wk_cam_access$ContentMarketingLag <- wk_cam_access$adImpact_ContentMarketing -wk_cam_access$ContentMarketing
wk_cam_access$OnlineLag <- wk_cam_access$adImpact_Online -wk_cam_access$Online
wk_cam_access$SEMLag <- wk_cam_access$adImpact_SEM -wk_cam_access$SEM
wk_cam_access$AffliatesLag <- wk_cam_access$adImpact_Affliates -wk_cam_access$Affliates


# scale raw data
wk_cam_access$sc_TV <- scale(wk_cam_access$TV, center = FALSE, scale = TRUE)
wk_cam_access$sc_Digital <- scale(wk_cam_access$Digital, center = FALSE, scale = TRUE)
wk_cam_access$sc_Sponsorship <- scale(wk_cam_access$Sponsorship, center = FALSE, scale = TRUE)
wk_cam_access$sc_ContentMarketing <- scale(wk_cam_access$ContentMarketing, center = FALSE, scale = TRUE)
wk_cam_access$sc_Online <- scale(wk_cam_access$Online, center = FALSE, scale = TRUE)
wk_cam_access$sc_SEM <- scale(wk_cam_access$SEM, center = FALSE, scale = TRUE)
wk_cam_access$sc_Affliates <- scale(wk_cam_access$Affliates, center = FALSE, scale = TRUE)

wk_cam_access$salesLag <- scale(wk_cam_access$salesLag, center = FALSE, scale = TRUE)
wk_cam_access$TVLag <- scale(wk_cam_access$TVLag, center = FALSE, scale = TRUE)
wk_cam_access$DigitalLag <- scale(wk_cam_access$DigitalLag, center = FALSE, scale = TRUE)
wk_cam_access$SponsorshipLag <- scale(wk_cam_access$SponsorshipLag, center = FALSE, scale = TRUE)
wk_cam_access$ContentMarketingLag <- scale(wk_cam_access$ContentMarketingLag, center = FALSE, scale = TRUE)
wk_cam_access$OnlineLag <- scale(wk_cam_access$OnlineLag, center = FALSE, scale = TRUE)
wk_cam_access$SEMLag <- scale(wk_cam_access$SEMLag, center = FALSE, scale = TRUE)
wk_cam_access$AffliatesLag <- scale(wk_cam_access$AffliatesLag, center = FALSE, scale = TRUE)


#wk_cam_access$salesLag <- wk_cam_access$salesLag - (.4*(wk_cam_access^2) + .3*wk_cam_access$salesLag)

dfMod_cam_acces <- wk_cam_access



# # Divide into training and test data set
#set the seed to 100. 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(wk_cam_access), 0.7*nrow(wk_cam_access))
# generate the train data set
train = wk_cam_access[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = wk_cam_access[-trainindices,]



# # Divide into training and test data set
#set the seed to 100. 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(dfMod_cam_acces), 0.7*nrow(dfMod_cam_acces))
# generate the train data set
train = dfMod_cam_acces[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = dfMod_cam_acces[-trainindices,]

# # Divide into training and test data set

#################### Basic Linear Regression model with no lag

b_train = train[,c(2:8,32:38)]
basic_cam_access_model1 <- lm(Sales ~ ., data=b_train)

# Multiple R-squared:  0.6979,	Adjusted R-squared:  .5195
summary(basic_cam_access_model1) 
vif(basic_cam_access_model1)

#Execute the first model_1 with stepAIC in the training set. 
step <- stepAIC(basic_cam_access_model1, direction="both")


basic_cam_access_model2 <- lm(Sales ~   delcdays + PromotionWeek + NPS + sc_TV + sc_Digital + 
                                sc_ContentMarketing, data=b_train)
#basic_cam_access_model2 <- lm(Sales ~ scale_PromotionWeek + scale_Discount + scale_Advertisement, data=train[,c(2,11:14)])

# Multiple R-squared:  0.6494,	Adjusted R-squared:  0.5909
summary(basic_cam_access_model2) 
vif(basic_cam_access_model2)



basic_cam_access_model3 <- lm(Sales ~   delcdays + PromotionWeek + NPS + sc_TV + sc_ContentMarketing , data=b_train)
#basic_cam_access_model2 <- lm(Sales ~ scale_PromotionWeek + scale_Discount + scale_Advertisement, data=train[,c(2,11:14)])

# Multiple R-squared:  0.6031,	Adjusted R-squared:  0.5518
summary(basic_cam_access_model3) 
vif(basic_cam_access_model3)



# basic_cam_access_model4 <- lm(Sales ~  PromotionWeek + NPS +  sc_Sponsorship , data=b_train)
# #basic_cam_access_model2 <- lm(Sales ~ scale_PromotionWeek + scale_Discount + scale_Advertisement, data=train[,c(2,11:14)])
# 
# # Multiple R-squared:  0.5516,	Adjusted R-squared:  0.5095
# summary(basic_cam_access_model4) 
# vif(basic_cam_access_model4)

# plot Residuals vs Fitted
op <- par(mfrow=c(2,2))
plot(basic_cam_access_model3)
par(op)

# Predict the house prices in the testing dataset
Predict_1 <- predict(basic_cam_access_model3,test[,-1])
test$bas_predicted_sale <- Predict_1
# Predict_1 <- predict(basic_cam_access_model2,test[,-1])
# test$bas_predicted_sale <- Predict_1


# calculate r-squared:.1510069
r <- cor(test$Sales,test$bas_predicted_sale)
rsquared <- cor(test$Sales,test$bas_predicted_sale)^2
rsquared

# calculate error on test data
test$error <- test$bas_predicted_sale - test$Sales

# calculate RMSE on test data: 2345414
computeRMSE(test$error)


# Plot - Actual vs Predicted Views Model
ggplot(test, aes(Week, Sales)) + geom_line(color = "blue",group=1 ) + geom_line(aes(Week,bas_predicted_sale), color="red", group=1) +labs(title = "Basic Linear Model")


################### Multiplicative Model

ggplot(dfMod_cam_acces,aes(Discount,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(NPS,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(TVLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(salesLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(DigitalLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(SponsorshipLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(ContentMarketingLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(OnlineLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(SEMLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(AffliatesLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(delbdays,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(delcdays,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(TV,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(Digital,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(Sponsorship,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(ContentMarketing,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(Online,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(SEM,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_cam_acces,aes(Affliates,Sales)) + geom_point() + geom_smooth()



ln_train <- train[,c(2:15)]
mul_cam_access_model1 <- lm(log(Sales) ~ PaymentType + PromotionWeek + Discount + delbdays + delcdays +NPS + log(TV) + log(Digital) 
                            + log(Sponsorship) + log(ContentMarketing) + log(Online) + log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  .9389,	Adjusted R-squared:  0.9028
summary(mul_cam_access_model1) 
vif(mul_cam_access_model1)

#Execute the first model_1 with stepAIC in the training set. 
step <- stepAIC(mul_cam_access_model1, direction="both")

mul_cam_access_model2 <- lm(log(Sales) ~ PromotionWeek + Discount + delbdays + log(TV) + 
                              log(Digital) + log(Sponsorship) + log(ContentMarketing) + 
                              log(Online) + log(SEM) + log(Affliates) ,na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.9247,	Adjusted R-squared:  0.9176
summary(mul_cam_access_model2) 
vif(mul_cam_access_model2)


mul_cam_access_model3 <- lm(log(Sales) ~ PromotionWeek + Discount + delbdays + log(Digital) + log(Sponsorship) + log(ContentMarketing) + 
                              log(Online) + log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.92,	Adjusted R-squared:  0.9152
summary(mul_cam_access_model3) 
vif(mul_cam_access_model3)


mul_cam_access_model4 <- lm(log(Sales) ~ PromotionWeek + Discount + delbdays + log(Digital) + log(ContentMarketing) + 
                              log(Online) + log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.92,	Adjusted R-squared:  0.9152
summary(mul_cam_access_model4) 
vif(mul_cam_access_model4)


mul_cam_access_model5 <- lm(log(Sales) ~ PromotionWeek + Discount + delbdays  + log(ContentMarketing) + 
                              log(Online) + log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.92,	Adjusted R-squared:  0.9152
summary(mul_cam_access_model5) 
vif(mul_cam_access_model5)


mul_cam_access_model6 <- lm(log(Sales) ~ PromotionWeek + Discount + delbdays  + log(ContentMarketing) + 
                               log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.92,	Adjusted R-squared:  0.9152
summary(mul_cam_access_model6) 
vif(mul_cam_access_model6)


mul_cam_access_model7 <- lm(log(Sales) ~ PromotionWeek + Discount + delbdays  +  
                              log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.92,	Adjusted R-squared:  0.9152
summary(mul_cam_access_model7) 
vif(mul_cam_access_model7)


mul_cam_access_model8 <- lm(log(Sales) ~ PromotionWeek + Discount + delbdays + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.92,	Adjusted R-squared:  0.9152
summary(mul_cam_access_model8) 
vif(mul_cam_access_model8)


mul_cam_access_model9 <- lm(log(Sales) ~ PromotionWeek + Discount +  log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.92,	Adjusted R-squared:  0.9152
summary(mul_cam_access_model9) 
vif(mul_cam_access_model9)


mul_cam_access_model10 <- lm(log(Sales) ~ PromotionWeek +   log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.92,	Adjusted R-squared:  0.9152
summary(mul_cam_access_model10) 
vif(mul_cam_access_model10)


mul_cam_access_model11 <- lm(log(Sales) ~ log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.92,	Adjusted R-squared:  0.9152
summary(mul_cam_access_model11) 
vif(mul_cam_access_model11)





# plot Residuals vs Fitted
op<-par(mfrow=c(2,2))
plot(mul_cam_access_model11)
par(op)

# Predict the sales in the testing dataset
Predict_1 <- predict(mul_cam_access_model11,test[,-1])
test$log_predicted_sale <- Predict_1


# Calculate RMSE: .4421 on train
ln_RSS_train <- c(crossprod(mul_cam_access_model11$residuals))
ln_RSS_train

ln_MSE_train <- ln_RSS_train / length(mul_cam_access_model11$residuals)
ln_MSE_train

ln_RMSE_train <- sqrt(ln_MSE_train)
ln_RMSE_train


# Predict the sales in the testing dataset
test$ln_predicted_sale <- exp(test$log_predicted_sale)

# calculate error on test data
test$error <- test$ln_predicted_sale - test$Sales

# calculate RMSE on test data: 1968154
computeRMSE(test$error)

ggplot(test, aes(Week, Sales)) + geom_line(color = "blue", group =1 ) + geom_line(aes(Week,ln_predicted_sale), color="red", group = 1) 


####################  Koyck Model

k_train=train[,c(2:8,24,32:38)]
ky_cam_access_model1 <- lm(Sales ~.,data=k_train)

# Multiple R-squared:  0.7431,	Adjusted R-squared:  0.5718
summary(ky_cam_access_model1) 
vif(ky_cam_access_model1)

#Execute the first model_1 with stepAIC in the training set. 
step <- stepAIC(ky_cam_access_model1, direction="both")


ky_cam_access_model2 <- lm(Sales ~ delcdays + PromotionWeek + NPS + salesLag + sc_TV + sc_Digital + 
                             sc_ContentMarketing, data=k_train)

# Multiple R-squared:  0.7255,	Adjusted R-squared:  0.6569
summary(ky_cam_access_model2) 
vif(ky_cam_access_model2)


ky_cam_access_model3 <- lm(Sales ~   delcdays + PromotionWeek + NPS + salesLag + sc_TV + sc_ContentMarketing, data=k_train)

# Multiple R-squared:  0.7054,	Adjusted R-squared:  0.6445
summary(ky_cam_access_model3) 
vif(ky_cam_access_model3)


ky_cam_access_model4 <- lm(Sales ~   delcdays + PromotionWeek + NPS + sc_TV + sc_ContentMarketing, data=k_train)

# Multiple R-squared:  0.609,	Adjusted R-squared:  0.5438
summary(ky_cam_access_model4) 
vif(ky_cam_access_model4)

# 
# ky_cam_access_model5 <- lm(Sales ~   PromotionWeek + NPS + salesLag +  
#                              sc_Sponsorship , data=k_train)
# 
# # Multiple R-squared:  0.6065,	Adjusted R-squared:  0.5557
# # RMSE: 2854138
# summary(ky_cam_access_model5) 
# vif(ky_cam_access_model5)
# 
# 
# ky_cam_access_model6 <- lm(Sales ~   PromotionWeek + NPS + sc_Sponsorship , data=k_train)
# 
# # Multiple R-squared:  0.5516,	Adjusted R-squared:  0.5095
# summary(ky_cam_access_model6) 
# vif(ky_cam_access_model6)

# plot Residuals vs Fitted
op <- par(mfrow = c(2, 2))
plot(ky_cam_access_model4)
par(op)

# Predict the house prices in the testing dataset
Predict_1 <- predict(ky_cam_access_model4,test[,-1])
test$ky_predicted_sale <- Predict_1


# Calculate RMSE: 1588457 on train data
ky_RSS_train <- c(crossprod(ky_cam_access_model4$residuals))
ky_RSS_train

ky_MSE_train <- ky_RSS_train / length(ky_cam_access_model4$residuals)
ky_MSE_train

ky_RMSE_train <- sqrt(ky_MSE_train)
ky_RMSE_train


# calculate error on test data
test$error <- test$ky_predicted_sale - test$Sales

# calculate RMSE on test data: 2345414
computeRMSE(test$error)

# calculate r-squared: 0.1510069
r <- cor(test$Sales,test$ky_predicted_sale)
rsquared <- cor(test$Sales,test$ky_predicted_sale)^2
rsquared


# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(Week, Sales)) + geom_line(color = "blue" ,group=1) + geom_line(aes(Week,ky_predicted_sale), color="red",group=1) 


################ Distribute Lag

dn_train <- train[,c(2:8,24:38)]
dis_cam_access_model1 <- lm(Sales ~ . ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.802,	Adjusted R-squared:  0.5051
summary(dis_cam_access_model1) 
vif(dis_cam_access_model1)

step <- stepAIC(dis_cam_access_model1, direction="both")

# dis_cam_access_model2 <- lm(Sales ~ Discount + PromotionWeek + NPS + salesLag + sc_Digital + 
#                               sc_Sponsorship + sc_ContentMarketing ,na.action = na.exclude, data=dn_train)
# 
# # Multiple R-squared:  0.7255,	Adjusted R-squared:  0.6569
# summary(dis_cam_access_model2) 
# vif(dis_cam_access_model2)

dis_cam_access_model2 <- lm(Sales ~  PaymentType + delbdays + PromotionWeek + NPS + salesLag + 
                              OnlineLag + SEMLag + AffliatesLag + sc_TV + sc_ContentMarketing + 
                              sc_Online + sc_SEM + sc_Affliates,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.7881,	Adjusted R-squared:  0.6469
summary(dis_cam_access_model2) 
vif(dis_cam_access_model2)


dis_cam_access_model3 <- lm(Sales ~  PaymentType + PromotionWeek + NPS + salesLag + 
                              OnlineLag + SEMLag + AffliatesLag + sc_TV + sc_ContentMarketing + 
                              sc_Online + sc_SEM + sc_Affliates ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  .7687,	Adjusted R-squared:  0.632
summary(dis_cam_access_model3) 
vif(dis_cam_access_model3)



dis_cam_access_model4 <- lm(Sales ~  PromotionWeek + NPS + salesLag + OnlineLag + SEMLag + AffliatesLag + sc_TV + sc_ContentMarketing + 
                              sc_Online + sc_SEM + sc_Affliates,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.7463,	Adjusted R-squared:  0.614
summary(dis_cam_access_model4) 
vif(dis_cam_access_model4)


dis_cam_access_model5 <- lm(Sales ~ PromotionWeek + NPS + salesLag +  SEMLag + AffliatesLag + sc_TV + sc_ContentMarketing + 
                              sc_Online + sc_SEM + sc_Affliates ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.7378,	Adjusted R-squared:  0.6176 
summary(dis_cam_access_model5) 
vif(dis_cam_access_model5)


dis_cam_access_model6 <- lm(Sales ~ PromotionWeek + NPS + salesLag +  SEMLag + AffliatesLag + sc_TV + sc_ContentMarketing + 
                              sc_Online +  sc_Affliates,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.7063,	Adjusted R-squared:  0.5889
summary(dis_cam_access_model6) 
vif(dis_cam_access_model6)


dis_cam_access_model7 <- lm(Sales ~ PromotionWeek + NPS + salesLag +  SEMLag + AffliatesLag + sc_TV + sc_Online +  sc_Affliates 
                            ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.706,	Adjusted R-squared:  0.6043
summary(dis_cam_access_model7) 
vif(dis_cam_access_model7)


dis_cam_access_model8 <- lm(Sales ~ PromotionWeek + NPS + salesLag +  AffliatesLag + sc_TV + sc_Online +  sc_Affliates ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.6977,	Adjusted R-squared:  0.6081
summary(dis_cam_access_model8) 
vif(dis_cam_access_model8)


dis_cam_access_model9 <- lm(Sales ~ PromotionWeek + NPS + salesLag +  AffliatesLag + sc_TV +   sc_Online  ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.6875,	Adjusted R-squared:  0.6094
summary(dis_cam_access_model9) 
vif(dis_cam_access_model9)


dis_cam_access_model10 <- lm(Sales ~ PromotionWeek + NPS + salesLag +   sc_TV +   sc_Online ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.6782,	Adjusted R-squared:  0.6116
summary(dis_cam_access_model10) 
vif(dis_cam_access_model10)


dis_cam_access_model11 <- lm(Sales ~ PromotionWeek + NPS + salesLag +   sc_Online   ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.6534,	Adjusted R-squared:  0.5956
summary(dis_cam_access_model11) 
vif(dis_cam_access_model11)


dis_cam_access_model12 <- lm(Sales ~  PromotionWeek + NPS + sc_Online  ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.6065,	Adjusted R-squared:  0.5557
# RMSE: 2854138
summary(dis_cam_access_model12) 
vif(dis_cam_access_model12)


dis_cam_access_model13 <- lm(Sales ~  PromotionWeek +   sc_Online  ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.5516,	Adjusted R-squared:  0.5095
# RMSE: 2854138
summary(dis_cam_access_model13) 
vif(dis_cam_access_model13)


# plot Residuals vs Fitted
op <- par(mfrow = c(2, 2))
plot(dis_cam_access_model13)
par(op)

# Predict the house prices in the testing dataset
Predict_1 <- predict(dis_cam_access_model13,test[,-1])
test$dy_predicted_sale <- Predict_1


# Calculate RMSE: 1488007 on train data
dy_RSS_train <- c(crossprod(dis_cam_access_model13$residuals))
dy_RSS_train

dy_MSE_train <- dy_RSS_train / length(dis_cam_access_model13$residuals)
dy_MSE_train

dy_RMSE_train <- sqrt(dy_MSE_train)
dy_RMSE_train


# calculate error on test data
test$error <- test$dy_predicted_sale - test$Sales

# calculate RMSE on test data: 2345414                     
computeRMSE(test$error)

# calculate r-squared: 0.01124042
r <- cor(test$Sales,test$dy_predicted_sale)
rsquared <- cor(test$Sales,test$dy_predicted_sale)^2
rsquared


# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(Week, Sales)) + geom_line(color = "blue" ,group=1) + geom_line(aes(Week,dy_predicted_sale), color="red",group=1) 

##############

mx_train <- train[,c(2,24:38)]


# find correlation between Sales and other variables
cor(dfMod_cam_acces[,-1])


# find interaction between variables
interaction_fit1 <- lm(log(Sales) ~ ( TVLag+AffliatesLag+SEMLag+ContentMarketingLag+SponsorshipLag+DigitalLag+OnlineLag)^2,data=train[,c(2,25:31)])
anova(interaction_fit1)

interaction_fit2 <- lm(log(Sales) ~ (sc_Affliates+sc_SEM+sc_Online+sc_ContentMarketing+sc_Sponsorship+sc_Digital+sc_TV)^2,data=train[,c(2,32:38)])
anova(interaction_fit2)

# TVLag and ContentMarketingLag have interaction
# RMSE on test: 2513942
mx_cam_access_model1 <- lm(log(Sales) ~ TVLag*ContentMarketingLag,na.action = na.exclude, data=train[,c(2,25:31)])

# Multiple R-squared:  0.2042,	Adjusted R-squared:  0.2042
summary(mx_cam_access_model1) 
vif(mx_cam_access_model1)


mx_cam_access_model2 <- lm(log(Sales) ~ TVLag*ContentMarketingLag + SEMLag , data=mx_train)
# Multiple R-squared:  0.3741,	Adjusted R-squared:  0.2934
# RMSE on test 2719685
summary(mx_cam_access_model2) 
vif(mx_cam_access_model2)


mx_cam_access_model3 <- lm(log(Sales)  ~ TVLag*ContentMarketingLag + poly(Digital,4,raw=TRUE)   , data=train[,c(2:4,7:15,24:38)])

# Multiple R-squared:  0.7396,	Adjusted R-squared:  0.6745
#Very high RMSE
summary(mx_cam_access_model3) 
vif(mx_cam_access_model3)


mx_cam_access_model4 <- lm(log(Sales)  ~  TVLag*ContentMarketingLag +poly(Digital,4,raw=TRUE) +  poly(Discount,4,raw=TRUE), data=train[,c(2:4,7:15,24:38)])
# Multiple R-squared:  0.5265,	Adjusted R-squared:  0.4834
#Very high RMSE
summary(mx_cam_access_model4) 
vif(mx_cam_access_model4)

# mx_cam_access_model1 is the most stable interaction variable model
# plot Residuals vs Fitted
op <- par(mfrow = c(2, 2))
plot(mx_cam_access_model2)
par(op)


# Predict the sales in the testing dataset
Predict_1 <- predict(mx_cam_access_model2,test[,-1])
test$lmx_predicted_sale <- Predict_1


# Calculate RMSE on train data (log scale)
mx_RSS_train <- c(crossprod(mx_cam_access_model2$residuals))
mx_RSS_train

mx_MSE_train <- mx_RSS_train / length(mx_cam_access_model2$residuals)
mx_MSE_train

mx_RMSE_train <- sqrt(mx_MSE_train)
mx_RMSE_train


# Predict the sales in the testing dataset
test$mx_predicted_sale <- exp(test$lmx_predicted_sale)

# calculate error on test data
test$error <- test$mx_predicted_sale - test$Sales

# calculate RMSE on test data
computeRMSE(test$error)

ggplot(test, aes(Week, Sales)) + geom_line(color = "blue", group =1 ) + geom_line(aes(Week,mx_predicted_sale), color="red", group = 1) 

 
#########################################################################################


################################### Model - Gaming Accessories ##############################



#########################

wk_gam_access$Week <- as.numeric(wk_gam_access$Week)

wk_gam_inves_plot <- ggplot(wk_gam_access, aes(x = Week))
wk_gam_inves_plot <- wk_gam_inves_plot + geom_line(aes(y = wk_gam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_gam_inves_plot <- wk_gam_inves_plot + geom_line(aes(y = wk_gam_access$Investment/10, colour = "Adv Investment",group=1),size=1)

wk_gam_inves_plot <- wk_gam_inves_plot + scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Adv Investment"))

# modifying colours and theme options
wk_gam_inves_plot <- wk_gam_inves_plot + scale_colour_manual(values = c("orange", "green"))
wk_gam_inves_plot <- wk_gam_inves_plot + labs(y = "Weekly Sales",
                                              x = "Week",
                                              colour = "Legend",title="Gaming Accessories")
wk_gam_inves_plot <- wk_gam_inves_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_gam_inves_plot <- wk_gam_inves_plot + geom_vline(xintercept = wk_gam_access[which(wk_gam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_gam_inves_plot

############# TV Lag: less likely impact

wk_gam_TV_plot <- ggplot(wk_gam_access, aes(x = Week))
wk_gam_TV_plot <- wk_gam_TV_plot + geom_line(aes(y = wk_gam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_gam_TV_plot <- wk_gam_TV_plot + geom_line(aes(y = wk_gam_access$TV, colour = "TV Investment",group=1),size=1)

wk_gam_TV_plot <- wk_gam_TV_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "TV Investment"))

# modifying colours and theme options
wk_gam_TV_plot <- wk_gam_TV_plot + scale_colour_manual(values = c("orange", "green"))
wk_gam_TV_plot <- wk_gam_TV_plot + labs(y = "Weekly Sales",
                                        x = "Week",
                                        colour = "Legend",title="Gaming Accessories")
wk_gam_TV_plot <- wk_gam_TV_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_gam_TV_plot <- wk_gam_TV_plot + geom_vline(xintercept = wk_gam_access[which(wk_gam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_gam_TV_plot



################### Digital Investment: lag of 2 weeks

wk_gam_Digital_plot <- ggplot(wk_gam_access, aes(x = Week))
wk_gam_Digital_plot <- wk_gam_Digital_plot + geom_line(aes(y = wk_gam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_gam_Digital_plot <- wk_gam_Digital_plot + geom_line(aes(y = wk_gam_access$Digital, colour = "Digital Investment",group=1),size=1)

wk_gam_Digital_plot <- wk_gam_Digital_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Digital Investment"))

# modifying colours and theme options
wk_gam_Digital_plot <- wk_gam_Digital_plot + scale_colour_manual(values = c("orange", "green"))
wk_gam_Digital_plot <- wk_gam_Digital_plot + labs(y = "Weekly Sales",
                                                  x = "Week",
                                                  colour = "Legend",title="Camera Accessories")
wk_gam_Digital_plot <- wk_gam_Digital_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_gam_Digital_plot <- wk_gam_Digital_plot + geom_vline(xintercept = wk_gam_access[which(wk_gam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_gam_Digital_plot


################### Sponsorship Investment: less lag impact

wk_gam_Sponsorship_plot <- ggplot(wk_gam_access, aes(x = Week))
wk_gam_Sponsorship_plot <- wk_gam_Sponsorship_plot + geom_line(aes(y = wk_gam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_gam_Sponsorship_plot <- wk_gam_Sponsorship_plot + geom_line(aes(y = wk_gam_access$Sponsorship/10, colour = "Sponsorship Investment",group=1),size=1)

wk_gam_Sponsorship_plot <- wk_gam_Sponsorship_plot + scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Sponsorship Investment"))

# modifying colours and theme options
wk_gam_Sponsorship_plot <- wk_gam_Sponsorship_plot + scale_colour_manual(values = c("orange", "green"))
wk_gam_Sponsorship_plot <- wk_gam_Sponsorship_plot + labs(y = "Weekly Sales",
                                                          x = "Week",
                                                          colour = "Legend",title="Camera Accessories")
wk_gam_Sponsorship_plot <- wk_gam_Sponsorship_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_gam_Sponsorship_plot <- wk_gam_Sponsorship_plot + geom_vline(xintercept = wk_gam_access[which(wk_gam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_gam_Sponsorship_plot


################### Content Marketing Investment : less lag impact

wk_gam_ContMarketing_plot <- ggplot(wk_gam_access, aes(x = Week))
wk_gam_ContMarketing_plot <- wk_gam_ContMarketing_plot + geom_line(aes(y = wk_gam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_gam_ContMarketing_plot <- wk_gam_ContMarketing_plot + geom_line(aes(y = wk_gam_access$ContentMarketing, colour = "Content Marketing Investment",group=1),size=1)

wk_gam_ContMarketing_plot <- wk_gam_ContMarketing_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Content Marketing Investment"))

# modifying colours and theme options
wk_gam_ContMarketing_plot <- wk_gam_ContMarketing_plot + scale_colour_manual(values = c("orange", "green"))
wk_gam_ContMarketing_plot <- wk_gam_ContMarketing_plot + labs(y = "Weekly Sales",
                                                              x = "Week",
                                                              colour = "Legend",title="Camera Accessories")
wk_gam_ContMarketing_plot <- wk_gam_ContMarketing_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_gam_ContMarketing_plot <- wk_gam_ContMarketing_plot + geom_vline(xintercept = wk_gam_access[which(wk_gam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_gam_ContMarketing_plot


################### Online Investment :2 weeks lag impact

wk_gam_Online_plot <- ggplot(wk_gam_access, aes(x = Week))
wk_gam_Online_plot <- wk_gam_Online_plot + geom_line(aes(y = wk_gam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_gam_Online_plot <- wk_gam_Online_plot + geom_line(aes(y = wk_gam_access$Online/10, colour = "Online Investment",group=1),size=1)

wk_gam_Online_plot <- wk_gam_Online_plot + scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Online Investment"))

# modifying colours and theme options
wk_gam_Online_plot <- wk_gam_Online_plot + scale_colour_manual(values = c("orange", "green"))
wk_gam_Online_plot <- wk_gam_Online_plot + labs(y = "Weekly Sales",
                                                x = "Week",
                                                colour = "Legend",title="Camera Accessories")
wk_gam_Online_plot <- wk_gam_Online_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_gam_Online_plot <- wk_gam_Online_plot + geom_vline(xintercept = wk_gam_access[which(wk_gam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_gam_Online_plot


################### SEM Investment : less lag impact

wk_gam_SEM_plot <- ggplot(wk_gam_access, aes(x = Week))
wk_gam_SEM_plot <- wk_gam_SEM_plot + geom_line(aes(y = wk_gam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_gam_SEM_plot <- wk_gam_SEM_plot + geom_line(aes(y = wk_gam_access$SEM, colour = "SEM Investment",group=1),size=1)

wk_gam_SEM_plot <- wk_gam_SEM_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "SEM Investment"))

# modifying colours and theme options
wk_gam_SEM_plot <- wk_gam_SEM_plot + scale_colour_manual(values = c("orange", "green"))
wk_gam_SEM_plot <- wk_gam_SEM_plot + labs(y = "Weekly Sales",
                                          x = "Week",
                                          colour = "Legend",title="Camera Accessories")
wk_gam_SEM_plot <- wk_gam_SEM_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_gam_SEM_plot <- wk_gam_SEM_plot + geom_vline(xintercept = wk_gam_access[which(wk_gam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_gam_SEM_plot


################### Affliates Investment: less lag impact

wk_gam_Affliates_plot <- ggplot(wk_gam_access, aes(x = Week))
wk_gam_Affliates_plot <- wk_gam_Affliates_plot + geom_line(aes(y = wk_gam_access$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_gam_Affliates_plot <- wk_gam_Affliates_plot + geom_line(aes(y = wk_gam_access$Affliates, colour = "Affliates Investment",group=1),size=1)

wk_gam_Affliates_plot <- wk_gam_Affliates_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Affliates Investment"))

# modifying colours and theme options
wk_gam_Affliates_plot <- wk_gam_Affliates_plot + scale_colour_manual(values = c("orange", "green"))
wk_gam_Affliates_plot <- wk_gam_Affliates_plot + labs(y = "Weekly Sales",
                                                      x = "Week",
                                                      colour = "Legend",title="Camera Accessories")
wk_gam_Affliates_plot <- wk_gam_Affliates_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_gam_Affliates_plot <- wk_gam_Affliates_plot + geom_vline(xintercept = wk_gam_access[which(wk_gam_access$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_gam_Affliates_plot

###############################################


wk_gam_access$adImpact_TV <- stats::filter(wk_gam_access$TV,c(.001),method="recursive")
wk_gam_access$adImpact_Digital <- stats::filter(wk_gam_access$Digital,c(.15,.15),method="recursive")
wk_gam_access$adImpact_Sponsorship <- stats::filter(wk_gam_access$Sponsorship,c(.001),method="recursive")
wk_gam_access$adImpact_ContentMarketing<- stats::filter(wk_gam_access$ContentMarketing,c(.001),method="recursive")
wk_gam_access$adImpact_Online <- stats::filter(wk_gam_access$Online,c(.15,.15),method="recursive")
wk_gam_access$adImpact_SEM <- stats::filter(wk_gam_access$SEM,c(.001),method="recursive")
wk_gam_access$adImpact_Affliates <- stats::filter(wk_gam_access$Affliates,c(.001),method="recursive")


# Sales (GMV) will also have a lag effect 
#wk_gam_access$impactedSales <- stats::filter(wk_gam_access$Sales,c(.15,.10,.05),method="recursive")
#wk_gam_access$salesLag <- wk_gam_access$impactedSales - wk_gam_access$Sales

#wk_gam_access$salesLag <- 0
wk_gam_access$salesLag <- lag(wk_gam_access$Sales,1)
# put NA equals zero in first row for sales LAG 
wk_gam_access$salesLag[1] <- 0

# calculate media lags
wk_gam_access$TVLag <- wk_gam_access$adImpact_TV -wk_gam_access$TV
wk_gam_access$DigitalLag <- wk_gam_access$adImpact_Digital - wk_gam_access$Digital
wk_gam_access$SponsorshipLag <- wk_gam_access$adImpact_Sponsorship - wk_gam_access$Sponsorship
wk_gam_access$ContentMarketingLag <- wk_gam_access$adImpact_ContentMarketing -wk_gam_access$ContentMarketing
wk_gam_access$OnlineLag <- wk_gam_access$adImpact_Online -wk_gam_access$Online
wk_gam_access$SEMLag <- wk_gam_access$adImpact_SEM -wk_gam_access$SEM
wk_gam_access$AffliatesLag <- wk_gam_access$adImpact_Affliates -wk_gam_access$Affliates


# scale raw data
wk_gam_access$sc_TV <- scale(wk_gam_access$TV, center = FALSE, scale = TRUE)
wk_gam_access$sc_Digital <- scale(wk_gam_access$Digital, center = FALSE, scale = TRUE)
wk_gam_access$sc_Sponsorship <- scale(wk_gam_access$Sponsorship, center = FALSE, scale = TRUE)
wk_gam_access$sc_ContentMarketing <- scale(wk_gam_access$ContentMarketing, center = FALSE, scale = TRUE)
wk_gam_access$sc_Online <- scale(wk_gam_access$Online, center = FALSE, scale = TRUE)
wk_gam_access$sc_SEM <- scale(wk_gam_access$SEM, center = FALSE, scale = TRUE)
wk_gam_access$sc_Affliates <- scale(wk_gam_access$Affliates, center = FALSE, scale = TRUE)

wk_gam_access$salesLag <- scale(wk_gam_access$salesLag, center = FALSE, scale = TRUE)
wk_gam_access$TVLag <- scale(wk_gam_access$TVLag, center = FALSE, scale = TRUE)
wk_gam_access$DigitalLag <- scale(wk_gam_access$DigitalLag, center = FALSE, scale = TRUE)
wk_gam_access$SponsorshipLag <- scale(wk_gam_access$SponsorshipLag, center = FALSE, scale = TRUE)
wk_gam_access$ContentMarketingLag <- scale(wk_gam_access$ContentMarketingLag, center = FALSE, scale = TRUE)
wk_gam_access$OnlineLag <- scale(wk_gam_access$OnlineLag, center = FALSE, scale = TRUE)
wk_gam_access$SEMLag <- scale(wk_gam_access$SEMLag, center = FALSE, scale = TRUE)
wk_gam_access$AffliatesLag <- scale(wk_gam_access$AffliatesLag, center = FALSE, scale = TRUE)


#wk_gam_access$salesLag <- wk_gam_access$salesLag - (.4*(wk_gam_access^2) + .3*wk_gam_access$salesLag)

dfMod_gam_acces <- wk_gam_access


###############################################

#################### Generate Linear Regression model


# # Divide into training and test data set
#set the seed to 100. 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(dfMod_gam_acces), 0.7*nrow(dfMod_gam_acces))
# generate the train data set
train = dfMod_gam_acces[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = dfMod_gam_acces[-trainindices,]

# # Divide into training and test data set


#################### Basic Linear Regression model with no lag

b_train = train[,c(2:8,32:38)]
basic_gam_access_model1 <- lm(Sales ~ ., data=b_train)

# Multiple R-squared:  0.6677,	Adjusted R-squared:  0.4798
summary(basic_gam_access_model1) 
vif(basic_gam_access_model1)

step <- stepAIC(basic_gam_access_model1, direction="both")


basic_gam_access_model2 <- lm(Sales ~ NPS + sc_TV + sc_Sponsorship + sc_SEM + sc_Affliates , data=b_train)

# Multiple R-squared:  0.6365,	Adjusted R-squared:  0.5779
summary(basic_gam_access_model2) 
vif(basic_gam_access_model2)


basic_gam_access_model3 <- lm(Sales ~ NPS +  sc_Sponsorship + sc_SEM + sc_Affliates , data=b_train)

# Multiple R-squared:  0.609,	Adjusted R-squared:  0.5601
summary(basic_gam_access_model3) 
vif(basic_gam_access_model3)


basic_gam_access_model4 <- lm(Sales ~   sc_Sponsorship + sc_SEM + sc_Affliates , data=b_train)

# Multiple R-squared:  0.5733,	Adjusted R-squared:  0.5345
summary(basic_gam_access_model4) 
vif(basic_gam_access_model4)


# plot Residuals vs Fitted
op <- par(mfrow=c(2,2))
plot(basic_gam_access_model4)
par(op)


# Predict the house prices in the testing dataset
Predict_1 <- predict(basic_gam_access_model4,test[,-1])
test$bas_predicted_sale <- Predict_1
# Predict_1 <- predict(basic_cam_access_model2,test[,-1])
# test$bas_predicted_sale <- Predict_1


# calculate r-squared:0.4224277
r <- cor(test$Sales,test$bas_predicted_sale)
rsquared <- cor(test$Sales,test$bas_predicted_sale)^2
rsquared

# calculate error on test data
test$error <- test$bas_predicted_sale - test$Sales

# calculate RMSE on test data: 1509094
computeRMSE(test$error)

# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(Week, Sales)) + geom_line(color = "blue",group=1 ) + geom_line(aes(Week,bas_predicted_sale), color="red", group=1) +labs(title = "Basic Linear Model")


################### Multiplicative Model

ggplot(dfMod_gam_acces,aes(Discount,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(NPS,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(TVLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(salesLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(DigitalLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(SponsorshipLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(ContentMarketingLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(OnlineLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(SEMLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(AffliatesLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(delbdays,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(delcdays,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(TV,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(Digital,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(Sponsorship,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(ContentMarketing,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(Online,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(SEM,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_gam_acces,aes(Affliates,Sales)) + geom_point() + geom_smooth()


ln_train <- train[,c(2:15)]
mul_gam_access_model1 <- lm(log(Sales) ~ PaymentType + PromotionWeek + Discount + delbdays + delcdays +NPS + log(TV) + log(Digital) 
                            + log(Sponsorship) + log(ContentMarketing) + log(Online) + log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.9537,	Adjusted R-squared:  0.9275
summary(mul_gam_access_model1) 
vif(mul_gam_access_model1)

#Execute the first model_1 with stepAIC in the training set. 
step <- stepAIC(mul_gam_access_model1, direction="both")


mul_gam_access_model2 <- lm(log(Sales) ~ delbdays + delcdays + NPS + log(TV) + log(Digital) + 
                              log(Sponsorship) + log(ContentMarketing) + log(Online) + 
                              log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.9476,	Adjusted R-squared:  0.9275
summary(mul_gam_access_model2) 
vif(mul_gam_access_model2)


mul_gam_access_model3 <- lm(log(Sales) ~ delbdays +  NPS + log(TV) + log(Digital) + 
                              log(Sponsorship) + log(ContentMarketing) + log(Online) + 
                              log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.9333,	Adjusted R-squared:  0.9111
summary(mul_gam_access_model3) 
vif(mul_gam_access_model3)


mul_gam_access_model4 <- lm(log(Sales) ~ delbdays +  NPS + log(TV)  + 
                              log(Sponsorship) + log(ContentMarketing) + log(Online) + 
                              log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.928,	Adjusted R-squared:  0.9074
summary(mul_gam_access_model4) 
vif(mul_gam_access_model4)


mul_gam_access_model5 <- lm(log(Sales) ~ delbdays +  NPS + log(TV)  +  log(ContentMarketing) + log(Online) + 
                              log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.9175,	Adjusted R-squared:  0.8976
summary(mul_gam_access_model5) 
vif(mul_gam_access_model5)


mul_gam_access_model6 <- lm(log(Sales) ~ delbdays +  NPS + log(TV)  +  log(ContentMarketing) + 
                              log(SEM) + log(Affliates) ,na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.8009,	Adjusted R-squared:  0.7611
summary(mul_gam_access_model6) 
vif(mul_gam_access_model6)


mul_gam_access_model7 <- lm(log(Sales) ~    NPS + log(TV)  +  log(ContentMarketing) + 
                              log(SEM) + log(Affliates) ,na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.7864,	Adjusted R-squared:  0.752
summary(mul_gam_access_model7) 
vif(mul_gam_access_model7)


mul_gam_access_model8 <- lm(log(Sales) ~    NPS +   log(ContentMarketing) + 
                              log(SEM) + log(Affliates) ,na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.7453,	Adjusted R-squared:  0.7135
summary(mul_gam_access_model8) 
vif(mul_gam_access_model8)


mul_gam_access_model9 <- lm(log(Sales) ~    NPS + log(ContentMarketing)  ,na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.6425,	Adjusted R-squared:  .6215
summary(mul_gam_access_model9) 
vif(mul_gam_access_model9)



mul_gam_access_model10 <- lm(log(Sales) ~    log(ContentMarketing)  ,na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.6417,	Adjusted R-squared:  .6315
summary(mul_gam_access_model10) 
vif(mul_gam_access_model10)


# plot Residuals vs Fitted
op<-par(mfrow=c(2,2))
plot(mul_gam_access_model10)
par(op)

# Predict the sales in the testing dataset
Predict_1 <- predict(mul_gam_access_model10,test[,-1])
test$log_predicted_sale <- Predict_1


# Calculate RMSE on train
ln_RSS_train <- c(crossprod(mul_gam_access_model10$residuals))
ln_RSS_train

ln_MSE_train <- ln_RSS_train / length(mul_gam_access_model10$residuals)
ln_MSE_train

ln_RMSE_train <- sqrt(ln_MSE_train)
ln_RMSE_train


# Predict the sales in the testing dataset
test$ln_predicted_sale <- exp(test$log_predicted_sale)

# calculate error on test data
test$error <- test$ln_predicted_sale - test$Sales

# calculate RMSE on test data: 1055844
computeRMSE(test$error)

ggplot(test, aes(Week, Sales)) + geom_line(color = "blue", group =1 ) + geom_line(aes(Week,ln_predicted_sale), color="red", group = 1) 


####################  Koyck Model

k_train=train[,c(2:8,24,32:38)]
ky_gam_access_model1 <- lm(Sales ~.,data=k_train)

# Multiple R-squared:  0.6736,	Adjusted R-squared:  0.4659
summary(ky_gam_access_model1) 
vif(ky_gam_access_model1)

#Execute the first model_1 with stepAIC in the training set. 
step <- stepAIC(ky_gam_access_model1, direction="both")


ky_gam_access_model2 <- lm(Sales ~   NPS + sc_TV + sc_Sponsorship + sc_SEM + sc_Affliates, data=k_train)

# Multiple R-squared:  0.6365,	Adjusted R-squared:  0.5779
summary(ky_gam_access_model2) 
vif(ky_gam_access_model2)


ky_gam_access_model3 <- lm(Sales ~   NPS +  sc_Sponsorship + sc_SEM + sc_Affliates, data=k_train)

# Multiple R-squared:  0.7054,	Adjusted R-squared:  0.6445
summary(ky_gam_access_model3) 
vif(ky_gam_access_model3)


ky_gam_access_model4 <- lm(Sales ~    sc_Sponsorship + sc_SEM + sc_Affliates, data=k_train)

# Multiple R-squared:  0.5733,	Adjusted R-squared:  0.5345
summary(ky_gam_access_model4) 
vif(ky_gam_access_model4)


ky_gam_access_model5 <- lm(Sales ~    sc_Sponsorship + sc_Affliates, data=k_train)

# Multiple R-squared:  0.3443,	Adjusted R-squared:  0.3058
summary(ky_gam_access_model5) 
vif(ky_gam_access_model5)


ky_gam_access_model6 <- lm(Sales ~    sc_Affliates, data=k_train)

# Multiple R-squared:  0.3238,	Adjusted R-squared:  0.3044 
summary(ky_gam_access_model6) 
vif(ky_gam_access_model6)



# plot Residuals vs Fitted
op <- par(mfrow = c(2, 2))
plot(ky_gam_access_model6)
par(op)

# Predict the house prices in the testing dataset
Predict_1 <- predict(ky_gam_access_model6,test[,-1])
test$ky_predicted_sale <- Predict_1


# Calculate RMSE: on train data
ky_RSS_train <- c(crossprod(ky_gam_access_model6$residuals))
ky_RSS_train

ky_MSE_train <- ky_RSS_train / length(ky_gam_access_model6$residuals)
ky_MSE_train

ky_RMSE_train <- sqrt(ky_MSE_train)
ky_RMSE_train


# calculate error on test data
test$error <- test$ky_predicted_sale - test$Sales

# calculate RMSE on test data: 1438992 
computeRMSE(test$error)

# calculate r-squared: 0.5401453
r <- cor(test$Sales,test$ky_predicted_sale)
rsquared <- cor(test$Sales,test$ky_predicted_sale)^2
rsquared


# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(Week, Sales)) + geom_line(color = "blue" ,group=1) + geom_line(aes(Week,ky_predicted_sale), color="red",group=1) 


################ Distribute Lag

dn_train <- train[,c(2:8,24:38)]
dis_gam_access_model1 <- lm(Sales ~ . ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.691,	Adjusted R-squared:  0.2585
summary(dis_gam_access_model1) 
vif(dis_gam_access_model1)

step <- stepAIC(dis_gam_access_model1, direction="both")


dis_gam_access_model2 <- lm(Sales ~ NPS + sc_TV + sc_Sponsorship + sc_SEM + sc_Affliates ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.6365,	Adjusted R-squared:  0.5779
summary(dis_gam_access_model2) 
vif(dis_gam_access_model2)


dis_gam_access_model3 <- lm(Sales ~ NPS +  sc_Sponsorship + sc_SEM + sc_Affliates ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.609,	Adjusted R-squared:  0.5601
summary(dis_gam_access_model3) 
vif(dis_gam_access_model3)


dis_gam_access_model4 <- lm(Sales ~ sc_Sponsorship + sc_SEM + sc_Affliates ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.5733,	Adjusted R-squared:  0.5345
summary(dis_gam_access_model4) 
vif(dis_gam_access_model4)

 
dis_gam_access_model5 <- lm(Sales ~ sc_Sponsorship +  sc_Affliates ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.3443,	Adjusted R-squared:  0.3058
summary(dis_gam_access_model5) 
vif(dis_gam_access_model5)


dis_gam_access_model6 <- lm(Sales ~ sc_Affliates ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.3238,	Adjusted R-squared:  0.3044
summary(dis_gam_access_model6) 
vif(dis_gam_access_model6)


# plot Residuals vs Fitted
op <- par(mfrow = c(2, 2))
plot(dis_gam_access_model6)
par(op)

# Predict the house prices in the testing dataset
Predict_1 <- predict(dis_gam_access_model6,test[,-1])
test$dy_predicted_sale <- Predict_1


# Calculate RMSE: 1488007 on train data
dy_RSS_train <- c(crossprod(dis_gam_access_model6$residuals))
dy_RSS_train

dy_MSE_train <- dy_RSS_train / length(dis_gam_access_model6$residuals)
dy_MSE_train

dy_RMSE_train <- sqrt(dy_MSE_train)
dy_RMSE_train


# calculate error on test data
test$error <- test$dy_predicted_sale - test$Sales

# calculate RMSE on test data: 1438992
computeRMSE(test$error)

# calculate r-squared: 0.5401453
r <- cor(test$Sales,test$dy_predicted_sale)
rsquared <- cor(test$Sales,test$dy_predicted_sale)^2
rsquared


# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(Week, Sales)) + geom_line(color = "blue" ,group=1) + geom_line(aes(Week,dy_predicted_sale), color="red",group=1) 


##################################### Mix model & Interaction variable 

mx_train <- train[,c(2,24:38)]

# find correlation between Sales and other variables
cor(dfMod_gam_acces[,-1])


# find interaction between variables: TVLag and ContentMarketingLag have interaction
interaction_fit1 <- lm(log(Sales) ~ ( TVLag+AffliatesLag+SEMLag+ContentMarketingLag+SponsorshipLag+DigitalLag+OnlineLag)^2,data=train[,c(2,25:31)])
anova(interaction_fit1)

# sc_Affliates and sc_ContentMarketing; sc_Affliates and sc_Sponsorship
interaction_fit2 <- lm(log(Sales) ~ (sc_Affliates+sc_SEM+sc_Online+sc_ContentMarketing+sc_Sponsorship+sc_Digital+sc_TV)^2,data=train[,c(2,32:38)])
anova(interaction_fit2)

# TVLag and ContentMarketingLag have interaction
# RMSE on test: 2513942
mx_gam_access_model1 <- lm(log(Sales) ~ TVLag*ContentMarketingLag,na.action = na.exclude, data=train[,c(2,25:31)])
# Multiple R-squared:  0.4042,	Adjusted R-squared:  0.3501
summary(mx_gam_access_model1) 
vif(mx_gam_access_model1)


mx_gam_access_model2 <- lm(log(Sales) ~ sc_Affliates*sc_Sponsorship,na.action = na.exclude, data=train[,c(2,25:38)])
# Multiple R-squared:  0.6166 ,	Adjusted R-squared:  0.5818
# RNSE: 5860810
summary(mx_gam_access_model2) 
vif(mx_gam_access_model2)

# mx_cam_access_model1 is the most stable interaction variable model
# plot Residuals vs Fitted
op <- par(mfrow = c(2, 2))
plot(mx_gam_access_model2)
par(op)


# Predict the sales in the testing dataset
Predict_1 <- predict(mx_gam_access_model2,test[,-1])
test$lmx_predicted_sale <- Predict_1


# Calculate RMSE: 1.113412 on train data (log scale)
mx_RSS_train <- c(crossprod(mx_gam_access_model2$residuals))
mx_RSS_train

mx_MSE_train <- mx_RSS_train / length(mx_gam_access_model2$residuals)
mx_MSE_train

mx_RMSE_train <- sqrt(mx_MSE_train)
mx_RMSE_train


# Predict the sales in the testing dataset
test$mx_predicted_sale <- exp(test$lmx_predicted_sale)

# calculate error on test data
test$error <- test$mx_predicted_sale - test$Sales

# calculate RMSE on test data
computeRMSE(test$error)

ggplot(test, aes(Week, Sales)) + geom_line(color = "blue", group =1 ) + geom_line(aes(Week,mx_predicted_sale), color="red", group = 1) 



####################################################

################################### Model - Home Audio ##############################

wk_hom_Audio$Week <- as.numeric(wk_hom_Audio$Week)

wk_hom_inves_plot <- ggplot(wk_hom_Audio, aes(x = Week))
wk_hom_inves_plot <- wk_hom_inves_plot + geom_line(aes(y = wk_hom_Audio$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_hom_inves_plot <- wk_hom_inves_plot + geom_line(aes(y = wk_hom_Audio$Investment/10, colour = "Adv Investment",group=1),size=1)

wk_hom_inves_plot <- wk_hom_inves_plot + scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Adv Investment"))

# modifying colours and theme options
wk_hom_inves_plot <- wk_hom_inves_plot + scale_colour_manual(values = c("orange", "green"))
wk_hom_inves_plot <- wk_hom_inves_plot + labs(y = "Weekly Sales",
                                              x = "Week",
                                              colour = "Legend",title="Gaming Accessories")
wk_hom_inves_plot <- wk_hom_inves_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_hom_inves_plot <- wk_hom_inves_plot + geom_vline(xintercept = wk_hom_Audio[which(wk_hom_Audio$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_hom_inves_plot

############# TV Lag: less likely impact

wk_hom_TV_plot <- ggplot(wk_hom_Audio, aes(x = Week))
wk_hom_TV_plot <- wk_hom_TV_plot + geom_line(aes(y = wk_hom_Audio$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_hom_TV_plot <- wk_hom_TV_plot + geom_line(aes(y = wk_hom_Audio$TV, colour = "TV Investment",group=1),size=1)

wk_hom_TV_plot <- wk_hom_TV_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "TV Investment"))

# modifying colours and theme options
wk_hom_TV_plot <- wk_hom_TV_plot + scale_colour_manual(values = c("orange", "green"))
wk_hom_TV_plot <- wk_hom_TV_plot + labs(y = "Weekly Sales",
                                        x = "Week",
                                        colour = "Legend",title="Gaming Accessories")
wk_hom_TV_plot <- wk_hom_TV_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_hom_TV_plot <- wk_hom_TV_plot + geom_vline(xintercept = wk_hom_Audio[which(wk_hom_Audio$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_hom_TV_plot



################### Digital Investment

wk_hom_Digital_plot <- ggplot(wk_hom_Audio, aes(x = Week))
wk_hom_Digital_plot <- wk_hom_Digital_plot + geom_line(aes(y = wk_hom_Audio$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_hom_Digital_plot <- wk_hom_Digital_plot + geom_line(aes(y = wk_hom_Audio$Digital, colour = "Digital Investment",group=1),size=1)

wk_hom_Digital_plot <- wk_hom_Digital_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Digital Investment"))

# modifying colours and theme options
wk_hom_Digital_plot <- wk_hom_Digital_plot + scale_colour_manual(values = c("orange", "green"))
wk_hom_Digital_plot <- wk_hom_Digital_plot + labs(y = "Weekly Sales",
                                                  x = "Week",
                                                  colour = "Legend",title="Camera Accessories")
wk_hom_Digital_plot <- wk_hom_Digital_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_hom_Digital_plot <- wk_hom_Digital_plot + geom_vline(xintercept = wk_hom_Audio[which(wk_hom_Audio$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_hom_Digital_plot


################### Sponsorship Investment: less lag impact

wk_hom_Sponsorship_plot <- ggplot(wk_hom_Audio, aes(x = Week))
wk_hom_Sponsorship_plot <- wk_hom_Sponsorship_plot + geom_line(aes(y = wk_hom_Audio$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_hom_Sponsorship_plot <- wk_hom_Sponsorship_plot + geom_line(aes(y = wk_hom_Audio$Sponsorship/10, colour = "Sponsorship Investment",group=1),size=1)

wk_hom_Sponsorship_plot <- wk_hom_Sponsorship_plot + scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Sponsorship Investment"))

# modifying colours and theme options
wk_hom_Sponsorship_plot <- wk_hom_Sponsorship_plot + scale_colour_manual(values = c("orange", "green"))
wk_hom_Sponsorship_plot <- wk_hom_Sponsorship_plot + labs(y = "Weekly Sales",
                                                          x = "Week",
                                                          colour = "Legend",title="Camera Accessories")
wk_hom_Sponsorship_plot <- wk_hom_Sponsorship_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_hom_Sponsorship_plot <- wk_hom_Sponsorship_plot + geom_vline(xintercept = wk_hom_Audio[which(wk_hom_Audio$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_hom_Sponsorship_plot


################### Content Marketing Investment : less lag impact

wk_hom_ContMarketing_plot <- ggplot(wk_hom_Audio, aes(x = Week))
wk_hom_ContMarketing_plot <- wk_hom_ContMarketing_plot + geom_line(aes(y = wk_hom_Audio$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_hom_ContMarketing_plot <- wk_hom_ContMarketing_plot + geom_line(aes(y = wk_hom_Audio$ContentMarketing, colour = "Content Marketing Investment",group=1),size=1)

wk_hom_ContMarketing_plot <- wk_hom_ContMarketing_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Content Marketing Investment"))

# modifying colours and theme options
wk_hom_ContMarketing_plot <- wk_hom_ContMarketing_plot + scale_colour_manual(values = c("orange", "green"))
wk_hom_ContMarketing_plot <- wk_hom_ContMarketing_plot + labs(y = "Weekly Sales",
                                                              x = "Week",
                                                              colour = "Legend",title="Camera Accessories")
wk_hom_ContMarketing_plot <- wk_hom_ContMarketing_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_hom_ContMarketing_plot <- wk_hom_ContMarketing_plot + geom_vline(xintercept = wk_hom_Audio[which(wk_hom_Audio$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_hom_ContMarketing_plot


################### Online Investment :less lag impact

wk_hom_Online_plot <- ggplot(wk_hom_Audio, aes(x = Week))
wk_hom_Online_plot <- wk_hom_Online_plot + geom_line(aes(y = wk_hom_Audio$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_hom_Online_plot <- wk_hom_Online_plot + geom_line(aes(y = wk_hom_Audio$Online/10, colour = "Online Investment",group=1),size=1)

wk_hom_Online_plot <- wk_hom_Online_plot + scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Online Investment"))

# modifying colours and theme options
wk_hom_Online_plot <- wk_hom_Online_plot + scale_colour_manual(values = c("orange", "green"))
wk_hom_Online_plot <- wk_hom_Online_plot + labs(y = "Weekly Sales",
                                                x = "Week",
                                                colour = "Legend",title="Camera Accessories")
wk_hom_Online_plot <- wk_hom_Online_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_hom_Online_plot <- wk_hom_Online_plot + geom_vline(xintercept = wk_hom_Audio[which(wk_hom_Audio$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_hom_Online_plot


################### SEM Investment : 1 week lag impact

wk_hom_SEM_plot <- ggplot(wk_hom_Audio, aes(x = Week))
wk_hom_SEM_plot <- wk_hom_SEM_plot + geom_line(aes(y = wk_hom_Audio$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_hom_SEM_plot <- wk_hom_SEM_plot + geom_line(aes(y = wk_hom_Audio$SEM, colour = "SEM Investment",group=1),size=1)

wk_hom_SEM_plot <- wk_hom_SEM_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "SEM Investment"))

# modifying colours and theme options
wk_hom_SEM_plot <- wk_hom_SEM_plot + scale_colour_manual(values = c("orange", "green"))
wk_hom_SEM_plot <- wk_hom_SEM_plot + labs(y = "Weekly Sales",
                                          x = "Week",
                                          colour = "Legend",title="Camera Accessories")
wk_hom_SEM_plot <- wk_hom_SEM_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_hom_SEM_plot <- wk_hom_SEM_plot + geom_vline(xintercept = wk_hom_Audio[which(wk_hom_Audio$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_hom_SEM_plot


################### Affliates Investment: 2 weeks lag impact

wk_hom_Affliates_plot <- ggplot(wk_hom_Audio, aes(x = Week))
wk_hom_Affliates_plot <- wk_hom_Affliates_plot + geom_line(aes(y = wk_hom_Audio$Sales, colour = "Weekly Sales",group=1),size=1)

# adding the Total Media Investment data and approximation for scale
wk_hom_Affliates_plot <- wk_hom_Affliates_plot + geom_line(aes(y = wk_hom_Audio$Affliates, colour = "Affliates Investment",group=1),size=1)

wk_hom_Affliates_plot <- wk_hom_Affliates_plot + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Affliates Investment"))

# modifying colours and theme options
wk_hom_Affliates_plot <- wk_hom_Affliates_plot + scale_colour_manual(values = c("orange", "green"))
wk_hom_Affliates_plot <- wk_hom_Affliates_plot + labs(y = "Weekly Sales",
                                                      x = "Week",
                                                      colour = "Legend",title="Camera Accessories")
wk_hom_Affliates_plot <- wk_hom_Affliates_plot + theme(legend.position = c(0.5, 0.8)) + theme_gray(base_size = 14)
wk_hom_Affliates_plot <- wk_hom_Affliates_plot + geom_vline(xintercept = wk_hom_Audio[which(wk_hom_Audio$PromotionWeek == 2),]$Week,col="blue",linetype = "3313")
wk_hom_Affliates_plot

###############################################


wk_hom_Audio$adImpact_TV <- stats::filter(wk_hom_Audio$TV,c(.001),method="recursive")
wk_hom_Audio$adImpact_Digital <- stats::filter(wk_hom_Audio$Digital,c(.001),method="recursive")
wk_hom_Audio$adImpact_Sponsorship <- stats::filter(wk_hom_Audio$Sponsorship,c(.001),method="recursive")
wk_hom_Audio$adImpact_ContentMarketing<- stats::filter(wk_hom_Audio$ContentMarketing,c(.001),method="recursive")
wk_hom_Audio$adImpact_Online <- stats::filter(wk_hom_Audio$Online,c(.001),method="recursive")
wk_hom_Audio$adImpact_SEM <- stats::filter(wk_hom_Audio$SEM,c(.15),method="recursive")
wk_hom_Audio$adImpact_Affliates <- stats::filter(wk_hom_Audio$Affliates,c(.20,.15),method="recursive")


# Sales (GMV) will also have a lag effect 
#wk_hom_Audio$impactedSales <- stats::filter(wk_hom_Audio$Sales,c(.15,.10,.05),method="recursive")
#wk_hom_Audio$salesLag <- wk_hom_Audio$impactedSales - wk_hom_Audio$Sales

#wk_hom_Audio$salesLag <- 0
wk_hom_Audio$salesLag <- lag(wk_hom_Audio$Sales,1)
# put NA equals 1 in first row for sales LAG 
wk_hom_Audio$salesLag[1] <- 1

# calculate media lags
wk_hom_Audio$TVLag <- wk_hom_Audio$adImpact_TV -wk_hom_Audio$TV
wk_hom_Audio$DigitalLag <- wk_hom_Audio$adImpact_Digital - wk_hom_Audio$Digital
wk_hom_Audio$SponsorshipLag <- wk_hom_Audio$adImpact_Sponsorship - wk_hom_Audio$Sponsorship
wk_hom_Audio$ContentMarketingLag <- wk_hom_Audio$adImpact_ContentMarketing -wk_hom_Audio$ContentMarketing
wk_hom_Audio$OnlineLag <- wk_hom_Audio$adImpact_Online -wk_hom_Audio$Online
wk_hom_Audio$SEMLag <- wk_hom_Audio$adImpact_SEM -wk_hom_Audio$SEM
wk_hom_Audio$AffliatesLag <- wk_hom_Audio$adImpact_Affliates -wk_hom_Audio$Affliates


# scale raw data
wk_hom_Audio$sc_TV <- scale(wk_hom_Audio$TV, center = FALSE, scale = TRUE)
wk_hom_Audio$sc_Digital <- scale(wk_hom_Audio$Digital, center = FALSE, scale = TRUE)
wk_hom_Audio$sc_Sponsorship <- scale(wk_hom_Audio$Sponsorship, center = FALSE, scale = TRUE)
wk_hom_Audio$sc_ContentMarketing <- scale(wk_hom_Audio$ContentMarketing, center = FALSE, scale = TRUE)
wk_hom_Audio$sc_Online <- scale(wk_hom_Audio$Online, center = FALSE, scale = TRUE)
wk_hom_Audio$sc_SEM <- scale(wk_hom_Audio$SEM, center = FALSE, scale = TRUE)
wk_hom_Audio$sc_Affliates <- scale(wk_hom_Audio$Affliates, center = FALSE, scale = TRUE)

wk_hom_Audio$sc_salesLag <- scale(wk_hom_Audio$salesLag, center = FALSE, scale = TRUE)
wk_hom_Audio$sc_TVLag <- scale(wk_hom_Audio$TVLag, center = FALSE, scale = TRUE)
wk_hom_Audio$sc_DigitalLag <- scale(wk_hom_Audio$DigitalLag, center = FALSE, scale = TRUE)
wk_hom_Audio$sc_SponsorshipLag <- scale(wk_hom_Audio$SponsorshipLag, center = FALSE, scale = TRUE)
wk_hom_Audio$sc_ContentMarketingLag <- scale(wk_hom_Audio$ContentMarketingLag, center = FALSE, scale = TRUE)
wk_hom_Audio$sc_OnlineLag <- scale(wk_hom_Audio$OnlineLag, center = FALSE, scale = TRUE)
wk_hom_Audio$sc_SEMLag <- scale(wk_hom_Audio$SEMLag, center = FALSE, scale = TRUE)
wk_hom_Audio$sc_AffliatesLag <- scale(wk_hom_Audio$AffliatesLag, center = FALSE, scale = TRUE)


#wk_hom_Audio$salesLag <- wk_hom_Audio$salesLag - (.4*(wk_hom_Audio^2) + .3*wk_hom_Audio$salesLag)

dfMod_hom_acces <- wk_hom_Audio[,-3]


###############################################

#################### Generate Linear Regression model


# # Divide into training and test data set
#set the seed to 100. 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(dfMod_hom_acces), 0.7*nrow(dfMod_hom_acces))
# generate the train data set
train = dfMod_hom_acces[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = dfMod_hom_acces[-trainindices,]

# # Divide into training and test data set


#################### Basic Linear Regression model with no lag


#################### Basic Linear Regression model with no lag

b_train = train[,c(2:7,32:37)]
basic_hom_audio_model1 <- lm(Sales ~ ., data=b_train)

# Multiple R-squared:  0.6714,	Adjusted R-squared:  0.5268
summary(basic_hom_audio_model1) 
vif(basic_hom_audio_model1)

step <- stepAIC(basic_hom_audio_model1, direction="both")


basic_hom_audio_model2 <- lm(Sales ~ Discount + PromotionWeek + NPS + sc_ContentMarketing + 
                               sc_Online, data=b_train)

# Multiple R-squared:  0.6398,	Adjusted R-squared:  0.5817
summary(basic_hom_audio_model2) 
vif(basic_hom_audio_model2)


basic_hom_audio_model3 <- lm(Sales ~ Discount +  NPS + sc_ContentMarketing + 
                               sc_Online, data=b_train)

# Multiple R-squared:  0.6046,	Adjusted R-squared:  0.5552
summary(basic_hom_audio_model3) 
vif(basic_hom_audio_model3)


basic_hom_audio_model4 <- lm(Sales ~ Discount +  NPS + sc_ContentMarketing , data=b_train)

# Multiple R-squared:  0.5333,	Adjusted R-squared:  0.4909
summary(basic_hom_audio_model4) 
vif(basic_hom_audio_model4)


basic_hom_audio_model5 <- lm(Sales ~ Discount +   sc_ContentMarketing , data=b_train)

# Multiple R-squared:  0.4846,	Adjusted R-squared:  0.4543
summary(basic_hom_audio_model5) 
vif(basic_hom_audio_model5)


# plot Residuals vs Fitted
op <- par(mfrow=c(2,2))
plot(basic_hom_audio_model5)
par(op)


# Predict the house prices in the testing dataset
Predict_1 <- predict(basic_hom_audio_model5,test[,-1])
test$bas_predicted_sale <- Predict_1
# Predict_1 <- predict(basic_cam_access_model2,test[,-1])
# test$bas_predicted_sale <- Predict_1


# calculate r-squared:0.09217232
r <- cor(test$Sales,test$bas_predicted_sale)
rsquared <- cor(test$Sales,test$bas_predicted_sale)^2
rsquared

# calculate error on test data
test$error <- test$bas_predicted_sale - test$Sales

# calculate RMSE on test data: 2705055
computeRMSE(test$error)

# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(Week, Sales)) + geom_line(color = "blue",group=1 ) + geom_line(aes(Week,bas_predicted_sale), color="red", group=1) +labs(title = "Basic Linear Model")


################### Multiplicative Model

ggplot(dfMod_hom_acces,aes(Discount,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(NPS,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(TVLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(salesLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(DigitalLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(SponsorshipLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(ContentMarketingLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(OnlineLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(SEMLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(AffliatesLag,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(delbdays,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(delcdays,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(TV,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(Digital,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(Sponsorship,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(ContentMarketing,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(Online,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(SEM,Sales)) + geom_point() + geom_smooth()
ggplot(dfMod_hom_acces,aes(Affliates,Sales)) + geom_point() + geom_smooth()


ln_train <- train[,c(2:14)]
mul_hom_audio_model1 <- lm(log(Sales) ~  PromotionWeek + Discount + delbdays + delcdays +NPS + log(TV) + log(Digital) 
                            + log(Sponsorship) + log(ContentMarketing) + log(Online) + log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.647,	Adjusted R-squared:  0.8931
summary(mul_hom_audio_model1) 
vif(mul_hom_audio_model1)

#Execute the first model_1 with stepAIC in the training set. 
step <- stepAIC(mul_hom_audio_model1, direction="both")


mul_hom_audio_model2 <- lm(log(Sales) ~  PromotionWeek + delbdays + delcdays + NPS + log(TV) + 
                             log(Digital) + log(Sponsorship) + log(ContentMarketing) + 
                             log(Online) + log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.9285,	Adjusted R-squared:  0.897
summary(mul_hom_audio_model2) 
vif(mul_hom_audio_model2)


mul_hom_audio_model3 <- lm(log(Sales) ~  PromotionWeek +  delcdays + NPS + log(TV) + 
                             log(Digital) + log(Sponsorship) + log(ContentMarketing) + 
                             log(Online) + log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.9243,	Adjusted R-squared:  0.8951
summary(mul_hom_audio_model3) 
vif(mul_hom_audio_model3)


mul_hom_audio_model4 <- lm(log(Sales) ~  PromotionWeek +   NPS + log(TV) + 
                             log(Digital) + log(Sponsorship) + log(ContentMarketing) + 
                             log(Online) + log(SEM) + log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.9188,	Adjusted R-squared:  0.8917
summary(mul_hom_audio_model4) 
vif(mul_hom_audio_model4)


mul_hom_audio_model5 <- lm(log(Sales) ~  PromotionWeek +   NPS + log(TV) + 
                             log(Digital) + log(Sponsorship) + log(ContentMarketing) + 
                             log(Online) +  log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.9095,	Adjusted R-squared:  0.8836
summary(mul_hom_audio_model5) 
vif(mul_hom_audio_model5)


mul_hom_audio_model6 <- lm(log(Sales) ~  PromotionWeek +   NPS + log(TV) + log(Sponsorship) + log(ContentMarketing) + 
                             log(Online) +  log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.9046	Adjusted R-squared:  0.8816
summary(mul_hom_audio_model6) 
vif(mul_hom_audio_model6)


mul_hom_audio_model7 <- lm(log(Sales) ~  PromotionWeek +   NPS + log(TV) + log(Sponsorship) + 
                             log(Online) +  log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.8934	Adjusted R-squared:  0.8721
summary(mul_hom_audio_model7) 
vif(mul_hom_audio_model7)


mul_hom_audio_model8 <- lm(log(Sales) ~  PromotionWeek +   NPS + log(TV) + log(Sponsorship) + 
                              log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.7676	Adjusted R-squared:  0.7301
summary(mul_hom_audio_model8) 
vif(mul_hom_audio_model8)


mul_hom_audio_model9 <- lm(log(Sales) ~  PromotionWeek +   NPS + log(TV) +  
                             log(Affliates),na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.7676	Adjusted R-squared:  0.7301
summary(mul_hom_audio_model9) 
vif(mul_hom_audio_model9)


mul_hom_audio_model10 <- lm(log(Sales) ~  PromotionWeek +   NPS + log(TV) ,na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.7665	Adjusted R-squared:  0.7453
summary(mul_hom_audio_model10) 
vif(mul_hom_audio_model10)


mul_hom_audio_model11 <- lm(log(Sales) ~  PromotionWeek + log(TV) ,na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.7438	Adjusted R-squared:  0.7287
summary(mul_hom_audio_model11) 
vif(mul_hom_audio_model11)


mul_hom_audio_model12 <- lm(log(Sales) ~   log(TV) ,na.action = na.exclude, data=ln_train)

# Multiple R-squared:  0.727	Adjusted R-squared:  0.7192
# RMSE on test: 1816283
summary(mul_hom_audio_model12) 
vif(mul_hom_audio_model12)


# plot Residuals vs Fitted
op<-par(mfrow=c(2,2))
plot(mul_hom_audio_model12)
par(op)

# Predict the sales in the testing dataset
Predict_1 <- predict(mul_hom_audio_model12,test[,-1])
test$log_predicted_sale <- Predict_1


# Calculate RMSE: 1.019651 on train
ln_RSS_train <- c(crossprod(mul_hom_audio_model12$residuals))
ln_RSS_train

ln_MSE_train <- ln_RSS_train / length(mul_hom_audio_model12$residuals)
ln_MSE_train

ln_RMSE_train <- sqrt(ln_MSE_train)
ln_RMSE_train


# Predict the sales in the testing dataset
test$ln_predicted_sale <- exp(test$log_predicted_sale)

# calculate error on test data
test$error <- test$ln_predicted_sale - test$Sales

# calculate RMSE on test data: 1816283
computeRMSE(test$error)

ggplot(test, aes(Week, Sales)) + geom_line(color = "blue", group =1 ) + geom_line(aes(Week,ln_predicted_sale), color="red", group = 1) 


####################  Koyck Model

k_train=train[,c(2:7,31:38)]
ky_hom_access_model1 <- lm(Sales ~.,data=k_train)

# Multiple R-squared:  0.7087,	Adjusted R-squared:  0.5441
summary(ky_hom_access_model1) 
vif(ky_hom_access_model1)

#Execute the first model_1 with stepAIC in the training set. 
step <- stepAIC(ky_hom_access_model1, direction="both")


ky_hom_access_model2 <- lm(Sales ~   Discount + PromotionWeek + NPS + sc_salesLag + sc_ContentMarketing + 
                             sc_Online, data=k_train)

# Multiple R-squared:  0.6794,	Adjusted R-squared:  0.6153
summary(ky_hom_access_model2) 
vif(ky_hom_access_model2)


ky_hom_access_model3 <- lm(Sales ~   Discount + PromotionWeek + NPS + sc_salesLag + sc_ContentMarketing , data=k_train)

# Multiple R-squared:  0.6564,	Adjusted R-squared:  0.601
summary(ky_hom_access_model3) 
vif(ky_hom_access_model3)


# plot Residuals vs Fitted
op <- par(mfrow = c(2, 2))
plot(ky_hom_access_model3)
par(op)

# Predict the house prices in the testing dataset
Predict_1 <- predict(ky_hom_access_model3,test[,-1])
test$ky_predicted_sale <- Predict_1


# Calculate RMSE: 1789549 on train data
ky_RSS_train <- c(crossprod(ky_hom_access_model3$residuals))
ky_RSS_train

ky_MSE_train <- ky_RSS_train / length(ky_hom_access_model3$residuals)
ky_MSE_train

ky_RMSE_train <- sqrt(ky_MSE_train)
ky_RMSE_train


# calculate error on test data
test$error <- test$ky_predicted_sale - test$Sales

# calculate RMSE on test data: 3274922 
computeRMSE(test$error)

# calculate r-squared: 0.001944915
r <- cor(test$Sales,test$ky_predicted_sale)
rsquared <- cor(test$Sales,test$ky_predicted_sale)^2
rsquared


# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(Week, Sales)) + geom_line(color = "blue" ,group=1) + geom_line(aes(Week,ky_predicted_sale), color="red",group=1) 


################ Distribute Lag

dn_train <- train[,c(2:7,23:45)]
dis_hom_audio_model1 <- lm(Sales ~ . ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.7513,	Adjusted R-squared:  0.4403
summary(dis_hom_audio_model1) 
vif(dis_hom_audio_model1)

step <- stepAIC(dis_hom_audio_model1, direction="both")


dis_hom_audio_model2 <- lm(Sales ~ Discount + PromotionWeek + NPS + salesLag + SEMLag + sc_ContentMarketing + sc_Online
                           ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.7013,	Adjusted R-squared:  0.6292
summary(dis_hom_audio_model2) 
vif(dis_hom_audio_model2)


dis_hom_audio_model3 <- lm(Sales ~ Discount + PromotionWeek + NPS + salesLag + sc_ContentMarketing 
                           ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.6564,	Adjusted R-squared:  0.601
# RMSE on test: 2709662
summary(dis_hom_audio_model3) 
vif(dis_hom_audio_model3)


dis_hom_audio_model4 <- lm(Sales ~  PromotionWeek + sc_ContentMarketing ,na.action = na.exclude, data=dn_train)

# Multiple R-squared:  0.5017,	Adjusted R-squared:  0.4724
# RMSE on test: 2541250
summary(dis_hom_audio_model4) 
vif(dis_hom_audio_model4)


# plot Residuals vs Fitted
op <- par(mfrow = c(2, 2))
plot(dis_hom_audio_model4)
par(op)

# Predict the house prices in the testing dataset
Predict_1 <- predict(dis_hom_audio_model4,test[,-1])
test$dy_predicted_sale <- Predict_1


# Calculate RMSE: 2155203 on train data
dy_RSS_train <- c(crossprod(dis_hom_audio_model4$residuals))
dy_RSS_train

dy_MSE_train <- dy_RSS_train / length(dis_hom_audio_model4$residuals)
dy_MSE_train

dy_RMSE_train <- sqrt(dy_MSE_train)
dy_RMSE_train


# calculate error on test data
test$error <- test$dy_predicted_sale - test$Sales

# calculate RMSE on test data: 2541250
computeRMSE(test$error)

# calculate r-squared: 0.1657872
r <- cor(test$Sales,test$dy_predicted_sale)
rsquared <- cor(test$Sales,test$dy_predicted_sale)^2
rsquared


# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(Week, Sales)) + geom_line(color = "blue" ,group=1) + geom_line(aes(Week,dy_predicted_sale), color="red",group=1) 


#####################################3 

mx_train <- train[,c(2,23:37)]

# find correlation between Sales and other variables
cor(dfMod_hom_acces[,-1])


# find interaction between variables
interaction_fit1 <- lm(log(Sales) ~ ( TVLag+AffliatesLag+SEMLag+ContentMarketingLag+SponsorshipLag+DigitalLag+OnlineLag)^2,data=train[,c(2,24:30)])
anova(interaction_fit1)

# sc_Affliates and sc_ContentMarketing
interaction_fit2 <- lm(log(Sales) ~ (sc_Affliates+sc_SEM+sc_Online+sc_ContentMarketing+sc_Sponsorship+sc_Digital+sc_TV)^2,data=train[,c(2,31:37)])
anova(interaction_fit2)


mx_hom_audio_model1 <- lm(log(Sales) ~ Affliates*Sponsorship,na.action = na.exclude, data=train[,c(2:14,23:30)])
# Multiple R-squared:  0.591 ,	Adjusted R-squared:  0.5538
# RMSE on test: 8910405
summary(mx_hom_audio_model1) 
vif(mx_hom_audio_model1)


# mx_cam_access_model1 is the most stable interaction variable model
# plot Residuals vs Fitted
op <- par(mfrow = c(2, 2))
plot(mx_hom_audio_model1)
par(op)


# Predict the sales in the testing dataset
Predict_1 <- predict(mx_hom_audio_model1,test[,-1])
test$lmx_predicted_sale <- Predict_1


# Calculate RMSE: 1.113412 on train data (log scale)
mx_RSS_train <- c(crossprod(mx_hom_audio_model1$residuals))
mx_RSS_train

mx_MSE_train <- mx_RSS_train / length(mx_hom_audio_model1$residuals)
mx_MSE_train

mx_RMSE_train <- sqrt(mx_MSE_train)
mx_RMSE_train


# Predict the sales in the testing dataset
test$mx_predicted_sale <- exp(test$lmx_predicted_sale)

# calculate error on test data
test$error <- test$mx_predicted_sale - test$Sales

# calculate RMSE on test data: 5860810
computeRMSE(test$error)

ggplot(test, aes(Week, Sales)) + geom_line(color = "blue", group =1 ) + geom_line(aes(Week,mx_predicted_sale), color="red", group = 1) 



######################## Mix Model

com_train <- train[-1,c(2:14,23:45)]
com_hom_audio_model1 <- lm(log(Sales) ~  log(ContentMarketing) ,na.action = na.exclude, data=com_train)

# Multiple R-squared:  0.7121,	Adjusted R-squared:  0.7037
# RMSE on test: 2166048
summary(com_hom_audio_model1) 
vif(com_hom_audio_model1)


com_hom_audio_model2 <- lm(log(Sales) ~  log(ContentMarketing) + Discount*NPS, na.action = na.exclude, data=com_train)

# Multiple R-squared:  0.8283,	Adjusted R-squared:  0.8061
# RMSE on test: 2365752
summary(com_hom_audio_model2) 
vif(com_hom_audio_model2)


# plot Residuals vs Fitted
op<-par(mfrow=c(2,2))
plot(com_hom_audio_model1)
par(op)

# Predict the sales in the testing dataset
Predict_1 <- predict(com_hom_audio_model2,test[,-1])
test$log_predicted_sale <- Predict_1


# Calculate RMSE: 0.9024241 on train
ln_RSS_train <- c(crossprod(com_hom_audio_model2$residuals))
ln_RSS_train

ln_MSE_train <- ln_RSS_train / length(com_hom_audio_model2$residuals)
ln_MSE_train

ln_RMSE_train <- sqrt(ln_MSE_train)
ln_RMSE_train
 

# Predict the sales in the testing dataset
test$ln_predicted_sale <- exp(test$log_predicted_sale)

# calculate error on test data
test$error <- test$ln_predicted_sale - test$Sales

# calculate RMSE on test data: 2258712
computeRMSE(test$error)

ggplot(test, aes(Week, Sales)) + geom_line(color = "blue", group =1 ) + geom_line(aes(Week,ln_predicted_sale), color="red", group = 1) 




################################################################################################ 

######################################################################### 
#
#
# 
################################### Methodology: #################################
#   
# Weeks are computed starting from January 2015.Fir eg., first week ended on 3rd January 2015.
# 
# EDA done on complete dataset for all three categories. 
#
# Weekly PaymentType data is computed by aggregating given Daily Transactional Data over weeks. 
# Most Frequent PaymentType used in all orders of that week is considered as PaymentType fpr that week. 
# Refer computeFrequency method 
# 
# Investment for each month is splitted into daily investment equally and then daily investment data is
# aggregated for each week to generate weekly investment data.
# 
# Discount is computed for each order by deducting GMV from product of no. of units sold and product mrp.
# Weekly Discount data is generated from median valye of discounts for all orders of that week.
# 
# Weekly delbdays data is generated from median valye of delbdays for all orders of that week.
# 
# Weekly delbcays data is generated from median valye of delbcays for all orders of that week.
# 
# All data is ordered by weeks. Promotion Weeks are marked.
# #
#
#
# Assumptions: 
#   
# 1. NPS for each week is same as NPS for the month of that week.
# 2. For weeks where number of COD payments is equal to number of Prepaid, COD paymenttpe is considered for that week.
#
# 
################################ EDA: ################################
#   
# NA check done for GMV, cust_id and pincode. 
# There are 4904 orders with missing information for cust_id, GMV and pincode. 
# All these orders are removed.
# 
# NA check done for product_mrp column. 
# There are 3145 orders in Camera Accessories segment with missing values for product_mrp.
# These orders were removed.
# 
# There are 217 orders in Gaming Accessories segment with missing values for product_mrp.
# These orders were removed.
# 
# There are 60 orders in Home Audio segment with missing values for product_mrp.
# These orders were removed.
# 
# 
# delbdays has some negative values for all three categories. Number of days can not be negative.
# There are 6 orders in Camera Accessories segment with values less than zero.
# These orders were removed.
# 
# There is 1 order in Gaming Accessories segment with values less than zero.
# This order was removed.
# 
# There are 3 orders in Home Audio segment with values less than zero.
# These orders were removed.
# 
# 
# delcdays column also checked for negative values. All orders with negative values already found removed.
# 
# 
# sla column checked for negative values.
# There are 4073 orders in Camera Accessories segment with values less than zero.
# These orders were removed.
# 
# There are 3470 orders in Gaming Accessories segment with values less than zero.
# This order was removed.
# 
# There are 4202 orders in Home Audio segment with values less than zero.
# These orders were removed.
# 
# Typos check done for product_analytic_sub_category column
# 
# New Discount column added for all three categories.
# 
# Weeks with missing value of NPS and different media investment types in merged dataframes of each category
# are equated to 1 instead of 0, to avoid Inf values. log(0) is infinite.
# 
# Weeks with no orders reported in that week are removed. 
# 
# Outlier treatment done on NPS column and Discount column for all three categories.
# 
# Scaling done on media investments columns and their respective lag columns.
# 
# 
#
################################## Graphs: ###############################################
#   
# Monthly Sales and Total Investment plotted for each month in same plot all three categories.
# Weekly Sales and all 7 media Investment plotted for each week for all three categories.
# 
# 
#
#
################################## Model Generation/Evaluation:#############################
#   
#
# Train-Test split of 70:30 mantained for model evaluation.
# All 5 market mix models and an additional mixed model generated and evaluated.
# Interaction variable analysis done and model generated.
# RMSE on Test dtatset and adjusted R square used as metric for model evaluation.
# 
#
#
#
# 
###############################################################################################################################################################
#   # Category               # Basic Linear Regression     #  Multiplicative  #  Koyck               #  Distributed Lag #     Mixed
###############################################################################################################################################################
#   #                        #                             #                  #                      #       		      #
#   Camera Accessories       #                             #                  #                      #                        #
#   adj. R square:	         # 0.5095		                   # 0.9152           # 0.5095	             # 0.5095		              # 0.2934     
#   RMSE on test:            # 2345414                     # 1968154          # 2345414              # 2345414                # 2719685
#   Singnificant Variables   # PromotionWeek, NPS ,        # ContentMarketing # PromotionWeek, NPS , # PromotionWeek, NPS ,   # SEMLag, interaction variable: 
#                            # Sponsorship                 #                  # Sponsorship          # Sponsorship            # TVLag*ContentMarketingLag
##############################################################################################################################################################   
#   Gaming Accesories
#   adj. R square:	         # 0.5345		                   # 0.6315           # 0.3044	             # 0.3044		              # 0.3501     
#   RMSE on test:            # 1509094			               # 1042405          # 1438992              # 1438992                # 2513942
#   Singnificant Variables   # SEM, Affliates, Sponsorship # ContentMarketing # Affliates            # Affliates              # interaction variable: 
#                            #                             #                  #                                               # TVLag*ContentMarketingLag
##############################################################################################################################################################  
#   Home Audio
#   adj. R square:    	     # 0.4543		                   # 0.7192           # 0.601	               # 0.4724	         	      # 0.8061
#   RMSE on test:            # 2705055			               # 1816283          # 3274922              # 2541250                # 2365752
#   Singnificant Variables   # ContentMarketing, Discount  # TV               # Discount, NPS,       # PromotionWeek,         # interaction variable: 
#                            #                                                # PromotionWeek,       # ContentMarketing       # Discount*NPS,
#                                                                             # salesLag,                                     # ContentMarketing
#                                                                             # ContentMarketing
##############################################################################################################################################################
#
#
# 
################################## Insights: ######################################################
# 
# Sales with Payment Type of COD is higher for all categories. 
# Graph for Weekly Sales and Digital Investment plotted for each week for Camera Accesories shows 2 weeks lag.
# Graph for Weekly Sales and Digital and Online Investment plotted for each week for Gaming Accesories shows 2 weeks lag.
# Graph for Weekly Sales and SEM investment plotted for each week for Home Audio shows 1 week lag.
# Graph for Weekly Sales and Affliates investment plotted for each week for Home Audio shows 2 weeks lag.
#
# Camera Accesories: Log Model (multiplicative) is best with least RMSE. It shows ContentMarketing investment as best choice for media investment
# Gaming Accesories: Log Model (multiplicative) is best with least RMSE. It shows ContentMarketing investment as best choice for media investment
# Home Audio: Log Model (multiplicative) is best with least RMSE. It shows TV investment as best choice for media investment

