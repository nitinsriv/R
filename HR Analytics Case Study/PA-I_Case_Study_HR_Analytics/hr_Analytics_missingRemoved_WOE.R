
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library("MASS")
library(car)
library(caret)
library(Information)
library(ROCR)


# read all data into R
emp_survey <- read.csv("C:\\IIITB\\HR Analytics Case Study\\PA-I_Case_Study_HR_Analytics\\employee_survey_data.csv")
gen_data <- read.csv("C:\\IIITB\\HR Analytics Case Study\\PA-I_Case_Study_HR_Analytics\\general_data.csv")
in_time <- read.csv("C:\\IIITB\\HR Analytics Case Study\\PA-I_Case_Study_HR_Analytics\\in_time.csv", stringsAsFactors=F,header=F)
mgr_survey <- read.csv("C:\\IIITB\\HR Analytics Case Study\\PA-I_Case_Study_HR_Analytics\\manager_survey_data.csv")
out_time <- read.csv("C:\\IIITB\\HR Analytics Case Study\\PA-I_Case_Study_HR_Analytics\\out_time.csv", stringsAsFactors=F,header=F)


# add IN to dates of first row for in_time and OUT to first row of out_time 
in_char <- "IN"
in_time[1,] <- sapply(in_time[1,], function(x) x <- paste(x,in_char,sep="_"))
out_char <- "OUT"
out_time[1,] <- sapply(out_time[1,], function(x) x <- paste(x,out_char,sep="_"))


# make first row as table columns for in_time and out_time
colnames(in_time) <- in_time[1,]
in_time <- in_time[-1,]
colnames(out_time) <- out_time[1,]
out_time <- out_time[-1,]


# in_time and out_time: assumption first column is EmployeeId 
# assign coumnname 'EmployeeID' to first column for in_time and out_time dataframe
# number of unique values in 'EmployeeId column' for both dataframes is 4410
colnames(in_time)[1] <- 'EmployeeID'
colnames(out_time)[1] <- 'EmployeeID'
setdiff(in_time$EmployeeID,out_time$EmployeeID)


# find and remove all IN_TIME and OUT_TIME columns which have all values as NA
in_time_na <- as.data.frame(sapply(in_time, function(x) sum(is.na(x))))
na_cols_in_time <- which(in_time_na == 4410)
in_time <- in_time[,-na_cols_in_time]

out_time_na <- as.data.frame(sapply(out_time, function(x) sum(is.na(x))))
na_cols_out_time <- which(out_time_na == 4410)
out_time <- out_time[,-na_cols_out_time]

diff_hours <- as.numeric(in_time$EmployeeID)
for (i in 2:250){
  act_workHours <- as.numeric(difftime(strptime(out_time[,i],"%Y-%m-%d %H:%M:%S"),
                                       strptime(in_time[,i],"%Y-%m-%d %H:%M:%S")))
  
  diff_hours <- cbind(diff_hours,act_workHours)
}
diff_hours <- as.data.frame(diff_hours)
colnames(diff_hours)[1] <- 'EmployeeID'
diff_hours$ActualWorkingHours <- apply(diff_hours[,-1],1,function(x) mean(x,na.rm=TRUE))
actual_workHours <- diff_hours[,c('EmployeeID','ActualWorkingHours')]

# notice number of rows in EmployeeID column for dataframes  - 4410.
length(unique(emp_survey$EmployeeID)) # confirm EmployeeID can be a key to merge different dataframe
length(unique(gen_data$EmployeeID)) # confirm EmployeeID can be a key to merge different dataframe
length(unique(mgr_survey$EmployeeID)) # confirm EmployeeID can be a key to merge different dataframe
length(unique(in_time$EmployeeID)) # confirm EmployeeID can be a key to merge different dataframe
length(unique(out_time$EmployeeID)) # confirm EmployeeID can be a key to merge different dataframe


# check if all values of employeeID are same inall dataframes
setdiff(emp_survey$EmployeeID,gen_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(gen_data$EmployeeID,in_time$EmployeeID) # Identical customerID across these datasets
setdiff(in_time$EmployeeID,mgr_survey$EmployeeID) # Identical customerID across these datasets
setdiff(mgr_survey$EmployeeID,out_time$EmployeeID) # Identical customerID across these datasets


# merge into single dataframe, joined by EmployeeID values.
emp_ef <- merge(emp_survey,gen_data,by="EmployeeID", all = F)
emp_ef <- merge(emp_ef,mgr_survey,by="EmployeeID", all = F)


# emp_ef <- merge(emp_ef,in_time,by="EmployeeID", all = F)
# emp_ef <- merge(emp_ef,out_time,by="EmployeeID", all = F)


# remove EmployeeCount, Over18 and StandardHours column since they hold same value for all rows.
unique(emp_ef$EmployeeCount)
unique(emp_ef$Over18)
unique(emp_ef$StandardHours)
emp_ef <- emp_ef[,-c(12,19,21)]


# summary of emp_ef
summary(emp_ef)

# structure of emp_ef
str(emp_ef)


########################## Missing Value Imputation ##########################

# find columns containing NA with number of NA
sapply(emp_ef, function(x) sum(is.na(x)))

# number of rows removed .03 % of total observations (4410)
emp_no_na <- na.omit(emp_ef)


levels(emp_no_na$Attrition) <-c(0,1)
emp_no_na$Attrition <- as.numeric(levels(emp_no_na$Attrition))[emp_no_na$Attrition]
IV <- create_infotables(emp_no_na[,-1], y="Attrition", bins=10, parallel=FALSE)



# custom function to compute WoE 
# compute total_good for all '1' values and
# total_bad for '0' values

computeWoE <- function(local_good, local_bad){
  total_good <- length(emp_no_na$Attrition[which(emp_no_na$Attrition == 1)])
  total_bad <- length(emp_no_na$Attrition[which(emp_no_na$Attrition == 0)])
  woe = log(local_good/total_good) - log(local_bad/total_bad)
  return(woe)
}



######################### outliner treatment ##############################

# outliner check for MonthlyIncome
quantile(emp_no_na$MonthlyIncome,seq(0,1,.01))
# jump at 90% to 91%, replacing all greater than 137756.0 with 137756.0
emp_no_na$MonthlyIncome[which(emp_no_na$MonthlyIncome>137756.0)] <- 137756.0


# binning values of Totalworkingyears based on WOE 
#TotalWorkingYears   N    Percent        WOE        IV
#1             [0,2] 363 0.08441860  1.3969494 0.2405392
#2             [3,4] 308 0.07162791  0.2880417 0.2470738
#3             [5,5] 255 0.05930233  0.1587747 0.2486502
#4             [6,7] 602 0.14000000  0.1811905 0.2535323
#5             [8,9] 577 0.13418605 -0.2703599 0.2624687
#6           [10,12] 837 0.19465116 -0.2422809 0.2729815
#7           [13,16] 423 0.09837209 -0.4820665 0.2923153
#8           [17,22] 487 0.11325581 -0.6384822 0.3292575
#9           [23,40] 448 0.10418605 -0.7039883 0.3696231


emp_no_na$TotalWorkingYears <- as.factor(emp_no_na$TotalWorkingYears)

# for coarse classing, compute WOE for 5,6 and 7 values 
TotalWorkingYears_567 <- emp_no_na[which(emp_no_na$TotalWorkingYears==6 | emp_no_na$TotalWorkingYears==7 | 
                                           emp_no_na$TotalWorkingYears==5 ),c(20,6)]
loc_good <- length(TotalWorkingYears_567$Attrition[which(TotalWorkingYears_567$Attrition==1)])
loc_bad <- length(TotalWorkingYears_567$Attrition[which(TotalWorkingYears_567$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # .18

emp_no_na$TotalWorkingYears <- as.numeric(emp_no_na$TotalWorkingYears)
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears>=0 & emp_no_na$TotalWorkingYears<=2)] <- '0-2'
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears>=3 & emp_no_na$TotalWorkingYears<=4)] <- '3-4'
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears>=5 & emp_no_na$TotalWorkingYears<=7)] <- '5-7'
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears>=8 & emp_no_na$TotalWorkingYears<=9)] <- '8-9'
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears>=10 & emp_no_na$TotalWorkingYears<=12)] <- '10-12'
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears>=13 & emp_no_na$TotalWorkingYears<=16)] <- '13-16'
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears>=17 & emp_no_na$TotalWorkingYears<=22)] <- '17-22'

# replace all values greater than 23 years with 23+ years 
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears>=23)] <- '23+'


#YearsAtCompany   N    Percent          WOE         IV
#1          [0,0] 126 0.02930233  1.092779966 0.04807756
#2          [1,1] 499 0.11604651  1.030207662 0.21489228
#3          [2,2] 369 0.08581395  0.345732134 0.22637931
#4          [3,4] 700 0.16279070  0.009043863 0.22639267
#5          [5,6] 799 0.18581395 -0.468968920 0.26111506
#6          [7,8] 498 0.11581395 -0.360796765 0.27442241
#7          [9,9] 234 0.05441860 -0.570198713 0.28892615
#8        [10,14] 610 0.14186047 -0.380870490 0.30696276
#9        [15,40] 465 0.10813953 -0.663537357 0.34472160


# Coarse Classing: Category for 0,1 and 5,6,7 and 8 to be combined. 
# Category 9,10,11,12,13,14 to be combined.
# After coarse classing, WOE trend is monotonic 

emp_no_na$YearsAtCompany <- as.numeric(emp_no_na$YearsAtCompany)

# check quantile distribution for YearsAtCompany 
quantile(emp_no_na$YearsAtCompany,seq(0,1,.01))
# for coarse classing, compute WOE 0,1
YearsAtCompany_01 <- emp_no_na[which(emp_no_na$YearsAtCompany==0 | emp_no_na$YearsAtCompany==1),c(22,6)]
loc_good <- length(YearsAtCompany_01$Attrition[which(YearsAtCompany_01$Attrition==1)])
loc_bad <- length(YearsAtCompany_01$Attrition[which(YearsAtCompany_01$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # 1.04

# for coarse classing, compute WOE 7 till 14
YearsAtCompany_5678 <- emp_no_na[which(emp_no_na$YearsAtCompany>=7 & emp_no_na$YearsAtCompany<=14 ),c(22,6)]
loc_good <- length(YearsAtCompany_5678$Attrition[which(YearsAtCompany_5678$Attrition==1)])
loc_bad <- length(YearsAtCompany_5678$Attrition[which(YearsAtCompany_5678$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.42


temp_yrs <- emp_no_na$YearsAtCompany
emp_no_na$YearsAtCompany[which(temp_yrs>=0 & temp_yrs<=1)] <- '0-1'
emp_no_na$YearsAtCompany[which(temp_yrs>=3 & temp_yrs<=4)] <- '3-4'
emp_no_na$YearsAtCompany[which(temp_yrs>=5 & temp_yrs<=6)] <- '5-6'
emp_no_na$YearsAtCompany[which(temp_yrs>=7 & temp_yrs<=14)] <- '7-14'


# replace all values greater than 15 years with 15+ years 
emp_no_na$YearsAtCompany[which(temp_yrs>=15)] <- '15+'



# check quantile distribution for YearsSinceLastPromotion 
emp_no_na$YearsSinceLastPromotion <- as.numeric((emp_no_na$YearsSinceLastPromotion))
quantile(emp_no_na$YearsSinceLastPromotion,seq(0,1,.01))

# binning values of YearsSinceLastPromotion
#YearsSinceLastPromotion    N    Percent          WOE         IV
#1                   [0,0] 1697 0.39465116  0.186823701 0.01465859
#2                   [1,1] 1050 0.24418605 -0.193060802 0.02317709
#3                   [2,3]  618 0.14372093  0.071279673 0.02392502
#4                   [4,6]  400 0.09302326 -0.579151108 0.04942133
#5                  [7,15]  535 0.12441860 -0.006510387 0.04942660


# for coarse classing, compute WOE 1 to 3 for binning
YearsSinceLastPromotion_123 <- emp_no_na[which(emp_no_na$YearsSinceLastPromotion>=1 & emp_no_na$YearsSinceLastPromotion<=3),c(23,6)]
loc_good <- length(YearsSinceLastPromotion_123$Attrition[which(YearsSinceLastPromotion_123$Attrition==1)])
loc_bad <- length(YearsSinceLastPromotion_123$Attrition[which(YearsSinceLastPromotion_123$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.09


# for coarse classing, compute WOE 4 to 15 for binning
YearsSinceLastPromotion_4_15 <- emp_no_na[which(emp_no_na$YearsSinceLastPromotion>=4 & emp_no_na$YearsSinceLastPromotion<=15),c(23,6)]
loc_good <- length(YearsSinceLastPromotion_4_15$Attrition[which(YearsSinceLastPromotion_4_15$Attrition==1)])
loc_bad <- length(YearsSinceLastPromotion_4_15$Attrition[which(YearsSinceLastPromotion_4_15$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.22


temp_yrsPromotion <- emp_no_na$YearsSinceLastPromotion
emp_no_na$YearsSinceLastPromotion[which(temp_yrsPromotion>=1 & temp_yrsPromotion<=3)] <- '1-3'

# replace all values greater than 11 years with 4+ years 
emp_no_na$YearsSinceLastPromotion[which(temp_yrsPromotion>=4)] <- '4+'


# check quantile distribution for YearsWithCurrManager 
emp_no_na$YearsWithCurrManager <- as.numeric(emp_no_na$YearsWithCurrManager)
quantile(emp_no_na$YearsWithCurrManager,seq(0,1,.01))

#YearsWithCurrManager    N    Percent        WOE        IV
#1                [0,0]  760 0.17674419  0.9272485 0.2007732
#2                [1,1]  222 0.05162791 -0.1351230 0.2016733
#3                [2,2] 1009 0.23465116 -0.1306429 0.2055035
#4                [3,3]  419 0.09744186 -0.2436555 0.2108235
#5                [4,6]  465 0.10813953 -0.3626588 0.2233694
#6                [7,8]  943 0.21930233 -0.2603369 0.2369589
#7               [9,17]  482 0.11209302 -0.8706348 0.2995737


# for coarse classing, combine 1 and 2 to make WOE trend monotonic
YearsWithCurrManager_12 <- emp_no_na[which(emp_no_na$YearsWithCurrManager==1 | 
                                             emp_no_na$YearsWithCurrManager==2),c(24,6)]
loc_good <- length(YearsWithCurrManager_12$Attrition[which(YearsWithCurrManager_12$Attrition==1)])
loc_bad <- length(YearsWithCurrManager_12$Attrition[which(YearsWithCurrManager_12$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.13

YearsWithCurrManager_4_8 <- emp_no_na[which(emp_no_na$YearsWithCurrManager>=4 & emp_no_na$YearsWithCurrManager<=8),c(24,6)]
loc_good <- length(YearsWithCurrManager_4_8$Attrition[which(YearsWithCurrManager_4_8$Attrition==1)])
loc_bad <- length(YearsWithCurrManager_4_8$Attrition[which(YearsWithCurrManager_4_8$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.29

# binning values of YearsWithCurrManager as per WOE
# 1&2 to be combined and 4-8 to be combined
temp_yrsCurMgr <- emp_no_na$YearsWithCurrManager
emp_no_na$YearsWithCurrManager[which(temp_yrsCurMgr>=1 & temp_yrsCurMgr<=2)] <- '1-2'
emp_no_na$YearsWithCurrManager[which(temp_yrsCurMgr>=4 & temp_yrsCurMgr<=8)] <- '4-8'

# replace all values greater than 9 years with 9+ years 
emp_no_na$YearsWithCurrManager[which(temp_yrsCurMgr>=9)] <- '9+'



# check quantile distribution for PercentSalaryHike 
emp_no_na$PercentSalaryHike <- as.numeric(emp_no_na$PercentSalaryHike)
quantile(emp_no_na$PercentSalaryHike,seq(0,1,.01))

#PercentSalaryHike   N    Percent         WOE          IV
#1           [11,11] 616 0.14325581 -0.11932634 0.001958391
#2           [12,12] 577 0.13418605 -0.09593163 0.003153576
#3           [13,13] 616 0.14325581 -0.01884256 0.003204114
#4           [14,14] 583 0.13558140 -0.10807753 0.004730500
#5           [15,16] 526 0.12232558  0.08167868 0.005569300
#6           [17,18] 496 0.11534884  0.01233584 0.005586927
#7           [19,20] 382 0.08883721  0.09828518 0.006473875
#8           [21,25] 504 0.11720930  0.19924622 0.011445736


# for coarse classing, combine 13 and 14 to make WOE tend monotonic
PercentSalaryHike_13_14 <- emp_no_na[which(emp_no_na$PercentSalaryHike==13 | 
                                             emp_no_na$PercentSalaryHike==14),c(18,6)]
loc_good <- length(PercentSalaryHike_13_14$Attrition[which(PercentSalaryHike_13_14$Attrition==1)])
loc_bad <- length(PercentSalaryHike_13_14$Attrition[which(PercentSalaryHike_13_14$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.06


# for coarse classing, comvine 15 till 18 ro make WOE tend monotonic
PercentSalaryHike_15_18 <- emp_no_na[which(emp_no_na$PercentSalaryHike>=15 & emp_no_na$PercentSalaryHike<=18),c(18,6)]
loc_good <- length(PercentSalaryHike_15_18$Attrition[which(PercentSalaryHike_15_18$Attrition==1)])
loc_bad <- length(PercentSalaryHike_15_18$Attrition[which(PercentSalaryHike_15_18$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # .05


# binning values of PercentSalaryHike
temp_perHike <- emp_no_na$PercentSalaryHike
emp_no_na$PercentSalaryHike[which(temp_perHike>=13 & temp_perHike<=14)] <- '13-14'
emp_no_na$PercentSalaryHike[which(temp_perHike>=15 & temp_perHike<=18)] <- '15-18'
emp_no_na$PercentSalaryHike[which(temp_perHike>=19 & temp_perHike<=20)] <- '19-20'


# replace all values greater than 21 years with 21+  
emp_no_na$PercentSalaryHike[which(temp_perHike>=21)] <- '21'


# check quantile distribution for DistanceFromHome 
emp_no_na$DistanceFromHome <- as.numeric(emp_no_na$DistanceFromHome)
quantile(emp_no_na$DistanceFromHome,seq(0,1,.01))

# binning values of DistanceFromHome
#DistanceFromHome   N    Percent         WOE          IV
#1            [1,1] 612 0.14232558 -0.07313919 0.000742638
#2            [2,2] 614 0.14279070  0.15694692 0.004449140
#3            [3,4] 428 0.09953488 -0.18709400 0.007716877
#4            [5,6] 358 0.08325581 -0.14885691 0.009470145
#5            [7,8] 481 0.11186047  0.03423041 0.009602737
#6           [9,10] 507 0.11790698  0.15289872 0.012503600
#7          [11,16] 433 0.10069767  0.08312033 0.013219029
#8          [17,22] 382 0.08883721  0.13406852 0.014889083
#9          [23,29] 485 0.11279070 -0.27407259 0.022598307

# for coarse classing, comvine 11 till 29 to make WOE tend monotonic
DistanceFromHome_11_29 <- emp_no_na[which(emp_no_na$DistanceFromHome>=11 & emp_no_na$DistanceFromHome<=29),c(9,6)]
loc_good <- length(DistanceFromHome_11_29$Attrition[which(DistanceFromHome_11_29$Attrition==1)])
loc_bad <- length(DistanceFromHome_11_29$Attrition[which(DistanceFromHome_11_29$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.02

# for coarse classing, comvine 3 till 10 to make WOE tend monotonic
DistanceFromHome_3_10 <- emp_no_na[which(emp_no_na$DistanceFromHome>=3 & emp_no_na$DistanceFromHome<=10),c(9,6)]
loc_good <- length(DistanceFromHome_3_10$Attrition[which(DistanceFromHome_3_10$Attrition==1)])
loc_bad <- length(DistanceFromHome_3_10$Attrition[which(DistanceFromHome_3_10$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.02

# for coarse classing, comvine 1 till 2 to make WOE tend monotonic
DistanceFromHome_12 <- emp_no_na[which(emp_no_na$DistanceFromHome>=1 & emp_no_na$DistanceFromHome<=2),c(9,6)]
loc_good <- length(DistanceFromHome_12$Attrition[which(DistanceFromHome_12$Attrition==1)])
loc_bad <- length(DistanceFromHome_12$Attrition[which(DistanceFromHome_12$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # .05

# assigning bins
temp_dist <- emp_no_na$DistanceFromHome
emp_no_na$DistanceFromHome[which(temp_dist>=1 & temp_dist<=2)] <- '1-2'
emp_no_na$DistanceFromHome[which(temp_dist>=3 & temp_dist<=10)] <- '3-10'


# replace all values greater than 20  with 20+  
emp_no_na$DistanceFromHome[which(temp_dist>=11)] <- '11+'



# check quantile distribution for DistanceFromHome 
emp_no_na$Age <- as.numeric(emp_no_na$Age)
quantile(emp_no_na$Age,seq(0,1,.01))
boxplot(emp_no_na$Age)

#Age   N    Percent        WOE        IV
#1  [18,25] 363 0.08441860  1.0626612 0.1300888
#2  [26,28] 393 0.09139535  0.2976112 0.1390172
#3  [29,30] 374 0.08697674  0.3286377 0.1494804
#4  [31,33] 551 0.12813953  0.3992264 0.1727371
#5  [34,35] 455 0.10581395 -0.3799950 0.1861330
#6  [36,37] 347 0.08069767 -0.5414899 0.2057278
#7  [38,40] 457 0.10627907 -0.7257546 0.2491533
#8  [41,44] 439 0.10209302 -0.4513413 0.2669342
#9  [45,49] 415 0.09651163 -0.6484938 0.2992945
#10 [50,60] 506 0.11767442 -0.1996615 0.3036751


# for coarse classing, combine 26 till 33 to make WOE tend monotonic
Age_26_33 <- emp_no_na[which(emp_no_na$Age>=26 & emp_no_na$Age<=33),c(5,6)]
loc_good <- length(Age_26_33$Attrition[which(Age_26_33$Attrition==1)])
loc_bad <- length(Age_26_33$Attrition[which(Age_26_33$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # .35


# for coarse classing, comvine 34 till 37 to make WOE tend monotonic
Age_ <- emp_no_na[which(emp_no_na$Age>=34 & emp_no_na$Age<=37),c(5,6)]
loc_good <- length(Age_$Attrition[which(Age_$Attrition==1)])
loc_bad <- length(Age_$Attrition[which(Age_$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.45


# for coarse classing, comvine 38 till 50 to make WOE tend monotonic
Age_ <- emp_no_na[which(emp_no_na$Age>=38 & emp_no_na$Age<=60),c(5,6)]
loc_good <- length(Age_$Attrition[which(Age_$Attrition==1)])
loc_bad <- length(Age_$Attrition[which(Age_$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.48


# binning values of Age
temp_age <- emp_no_na$Age
emp_no_na$Age[which(temp_age>=18 & temp_age<=25)] <- '18-25'
emp_no_na$Age[which(temp_age>=26 & temp_age<=33)] <- '26-33'
emp_no_na$Age[which(temp_age>=34 & temp_age<=37)] <- '34-37'


# replace all values greater than 38 with 38+  
emp_no_na$Age[which(temp_age>=38)] <- '38+'



########################## Dummy Variable Creation ############################


# converting Education into factor.
# Converting "Education" into dummies . 
emp_no_na$Education <- as.factor(emp_no_na$Education)
dummy_education <- data.frame(model.matrix( ~Education, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_education <- dummy_education[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-10], dummy_education)


# converting EnvironmentSatisfaction into factor.
# Converting "EnvironmentSatisfaction" into dummies . 
emp_no_na$EnvironmentSatisfaction <- as.factor(emp_no_na$EnvironmentSatisfaction)
dummy_EnvironmentSatisfaction <- data.frame(model.matrix( ~EnvironmentSatisfaction, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_EnvironmentSatisfaction <- dummy_EnvironmentSatisfaction[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-2], dummy_EnvironmentSatisfaction)


# converting JobSatisfaction into factor.
# Converting "JobSatisfaction" into dummies . 
emp_no_na$JobSatisfaction <- as.factor(emp_no_na$JobSatisfaction)
dummy_JobSatisfaction <- data.frame(model.matrix( ~JobSatisfaction, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_JobSatisfaction <- dummy_JobSatisfaction[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-2], dummy_JobSatisfaction)



# converting WorkLifeBalance into factor.
# Converting "WorkLifeBalance" into dummies . 
emp_no_na$WorkLifeBalance <- as.factor(emp_no_na$WorkLifeBalance)
dummy_WorkLifeBalance <- data.frame(model.matrix( ~WorkLifeBalance, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_WorkLifeBalance <- dummy_WorkLifeBalance[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-2], dummy_WorkLifeBalance)


# converting BusinessTravel into factor.
# Converting "BusinessTravel" into dummies . 
emp_no_na$BusinessTravel <- as.factor(emp_no_na$BusinessTravel)
dummy_BusinessTravel <- data.frame(model.matrix( ~BusinessTravel, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_BusinessTravel <- dummy_BusinessTravel[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-4], dummy_BusinessTravel)


# converting Department into factor.
# Converting "Department" into dummies . 
emp_no_na$Department <- as.factor(emp_no_na$Department)
dummy_Department <- data.frame(model.matrix( ~Department, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_Department <- dummy_Department[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-4], dummy_Department)


# converting EducationField into factor.
# Converting "EducationField" into dummies . 
emp_no_na$EducationField <- as.factor(emp_no_na$EducationField)
dummy_EducationField <- data.frame(model.matrix( ~EducationField, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_EducationField <- dummy_EducationField[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-5], dummy_EducationField)


# variables with 2 levels are  assigned 1 and 0.
# Gender: Male - 0; Female - 1
emp_no_na$Gender <- as.factor(emp_no_na$Gender)
levels(emp_no_na$Gender) <-c(1,0)
emp_no_na$Gender<- as.numeric(levels(emp_no_na$Gender))[emp_no_na$Gender]


# converting JobLevel into factor.
# Converting "JobLevel" into dummies . 
emp_no_na$JobLevel <- as.factor(emp_no_na$JobLevel)
dummy_JobLevel <- data.frame(model.matrix( ~JobLevel, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_JobLevel <- dummy_JobLevel[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-6], dummy_JobLevel)


# converting JobRole into factor.
# Converting "JobRole" into dummies . 
emp_no_na$JobRole <- as.factor(emp_no_na$JobRole)
dummy_JobRole <- data.frame(model.matrix( ~JobRole, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_JobRole <- dummy_JobRole[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-6], dummy_JobRole)


# converting MaritalStatus into factor.
# Converting "MaritalStatus" into dummies . 
emp_no_na$MaritalStatus <- as.factor(emp_no_na$MaritalStatus)
dummy_MaritalStatus <- data.frame(model.matrix( ~MaritalStatus, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_MaritalStatus <- dummy_MaritalStatus[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-6], dummy_MaritalStatus)


# converting NumCompaniesWorked into factor.
# Converting "NumCompaniesWorked" into dummies . 
emp_no_na$NumCompaniesWorked <- as.factor(emp_no_na$NumCompaniesWorked)
dummy_NumCompaniesWorked <- data.frame(model.matrix( ~NumCompaniesWorked, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_NumCompaniesWorked <- dummy_NumCompaniesWorked[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-7], dummy_NumCompaniesWorked)


# converting StockOptionLevel into factor.
# Converting "StockOptionLevel" into dummies . 
emp_no_na$StockOptionLevel <- as.factor(emp_no_na$StockOptionLevel)
dummy_StockOptionLevel <- data.frame(model.matrix( ~StockOptionLevel, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_StockOptionLevel <- dummy_StockOptionLevel[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-8], dummy_StockOptionLevel)


# converting TrainingTimesLastYear into factor.
# Converting "TrainingTimesLastYear" into dummies . 
emp_no_na$TrainingTimesLastYear <- as.factor(emp_no_na$TrainingTimesLastYear)
dummy_TrainingTimesLastYear <- data.frame(model.matrix( ~TrainingTimesLastYear, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_TrainingTimesLastYear <- dummy_TrainingTimesLastYear[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-9], dummy_TrainingTimesLastYear)


# converting JobInvolvement into factor.
# Converting "JobInvolvement" into dummies . 
emp_no_na$JobInvolvement <- as.factor(emp_no_na$JobInvolvement)
dummy_JobInvolvement <- data.frame(model.matrix( ~JobInvolvement, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_JobInvolvement <- dummy_JobInvolvement[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-12], dummy_JobInvolvement)


# converting PerformanceRating into factor.
# Converting "PerformanceRating" into dummies . 
# PerformanceRating has only 3 or 4 values
emp_no_na$PerformanceRating <- as.factor(emp_no_na$PerformanceRating)
levels(emp_no_na$PerformanceRating) <-c(1,0)
emp_no_na$PerformanceRating<- as.numeric(levels(emp_no_na$PerformanceRating))[emp_no_na$PerformanceRating]


# converting PercentSalaryHike into factor.
# Converting "PercentSalaryHike" into dummies . 
emp_no_na$PercentSalaryHike <- as.factor(emp_no_na$PercentSalaryHike)
dummy_PercentSalaryHike <- data.frame(model.matrix( ~PercentSalaryHike, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_PercentSalaryHike <- dummy_PercentSalaryHike[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-7], dummy_PercentSalaryHike)


# converting TotalWorkingYears into factor.
# Converting "TotalWorkingYears" into dummies . 
emp_no_na$TotalWorkingYears <- as.factor(emp_no_na$TotalWorkingYears)
dummy_TotalWorkingYears <- data.frame(model.matrix( ~TotalWorkingYears, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_TotalWorkingYears <- dummy_TotalWorkingYears[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-7], dummy_TotalWorkingYears)


# converting YearsAtCompany into factor.
# Converting "YearsAtCompany" into dummies . 
emp_no_na$YearsAtCompany <- as.factor(emp_no_na$YearsAtCompany)
dummy_YearsAtCompany <- data.frame(model.matrix( ~YearsAtCompany, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_YearsAtCompany <- dummy_YearsAtCompany[,-1]

emp_no_na <- cbind(emp_no_na[,-7], dummy_YearsAtCompany)



# converting YearsSinceLastPromotion into factor.
# Converting "YearsSinceLastPromotion" into dummies . 
emp_no_na$YearsSinceLastPromotion <- as.factor(emp_no_na$YearsSinceLastPromotion)
dummy_YearsSinceLastPromotion <- data.frame(model.matrix( ~YearsSinceLastPromotion, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_YearsSinceLastPromotion <- dummy_YearsSinceLastPromotion[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-7], dummy_YearsSinceLastPromotion)
# Combine the dummy variables to the main data set, after removing the original  column


# converting YearsWithCurrManager into factor.
# Converting "YearsWithCurrManager" into dummies . 
emp_no_na$YearsWithCurrManager <- as.factor(emp_no_na$YearsWithCurrManager)
dummy_YearsWithCurrManager <- data.frame(model.matrix( ~YearsWithCurrManager, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_YearsWithCurrManager <- dummy_YearsWithCurrManager[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-7], dummy_YearsWithCurrManager)


# converting DistanceFromHome into factor.
# Converting "DistanceFromHome" into dummies . 
emp_no_na$DistanceFromHome <- as.factor(emp_no_na$DistanceFromHome)
dummy_DistanceFromHome <- data.frame(model.matrix( ~DistanceFromHome, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_DistanceFromHome <- dummy_DistanceFromHome[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-4], dummy_DistanceFromHome)


# converting Age into factor.
# Converting "Age" into dummies . 
emp_no_na$Age <- as.factor(emp_no_na$Age)
dummy_Age <- data.frame(model.matrix( ~Age, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_Age <- dummy_Age[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-2], dummy_Age)


###################### Dummy Variable Creation - End ##########################

# If working hours are greater that 8.5, mark difference in hours 1 else zero
actual_workHours[which(actual_workHours$ActualWorkingHours > 8.5),2]  <- 1
actual_workHours[which(actual_workHours$ActualWorkingHours != 1.0),2] <- 0


# scale MonthlyIncome
emp_no_na$MonthlyIncome <- scale(emp_no_na$MonthlyIncome)

# final dataframe to be used for model generation
emp_final <- merge(emp_no_na,actual_workHours,by="EmployeeID", all = F)

# remove EmployeeId column
emp_final <- emp_final[,-1]

# Correlation Matrix:
cor_matrix_dataframe <- emp_final[,-1]
cor_matrix_dataframe$Attrition <- as.numeric(cor_matrix_dataframe$Attrition)
cor_df <- cor(cor_matrix_dataframe)


###################### Logistic Regression ############################

# splitting the data between train and test
set.seed(100)
indices= sample(1:nrow(emp_final), 0.7*nrow(emp_final))
train = emp_final[indices,]
test = emp_final[-(indices),]

# first model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)
vif(model_2)

# remove MaritalStatusMarried it has high p-value
model_3 <- glm(Attrition ~  PerformanceRating + Education3 + Education4 + 
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobLevel5 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Representative +  MaritalStatusSingle + 
                 NumCompaniesWorked1 + NumCompaniesWorked4 + NumCompaniesWorked5 + 
                 NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked8 + 
                 NumCompaniesWorked9 + StockOptionLevel1 + TrainingTimesLastYear4 + 
                 TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + 
                 YearsSinceLastPromotion1.3 + YearsSinceLastPromotion4. + 
                 YearsWithCurrManager1.2 + YearsWithCurrManager3 + YearsWithCurrManager4.8 + 
                 YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
               family = "binomial", data = train)
summary(model_3)
vif(model_3)


# remove Education4 it has high p-value
model_4 <- glm(Attrition ~  PerformanceRating + Education3 +  
                 EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobLevel5 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Representative +  MaritalStatusSingle + 
                 NumCompaniesWorked1 + NumCompaniesWorked4 + NumCompaniesWorked5 + 
                 NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked8 + 
                 NumCompaniesWorked9 + StockOptionLevel1 + TrainingTimesLastYear4 + 
                 TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + 
                 YearsSinceLastPromotion1.3 + YearsSinceLastPromotion4. + 
                 YearsWithCurrManager1.2 + YearsWithCurrManager3 + YearsWithCurrManager4.8 + 
                 YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
               family = "binomial", data = train)
summary(model_4)
vif(model_4)

# remove Education3 it has high p-value
model_5 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobLevel5 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Representative +  MaritalStatusSingle + 
                 NumCompaniesWorked1 + NumCompaniesWorked4 + NumCompaniesWorked5 + 
                 NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked8 + 
                 NumCompaniesWorked9 + StockOptionLevel1 + TrainingTimesLastYear4 + 
                 TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + 
                 YearsSinceLastPromotion1.3 + YearsSinceLastPromotion4. + 
                 YearsWithCurrManager1.2 + YearsWithCurrManager3 + YearsWithCurrManager4.8 + 
                 YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
               family = "binomial", data = train)
summary(model_5)
vif(model_5)


#remove YearsWithCurrManager4.8 it has high p-value
model_6 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobLevel5 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Representative +  MaritalStatusSingle + 
                 NumCompaniesWorked1 + NumCompaniesWorked4 + NumCompaniesWorked5 + 
                 NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked8 + 
                 NumCompaniesWorked9 + StockOptionLevel1 + TrainingTimesLastYear4 + 
                 TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + 
                 YearsSinceLastPromotion1.3 + YearsSinceLastPromotion4. + 
                 YearsWithCurrManager1.2 + YearsWithCurrManager3 +  
                 YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
               family = "binomial", data = train)
summary(model_6)
vif(model_6)

# remove YearsWithCurrManager3 it has high p-value
model_7 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobLevel5 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Representative +  MaritalStatusSingle + 
                 NumCompaniesWorked1 + NumCompaniesWorked4 + NumCompaniesWorked5 + 
                 NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked8 + 
                 NumCompaniesWorked9 + StockOptionLevel1 + TrainingTimesLastYear4 + 
                 TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + 
                 YearsSinceLastPromotion1.3 + YearsSinceLastPromotion4. + 
                 YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
               family = "binomial", data = train)
summary(model_7)
vif(model_7)

# remove YearsSinceLastPromotion1.3 it has high p-value
model_8 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobLevel5 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Representative +  MaritalStatusSingle + 
                 NumCompaniesWorked1 + NumCompaniesWorked4 + NumCompaniesWorked5 + 
                 NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked8 + 
                 NumCompaniesWorked9 + StockOptionLevel1 + TrainingTimesLastYear4 + 
                 TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                 YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
               family = "binomial", data = train)
summary(model_8)
vif(model_8)

# remove NumCompaniesWorked8 it has high p-value
model_9 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobLevel5 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Representative +  MaritalStatusSingle + 
                 NumCompaniesWorked1 + NumCompaniesWorked4 + NumCompaniesWorked5 + 
                 NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 + TrainingTimesLastYear4 + 
                 TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                 YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
               family = "binomial", data = train)
summary(model_9)
vif(model_9)

# remove NumCompaniesWorked4 it has high p-value
model_10 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                 WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobLevel5 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Representative +  MaritalStatusSingle + 
                 NumCompaniesWorked1 + NumCompaniesWorked5 + 
                 NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 + TrainingTimesLastYear4 + 
                 TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                 YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
               family = "binomial", data = train)
summary(model_10)
vif(model_10)

# remove TrainingTimesLastYear4 it has high p-value
model_11 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel2 + JobLevel5 + JobRoleManager + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  JobRoleSales.Representative +  MaritalStatusSingle + 
                  NumCompaniesWorked1 + NumCompaniesWorked5 + 
                  NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_11)
vif(model_11)

# remove JobRoleSales.Representative it has high p-value
model_12 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel2 + JobLevel5 + JobRoleManager + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + 
                  NumCompaniesWorked1 + NumCompaniesWorked5 + 
                  NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_12)
vif(model_12)

# remove JobSatisfaction2 it has high p-value
model_13 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel2 + JobLevel5 + JobRoleManager + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + 
                  NumCompaniesWorked1 + NumCompaniesWorked5 + 
                  NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_13)
vif(model_13)

# remove JobSatisfaction3 it has high p-value
model_14 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel2 + JobLevel5 + JobRoleManager + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + 
                  NumCompaniesWorked1 + NumCompaniesWorked5 + 
                  NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_14)
vif(model_14)

# remove NumCompaniesWorked1 it has high p-value
model_15 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel2 + JobLevel5 + JobRoleManager + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_15)
vif(model_15)

# remove JobLevel2 it has high p-value
model_16 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel5 + JobRoleManager + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_16)
vif(model_16)

# remove JobRoleManager it has high p-value
model_17 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_17)
vif(model_17)

# remove EnvironmentSatisfaction3 since it is related to EnvironmentSatisfaction4
model_18 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction2 + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_18)
vif(model_18)

# remove EnvironmentSatisfaction2 since it is insignificant
model_19 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_19)
vif(model_19)

# remove JobInvolvement3 since it is insignificant
model_20 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_20)
vif(model_20)

# remove WorkLifeBalance2 since it is related to WorkLifeBalance3
model_21 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + WorkLifeBalance4 + 
                  BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_21)
vif(model_21)

# remove WorkLifeBalance4 since it is insignificant
model_22 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_22)
vif(model_22)

# remove BusinessTravelTravel_Rarelysince it is related to BusinessTravelTravel_Frequently
model_23 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_23)
vif(model_23)

# remove DepartmentSales it is related to DepartmentResearch...Development
model_24 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_24)
vif(model_24)

# remove JobRoleResearch.Director sincie it is insignificant
model_25 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_25)
vif(model_25)

# remove EducationFieldLife.Sciences sincie it is related to EducationFieldMedical
model_26 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +  
                  EducationFieldMedical + JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_26)
vif(model_26)

# remove EducationFieldMedical since it became insignificant
model_27 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_27)
vif(model_27)

# remove TotalWorkingYears23. since it is related to TotalWorkingYears10.12
model_28 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 +  
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_28)
vif(model_28)

# remove TotalWorkingYears13.16 it is insignificant
model_29 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + TotalWorkingYears10.12 + TotalWorkingYears17.22 +  
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_29)
vif(model_29)

# remove TotalWorkingYears10.12 since it is insignificant
model_30 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + TotalWorkingYears17.22 +  
                  YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_30)
vif(model_30)

# remove TotalWorkingYears17.22 since it is related to YearsAtCompany15.
model_31 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  
                  TrainingTimesLastYear6 + YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_31)
vif(model_31)

#remove StockOptionLevel1 since it is insignificant.
model_32 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 +   
                  TrainingTimesLastYear6 + YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + YearsWithCurrManager9. + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_32)
vif(model_32)

# remove YearsWithCurrManager9. since it is related to YearsSinceLastPromotion4.
model_33 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 +   
                  TrainingTimesLastYear6 + YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 + YearsSinceLastPromotion4. + 
                  YearsWithCurrManager1.2 + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_33)
vif(model_33)

# remove YearsSinceLastPromotion4. since it is related to YearsAtCompany15. and YearsAtCompany7.14
model_34 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 +   
                  TrainingTimesLastYear6 + YearsAtCompany15. + YearsAtCompany5.6 + YearsAtCompany7.14 +  
                  YearsWithCurrManager1.2 + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_34)
vif(model_34)

# remove YearsAtCompany7.14 since it is related to YearsAtCompany15. and YearsAtCompany5.6
model_35 <- glm(Attrition ~  PerformanceRating + EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 +   
                  TrainingTimesLastYear6 + YearsAtCompany15. + YearsAtCompany5.6 +   
                  YearsWithCurrManager1.2 + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_35)
vif(model_35)

# remove PerformanceRating since it is related to YearsAtCompany15. and YearsAtCompany5.6
model_36 <- glm(Attrition ~  EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 +   
                  TrainingTimesLastYear6 + YearsAtCompany15. + YearsAtCompany5.6 +   
                  YearsWithCurrManager1.2 + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_36)
vif(model_36)

# remove YearsAtCompany15. since it is related to Age38. 
model_37 <- glm(Attrition ~  EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 +   
                  TrainingTimesLastYear6 +  YearsAtCompany5.6 +   
                  YearsWithCurrManager1.2 + Age26.33 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_37)
vif(model_37)

# remove Age26.33 since it is related to Age38. 
model_38 <- glm(Attrition ~  EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 +   
                  TrainingTimesLastYear6 +  YearsAtCompany5.6 +   
                  YearsWithCurrManager1.2 + Age34.37 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_38)
vif(model_38)

# remove Age34.37 since it is related to Age38. 
model_39 <- glm(Attrition ~  EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development +JobLevel5 + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 +   
                  TrainingTimesLastYear6 +  YearsAtCompany5.6 +   
                  YearsWithCurrManager1.2 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_39)
vif(model_39)

# remove JobLevel5 since it is insignificant 
model_40 <- glm(Attrition ~  EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  DepartmentResearch...Development + JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 +   
                  TrainingTimesLastYear6 +  YearsAtCompany5.6 +   
                  YearsWithCurrManager1.2 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_40)
vif(model_40)

#remove DepartmentResearch...Development since it is insignificant 
model_41 <- glm(Attrition ~  EnvironmentSatisfaction4 + 
                  JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently +  
                  JobRoleManufacturing.Director + MaritalStatusSingle + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 +   
                  TrainingTimesLastYear6 +  YearsAtCompany5.6 +   
                  YearsWithCurrManager1.2 + Age38. + ActualWorkingHours, 
                family = "binomial", data = train)
summary(model_41)
vif(model_41)


final_model <- model_41


########################## Model Evaluation ###########################


# predicted probabilities of Churn 1 for test data
test_pred = predict(final_model, test[,-1],type = "response")
summary(test_pred)
test$prob <- test_pred

# probability greaer than .5 is 1 (employee will leave)
test_pred_attrition_50 <- factor(ifelse(test_pred >= 0.50, 1,0))


# confusion matrix
test_conf <- confusionMatrix(test_pred_attrition_50, test$Attrition, positive = "1")

#Sensitivity : 0.12273         
#Specificity : 0.98411      
#Accuracy : 0.8372        
test_conf


# compute optimal probalility cutoff for better model reliability
perform_fn <- function(cutoff) 
{
  pred_attrition <- factor(ifelse(test_pred >= cutoff, 1,0))
  conf <- confusionMatrix(pred_attrition, test$Attrition, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.
prob_seq = seq(.006,.82,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(prob_seq[i])
} 

# plot sensitivity , specificity and accuracy with different values of probability
plot(prob_seq, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(prob_seq,OUT[,2],col="darkgreen",lwd=2)
lines(prob_seq,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


# find cutoff probability for threshold value above which represents that employee will leave
# value: .19
cutoff <- prob_seq[which(abs(OUT[,1]-OUT[,2])<0.01)]


# probability greaer than .16 is 1 (employee will leave)
test_pred_attrition <- factor(ifelse(test_pred >= 0.16, 1,0))


# confusion matrix
test_conf <- confusionMatrix(test_pred_attrition, test$Attrition, positive = "1")

#Accuracy : 0.7335  
#Sensitivity : 0.7512          
# Specificity : 0.7303 
test_conf



########################## KS -statistic ######################

ks_stat(test$Attrition,test_pred_attrition)
ks_plot(test$Attrition,test_pred_attrition)
k_stat_prd <- as.vector(as.numeric(test_pred_attrition))
k_stat_act <- as.vector(as.numeric(test$Attrition))
pred_object_test<- prediction(k_stat_prd, k_stat_act)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)

############################ Lift & Gain Chart ########################################


# plotting the lift chart
lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test$Attrition, test_pred_attrition, groups = 10)