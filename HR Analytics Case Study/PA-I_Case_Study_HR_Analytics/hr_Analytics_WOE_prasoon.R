
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
emp_survey <- read.csv("employee_survey_data.csv")
gen_data <- read.csv("general_data.csv")
in_time <- read.csv("in_time.csv", stringsAsFactors=F,header=F)
mgr_survey <- read.csv("manager_survey_data.csv")
out_time <- read.csv("out_time.csv", stringsAsFactors=F,header=F)


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


# custom function to compute WoE 
# compute total_good for all '1' values and
# total_bad for '0' values

computeWoE <- function(local_good, local_bad){
  total_good <- length(emp_ef$Attrition[which(emp_ef$Attrition == 1)])
  total_bad <- length(emp_ef$Attrition[which(emp_ef$Attrition == 0)])
  woe = log(local_good/total_good) - log(local_bad/total_bad)
  return(woe)
}
  
########################## Missing Value Imputation ##########################

# find columns containing NA with number of NA
sapply(emp_ef, function(x) sum(is.na(x)))

levels(emp_ef$Attrition) <-c(0,1)
emp_ef$Attrition <- as.numeric(levels(emp_ef$Attrition))[emp_ef$Attrition]


#IV <- create_infotables(emp_ef[,-1], y="Attrition", bins=10, parallel=FALSE)


# find columns containing NA with number of NA
sapply(emp_ef, function(x) sum(is.na(x)))



# For EnvironmentSatisfaction, No. of NA found 25. 
# WOE for 1,2,3,4 and NA are:
#EnvironmentSatisfaction    N     Percent         WOE           IV
#1                      NA   25 0.005668934  0.26285100 0.0004272596
#2                   [1,1]  845 0.191609977  0.56154813 0.0727103366
#3                   [2,2]  856 0.194104308 -0.08912542 0.0742060233
#4                   [3,3] 1350 0.306122449 -0.18472559 0.0840105683
#5                   [4,4] 1334 0.302494331 -0.21532446 0.0970352143

# plot WOE is monotonic ignoring WOE for NA

x <- seq(1,4,1)
#y <- IV$Tables$EnvironmentSatisfaction$WOE[2:5]
#ggplot(IV$Tables$EnvironmentSatisfaction[-1,],aes(x,y))+geom_smooth()

# using WOE values for EnvironmentSatisfaction

emp_ef$EnvironmentSatisfaction <- as.factor(emp_ef$EnvironmentSatisfaction)
levels(emp_ef$EnvironmentSatisfaction) <-c(.56,-.09,-.19,-.22)
emp_ef$EnvironmentSatisfaction <- as.numeric(levels(emp_ef$EnvironmentSatisfaction))[emp_ef$EnvironmentSatisfaction]

# replace missing values with WoE for NA
emp_ef$EnvironmentSatisfaction[which(is.na(emp_ef$EnvironmentSatisfaction))] <- 0.26



# For JobSatisfaction, No. of NA found 20.
#JobSatisfaction    N     Percent         WOE          IV
#1              NA   20 0.004535147 -1.29529362 0.004831515
#2           [1,1]  860 0.195011338  0.43557410 0.047446739
#3           [2,2]  840 0.190476190  0.02246564 0.047543607
#4           [3,3] 1323 0.300000000  0.03152187 0.047844887
#5           [4,4] 1367 0.309977324 -0.40020037 0.091057122

# Coarse Classing: WOE for 2 and 3 values of JobSatisfaction is same.
# After coarse classing, WOE trend is monotonic 
# Combining value 2 and 3 into one category 

emp_ef$JobSatisfaction <- as.factor(emp_ef$JobSatisfaction)

# for coarse classing, compute WOE for 2 and 3 values to .028 (value of combined_woe)
jb_satisfaction_2_3 <- emp_ef[which(emp_ef$JobSatisfaction==2 | emp_ef$JobSatisfaction==3),c(3,6)]
loc_good <- length(jb_satisfaction_2_3$Attrition[which(jb_satisfaction_2_3$Attrition==1)])
loc_bad <- length(jb_satisfaction_2_3$Attrition[which(jb_satisfaction_2_3$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad)

levels(emp_ef$JobSatisfaction) <-c(.43,.028,.028,-.40)
emp_ef$JobSatisfaction <- as.numeric(levels(emp_ef$JobSatisfaction))[emp_ef$JobSatisfaction]

# replace missing values with WoE for NA
emp_ef$JobSatisfaction[which(is.na(emp_ef$JobSatisfaction))] <- -1.29




# For WorkLifeBalance, No. of NA found 38.
#WorkLifeBalance    N    Percent         WOE          IV
#1              NA   38 0.00861678 -0.49092080 0.001750523
#2           [1,1]  239 0.05419501  0.86676705 0.054752396
#3           [2,2] 1019 0.23106576  0.04792828 0.055291837
#4           [3,3] 2660 0.60317460 -0.14261411 0.066975398
#5           [4,4]  454 0.10294785  0.12201610 0.068572099

# Coarse Classing: WOE for 2 and 3 to be combined.
# After coarse classing, WOE trend is monotonic 
# Combining value 2 and 3 into one category 

emp_ef$WorkLifeBalance <- as.factor(emp_ef$WorkLifeBalance)

# for coarse classing, compute WOE for 3 and 4 values to .10 (value of combined_woe)
worklifebal_2_3 <- emp_ef[which(emp_ef$WorkLifeBalance==3 | emp_ef$WorkLifeBalance==4),c(4,6)]
loc_good <- length(worklifebal_2_3$Attrition[which(worklifebal_2_3$Attrition==1)])
loc_bad <- length(worklifebal_2_3$Attrition[which(worklifebal_2_3$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad)

levels(emp_ef$WorkLifeBalance) <-c(.87,.05,-.1,-.10)
emp_ef$WorkLifeBalance <- as.numeric(levels(emp_ef$WorkLifeBalance))[emp_ef$WorkLifeBalance]

# replace missing values with WoE for -.49
emp_ef$WorkLifeBalance[which(is.na(emp_ef$WorkLifeBalance))] <- -.49



# For NumCompaniesWorked, No. of NA found 19
# NumCompaniesWorked    N    Percent        WOE           IV
#1                 NA   19 0.00430839  0.3273895 0.0005142402
#2              [0,0]  586 0.13287982 -0.3647910 0.0160985101
#3              [1,1] 1558 0.35328798  0.1864906 0.0291736101
#4              [2,2]  438 0.09931973 -0.4458004 0.0460798546
#5              [3,3]  474 0.10748299 -0.5340930 0.0715324358
#6              [4,4]  415 0.09410431 -0.3387290 0.0811360223
#7              [5,6]  395 0.08956916  0.4853299 0.1058076762
#8              [7,9]  525 0.11904762  0.2628510 0.1147801276

# Coarse Classing: Category for 1,2,3 and 4 to be combined. 5,6,7,8,9 to be combined
# After coarse classing, WOE trend is monotonic 

emp_ef$NumCompaniesWorked <- as.factor(emp_ef$NumCompaniesWorked)

# for coarse classing, compute WOE for 1,2,3 and 4 values 
NumCompaniesWorked_1234 <- emp_ef[which(emp_ef$NumCompaniesWorked==2 | emp_ef$NumCompaniesWorked==3 | 
                                      emp_ef$NumCompaniesWorked==4 |emp_ef$NumCompaniesWorked==1),c(17,6)]
loc_good <- length(NumCompaniesWorked_1234$Attrition[which(NumCompaniesWorked_1234$Attrition==1)])
loc_bad <- length(NumCompaniesWorked_1234$Attrition[which(NumCompaniesWorked_1234$Attrition==0)])
combined_woe_1234 <- computeWoE(loc_good,loc_bad) # -.07


# for coarse classing, compute WOE for 5,6,7,8 and 9 values 
NumCompaniesWorked_56789 <- emp_ef[which(emp_ef$NumCompaniesWorked==5 | emp_ef$NumCompaniesWorked==6 |
                                         emp_ef$NumCompaniesWorked==7 | emp_ef$NumCompaniesWorked==8 |
                                           emp_ef$NumCompaniesWorked==9),c(17,6)]
loc_good <- length(NumCompaniesWorked_56789$Attrition[which(NumCompaniesWorked_56789$Attrition==1)])
loc_bad <- length(NumCompaniesWorked_56789$Attrition[which(NumCompaniesWorked_56789$Attrition==0)])
combined_woe_56789 <- computeWoE(loc_good,loc_bad) # .36


# assigning WOE values
levels(emp_ef$NumCompaniesWorked) <-c(-.37,-.07,-.07,-.07,-.07,.36,.36,.36,.36,.36)
emp_ef$NumCompaniesWorked <- as.numeric(levels(emp_ef$NumCompaniesWorked))[emp_ef$NumCompaniesWorked]

# replace missing values with WoE for -.33
emp_ef$NumCompaniesWorked[which(is.na(emp_ef$NumCompaniesWorked))] <- -.33



# For TotalWorkingYears, No. of NA found 9.
# ignoring NA values since they are less than .5 percent of sample size
# TotalWorkingYears   N     Percent        WOE           IV
#1                 NA   9 0.002040816  0.3963824 0.0003648843
#2              [0,2] 368 0.083446712  1.3978309 0.2386678860
#3              [3,4] 315 0.071428571  0.2628510 0.2440513569
#4              [5,5] 264 0.059863946  0.1450680 0.2453738527
#5              [6,7] 618 0.140136054  0.2260370 0.2530919752
#6              [8,9] 594 0.134693878 -0.2550921 0.2611195532
#7            [10,12] 855 0.193877551 -0.2533655 0.2725253823
#8            [13,16] 432 0.097959184 -0.5026168 0.2932994067
#9            [17,22] 499 0.113151927 -0.6622893 0.3326690216
#10           [23,40] 456 0.103401361 -0.7203792 0.3743651832

# Coarse Classing: Category for 5,6,7 to be combined
# After coarse classing, WOE trend is monotonic 

emp_ef$TotalWorkingYears <- as.factor(emp_ef$TotalWorkingYears)

# for coarse classing, compute WOE for 5,6 and 7 values 
TotalWorkingYears_567 <- emp_ef[which(emp_ef$TotalWorkingYears==6 | emp_ef$TotalWorkingYears==7 | 
                                          emp_ef$TotalWorkingYears==5 ),c(20,6)]
loc_good <- length(TotalWorkingYears_567$Attrition[which(TotalWorkingYears_567$Attrition==1)])
loc_bad <- length(TotalWorkingYears_567$Attrition[which(TotalWorkingYears_567$Attrition==0)])
combined_woe_1234 <- computeWoE(loc_good,loc_bad) # -.22

# create dataframe from emp_ef without NA values
# number of rows removed .002 % of total observations (4410)
emp_no_na <- na.omit(emp_ef)


######################### outlier treatment ##############################

# outliner check for MonthlyIncome
quantile(emp_no_na$MonthlyIncome,seq(0,1,.01))
# jump at 90% to 91%, replacing all greater than 137756.0 with 137756.0
emp_no_na$MonthlyIncome[which(emp_no_na$MonthlyIncome>137756.0)] <- 137756.0


# binning values of Totalworkingyears based on WOE shown above
emp_no_na$TotalWorkingYears <- as.numeric(emp_no_na$TotalWorkingYears)

# Category 9,10,11,12,13,14 to be combined.
# After coarse classing, WOE trend is monotonic 

emp_no_na$YearsAtCompany <- as.numeric(emp_no_na$YearsAtCompany)

# check quantile distribution for YearsAtCompany 
quantile(emp_no_na$YearsAtCompany,seq(0,1,.01))
# for coarse classing, compute WOE 0,1
YearsAtCompany_01 <- emp_no_na[which(emp_no_na$YearsAtCompany==0 | emp_no_na$YearsAtCompany==1),c(22,6)]
loc_good <- length(YearsAtCompany_01$Attrition[which(YearsAtCompany_01$Attrition==1)])
loc_bad <- length(YearsAtCompany_01$Attrition[which(YearsAtCompany_01$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # 1.02

# for coarse classing, compute WOE 5,6,7,8
YearsAtCompany_5678 <- emp_no_na[which(emp_no_na$YearsAtCompany==7 | emp_no_na$YearsAtCompany==8 |
                                        emp_no_na$YearsAtCompany==5 | emp_no_na$YearsAtCompany==6 ),c(22,6)]
loc_good <- length(YearsAtCompany_5678$Attrition[which(YearsAtCompany_5678$Attrition==1)])
loc_bad <- length(YearsAtCompany_5678$Attrition[which(YearsAtCompany_5678$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.42


# for coarse classing, compute WOE 9 to 14 for binning
YearsAtCompany_9_14 <- emp_no_na[which(emp_no_na$YearsAtCompany==9 | emp_no_na$YearsAtCompany==10 |
                                        emp_no_na$YearsAtCompany==11 | emp_no_na$YearsAtCompany==12 |
                                        emp_no_na$YearsAtCompany==13 | emp_no_na$YearsAtCompany==14),c(22,6)]
loc_good <- length(YearsAtCompany_9_14$Attrition[which(YearsAtCompany_9_14$Attrition==1)])
loc_bad <- length(YearsAtCompany_9_14$Attrition[which(YearsAtCompany_9_14$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.44


# check quantile distribution for YearsSinceLastPromotion 
emp_no_na$YearsSinceLastPromotion <- as.numeric((emp_no_na$YearsSinceLastPromotion))
quantile(emp_no_na$YearsSinceLastPromotion,seq(0,1,.01))

# binning values of YearsSinceLastPromotion
#YearsSinceLastPromotion    N    Percent         WOE         IV
#1                   [0,0] 1743 0.39523810  0.19476763 0.01599819
#2                   [1,1] 1071 0.24285714 -0.18913412 0.02413969
#3                   [2,3]  633 0.14353741  0.06787833 0.02481634
#4                   [4,6]  414 0.09387755 -0.61421902 0.05339376
#5                  [7,15]  549 0.12448980 -0.02051141 0.05344577



# check quantile distribution for YearsWithCurrManager 
emp_no_na$YearsWithCurrManager <- as.numeric(emp_no_na$YearsWithCurrManager)
quantile(emp_no_na$YearsWithCurrManager,seq(0,1,.01))

#YearsWithCurrManager    N    Percent        WOE        IV
#1                [0,0]  789 0.17891156  0.9100131 0.1950035
#2                [1,1]  228 0.05170068 -0.1273466 0.1958062
#3                [2,2] 1032 0.23401361 -0.1224114 0.1991691
#4                [3,3]  426 0.09659864 -0.2186000 0.2034510
#5                [4,6]  474 0.10748299 -0.3408896 0.2145519
#6                [7,8]  969 0.21972789 -0.2791896 0.2301069
#7               [9,17]  492 0.11156463 -0.8898285 0.2947473


# check quantile distribution for PercentSalaryHike 
emp_no_na$PercentSalaryHike <- as.numeric(emp_no_na$PercentSalaryHike)
quantile(emp_no_na$PercentSalaryHike,seq(0,1,.01))

#PercentSalaryHike   N    Percent         WOE          IV
#1           [11,11] 630 0.14285714 -0.14261411 0.002767159
#2           [12,12] 594 0.13469388 -0.07362124 0.003479133
#3           [13,13] 627 0.14217687  0.01071991 0.003495530
#4           [14,14] 603 0.13673469 -0.13105328 0.005741036
#5           [15,16] 537 0.12176871  0.08592029 0.006666333
#6           [17,18] 513 0.11632653  0.01850524 0.006706418
#7           [19,20] 393 0.08911565  0.10250835 0.007675652
#8           [21,25] 513 0.11632653  0.18040733 0.011696557



# check quantile distribution for DistanceFromHome 
emp_no_na$DistanceFromHome <- as.numeric(emp_no_na$DistanceFromHome)
quantile(emp_no_na$DistanceFromHome,seq(0,1,.01))

# binning values of DistanceFromHome
#DistanceFromHome   N    Percent         WOE           IV
#1            [1,1] 624 0.14149660 -0.05560273 0.0004292633
#2            [2,2] 633 0.14353741  0.13343993 0.0031019798
#3            [3,4] 444 0.10068027 -0.15051929 0.0052684365
#4            [5,6] 372 0.08435374 -0.19047013 0.0081350601
#5            [7,8] 492 0.11156463  0.02500130 0.0082053873
#6           [9,10] 513 0.11632653  0.18040733 0.0122262925
#7          [11,16] 441 0.10000000  0.11145135 0.0135157858
#8          [17,22] 390 0.08843537  0.11181074 0.0146636576
#9          [23,29] 501 0.11360544 -0.28993882 0.0233046423

# check quantile distribution for Age 
emp_no_na$Age <- as.numeric(emp_no_na$Age)
quantile(emp_no_na$Age,seq(0,1,.01))
boxplot(emp_no_na$Age)

#Age   N    Percent        WOE        IV
#1  [18,25] 369 0.08367347  1.0638871 0.1293502
#2  [26,28] 405 0.09183673  0.3530021 0.1421973
#3  [29,30] 384 0.08707483  0.3298617 0.1527561
#4  [31,33] 564 0.12789116  0.3722848 0.1727755
#5  [34,35] 465 0.10544218 -0.3190705 0.1823895
#6  [36,37] 357 0.08095238 -0.5387768 0.2018649
#7  [38,40] 471 0.10680272 -0.7557186 0.2486710
#8  [41,44] 453 0.10272109 -0.4835407 0.2689694
#9  [45,49] 423 0.09591837 -0.6379355 0.3002024
#10 [50,60] 519 0.11768707 -0.2259957 0.3057637





########################## Dummy Variable Creation ############################


# converting Education into factor.
# Converting "Education" into dummies . 
emp_no_na$Education <- as.factor(emp_no_na$Education)
dummy_education <- data.frame(model.matrix( ~Education, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_education <- dummy_education[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("Education", colnames(emp_no_na))], dummy_education)


# converting EnvironmentSatisfaction into factor.
# Converting "EnvironmentSatisfaction" into dummies . 
emp_no_na$EnvironmentSatisfaction <- as.factor(emp_no_na$EnvironmentSatisfaction)
dummy_EnvironmentSatisfaction <- data.frame(model.matrix( ~EnvironmentSatisfaction, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_EnvironmentSatisfaction <- dummy_EnvironmentSatisfaction[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("EnvironmentSatisfaction", colnames(emp_no_na))], dummy_EnvironmentSatisfaction)


# converting JobSatisfaction into factor.
# Converting "JobSatisfaction" into dummies . 
emp_no_na$JobSatisfaction <- as.factor(emp_no_na$JobSatisfaction)
dummy_JobSatisfaction <- data.frame(model.matrix( ~JobSatisfaction, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_JobSatisfaction <- dummy_JobSatisfaction[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("JobSatisfaction", colnames(emp_no_na))], dummy_JobSatisfaction)



# converting WorkLifeBalance into factor.
# Converting "WorkLifeBalance" into dummies . 
emp_no_na$WorkLifeBalance <- as.factor(emp_no_na$WorkLifeBalance)
dummy_WorkLifeBalance <- data.frame(model.matrix( ~WorkLifeBalance, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_WorkLifeBalance <- dummy_WorkLifeBalance[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("WorkLifeBalance", colnames(emp_no_na))], dummy_WorkLifeBalance)


# converting BusinessTravel into factor.
# Converting "BusinessTravel" into dummies . 
emp_no_na$BusinessTravel <- as.factor(emp_no_na$BusinessTravel)
dummy_BusinessTravel <- data.frame(model.matrix( ~BusinessTravel, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_BusinessTravel <- dummy_BusinessTravel[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("BusinessTravel", colnames(emp_no_na))], dummy_BusinessTravel)


# converting Department into factor.
# Converting "Department" into dummies . 
emp_no_na$Department <- as.factor(emp_no_na$Department)
dummy_Department <- data.frame(model.matrix( ~Department, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_Department <- dummy_Department[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("Department", colnames(emp_no_na))], dummy_Department)


# converting EducationField into factor.
# Converting "EducationField" into dummies . 
emp_no_na$EducationField <- as.factor(emp_no_na$EducationField)
dummy_EducationField <- data.frame(model.matrix( ~EducationField, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_EducationField <- dummy_EducationField[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("EducationField", colnames(emp_no_na))], dummy_EducationField)


# variables with 2 levels are  assigned 1 and 0.
# Gender: Male - 0; Female - 1
emp_no_na$Gender <- as.factor(emp_no_na$Gender)
levels(emp_no_na$Gender) <-c(1,0)
emp_no_na$Gender<- as.numeric(levels(emp_no_na$Gender))[emp_no_na$Gender]


# converting JobLevel into factor.
# Converting "JobLevel" into dummies . 
emp_no_na$JobLevel <- as.factor(emp_no_na$JobLevel)
dummy_JobLevel <- data.frame(model.matrix( ~JobLevel, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_JobLevel <- dummy_JobLevel[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("JobLevel", colnames(emp_no_na))], dummy_JobLevel)


# converting JobRole into factor.
# Converting "JobRole" into dummies . 
emp_no_na$JobRole <- as.factor(emp_no_na$JobRole)
dummy_JobRole <- data.frame(model.matrix( ~JobRole, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_JobRole <- dummy_JobRole[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("JobRole", colnames(emp_no_na))], dummy_JobRole)


# converting MaritalStatus into factor.
# Converting "MaritalStatus" into dummies . 
emp_no_na$MaritalStatus <- as.factor(emp_no_na$MaritalStatus)
dummy_MaritalStatus <- data.frame(model.matrix( ~MaritalStatus, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_MaritalStatus <- dummy_MaritalStatus[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("MaritalStatus", colnames(emp_no_na))], dummy_MaritalStatus)


# converting NumCompaniesWorked into factor.
# Converting "NumCompaniesWorked" into dummies . 
emp_no_na$NumCompaniesWorked <- as.factor(emp_no_na$NumCompaniesWorked)
dummy_NumCompaniesWorked <- data.frame(model.matrix( ~NumCompaniesWorked, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_NumCompaniesWorked <- dummy_NumCompaniesWorked[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("NumCompaniesWorked", colnames(emp_no_na))], dummy_NumCompaniesWorked)


# converting StockOptionLevel into factor.
# Converting "StockOptionLevel" into dummies . 
emp_no_na$StockOptionLevel <- as.factor(emp_no_na$StockOptionLevel)
dummy_StockOptionLevel <- data.frame(model.matrix( ~StockOptionLevel, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_StockOptionLevel <- dummy_StockOptionLevel[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("StockOptionLevel", colnames(emp_no_na))], dummy_StockOptionLevel)


# converting TrainingTimesLastYear into factor.
# Converting "TrainingTimesLastYear" into dummies . 
emp_no_na$TrainingTimesLastYear <- as.factor(emp_no_na$TrainingTimesLastYear)
dummy_TrainingTimesLastYear <- data.frame(model.matrix( ~TrainingTimesLastYear, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_TrainingTimesLastYear <- dummy_TrainingTimesLastYear[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("TrainingTimesLastYear", colnames(emp_no_na))], dummy_TrainingTimesLastYear)


# converting JobInvolvement into factor.
# Converting "JobInvolvement" into dummies . 
emp_no_na$JobInvolvement <- as.factor(emp_no_na$JobInvolvement)
dummy_JobInvolvement <- data.frame(model.matrix( ~JobInvolvement, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_JobInvolvement <- dummy_JobInvolvement[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("JobInvolvement", colnames(emp_no_na))], dummy_JobInvolvement)


# converting PerformanceRating into factor.
# Converting "PerformanceRating" into dummies . 
# PerformanceRating has only 3 or 4 values
emp_no_na$PerformanceRating <- as.factor(emp_no_na$PerformanceRating)
levels(emp_no_na$PerformanceRating) <-c(1,0)
emp_no_na$PerformanceRating<- as.numeric(levels(emp_no_na$PerformanceRating))[emp_no_na$PerformanceRating]



# converting YearsWithCurrManager into factor.
# Converting "YearsWithCurrManager" into dummies . 
emp_no_na$YearsWithCurrManager <- as.factor(emp_no_na$YearsWithCurrManager)
dummy_YearsWithCurrManager <- data.frame(model.matrix( ~YearsWithCurrManager, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_YearsWithCurrManager <- dummy_YearsWithCurrManager[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("YearsWithCurrManager", colnames(emp_no_na))], dummy_YearsWithCurrManager)


# converting DistanceFromHome into factor.
# Converting "DistanceFromHome" into dummies . 
emp_no_na$DistanceFromHome <- as.factor(emp_no_na$DistanceFromHome)
dummy_DistanceFromHome <- data.frame(model.matrix( ~DistanceFromHome, data = emp_no_na))

#This column should be removed from the newly created dummy dataframe.
dummy_DistanceFromHome <- dummy_DistanceFromHome[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-grep("DistanceFromHome", colnames(emp_no_na))], dummy_DistanceFromHome)




###################### Dummy Variable Creation - End ##########################



# If working hours are greater that 8.5, mark difference in hours 1 else zero
actual_workHours[which(actual_workHours$ActualWorkingHours > 8.5),2]  <- 1
actual_workHours[which(actual_workHours$ActualWorkingHours != 1.0),2] <- 0


# scale MonthlyIncome
emp_no_na$MonthlyIncome <- scale(emp_no_na$MonthlyIncome)

# final dataframe to be used for model generation
emp_final <- merge(emp_no_na,actual_workHours,by="EmployeeID", all = F)

emp_final <- emp_final[,-1]

# Correlation Matrix:
cor_matrix_dataframe <- emp_final
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

# remove Education5 it has high p-value
model_3 <- glm(Attrition ~ PerformanceRating + EnvironmentSatisfaction.0.19 + 
                 EnvironmentSatisfaction.0.09 + EnvironmentSatisfaction0.56 + 
                 JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance.0.1 + 
                 WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobLevel5 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 MaritalStatusSingle + NumCompaniesWorked.0.07 + NumCompaniesWorked0.36 + 
                 StockOptionLevel1 + TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                 JobInvolvement2 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                 YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                 Age26.33 + Age34.37 + Age38. + ActualWorkingHours + JobRoleResearch.Scientist,
                  family = "binomial", data = train)
summary(model_3)
vif(model_3)


# remove JobLevel5 since it has high p-value
model_4 <- glm(Attrition ~ PerformanceRating + EnvironmentSatisfaction.0.19 + 
                 EnvironmentSatisfaction.0.09 + EnvironmentSatisfaction0.56 + 
                 JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance.0.1 + 
                 WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 MaritalStatusSingle + NumCompaniesWorked.0.07 + NumCompaniesWorked0.36 + 
                 StockOptionLevel1 + TrainingTimesLastYear5 + TrainingTimesLastYear6 + 
                 JobInvolvement2 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                 YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                 Age26.33 + Age34.37 + Age38. + ActualWorkingHours + JobRoleResearch.Scientist,
               family = "binomial", data = train)
summary(model_4)
vif(model_4)


# remove TrainingTimesLastYear5 it has high p-value
model_5 <- glm(Attrition ~ PerformanceRating + EnvironmentSatisfaction.0.19 + 
                 EnvironmentSatisfaction.0.09 + EnvironmentSatisfaction0.56 + 
                 JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance.0.1 + 
                 WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 MaritalStatusSingle + NumCompaniesWorked.0.07 + NumCompaniesWorked0.36 + 
                 StockOptionLevel1 +  TrainingTimesLastYear6 + 
                 JobInvolvement2 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                 YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                 Age26.33 + Age34.37 + Age38. + ActualWorkingHours + JobRoleResearch.Scientist,
               family = "binomial", data = train)
summary(model_5)
vif(model_5)


# remove PerformanceRating it has high p-value
model_6 <- glm(Attrition ~ EnvironmentSatisfaction.0.19 + 
                 EnvironmentSatisfaction.0.09 + EnvironmentSatisfaction0.56 + 
                 JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance.0.1 + 
                 WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 MaritalStatusSingle + NumCompaniesWorked.0.07 + NumCompaniesWorked0.36 + 
                 StockOptionLevel1 +  TrainingTimesLastYear6 + 
                 JobInvolvement2 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                 YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                 Age26.33 + Age34.37 + Age38. + ActualWorkingHours + JobRoleResearch.Scientist,
               family = "binomial", data = train)
summary(model_6)
vif(model_6)


# remove NumCompaniesWorked.0.07 since it has high p-value
model_7 <- glm(Attrition ~ EnvironmentSatisfaction.0.19 + 
                 EnvironmentSatisfaction.0.09 + EnvironmentSatisfaction0.56 + 
                 JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance.0.1 + 
                 WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 MaritalStatusSingle +  NumCompaniesWorked0.36 + 
                 StockOptionLevel1 +  TrainingTimesLastYear6 + 
                 JobInvolvement2 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                 YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                 Age26.33 + Age34.37 + Age38. + ActualWorkingHours + JobRoleResearch.Scientist,
               family = "binomial", data = train)
summary(model_7)
vif(model_7)

# remove JobRoleResearch.Scientist since it has high p-value
model_8 <- glm(Attrition ~ EnvironmentSatisfaction.0.19 + 
                 EnvironmentSatisfaction.0.09 + EnvironmentSatisfaction0.56 + 
                 JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance.0.1 + 
                 WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 MaritalStatusSingle +  NumCompaniesWorked0.36 + 
                 StockOptionLevel1 +  TrainingTimesLastYear6 + 
                 JobInvolvement2 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                 YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                 Age26.33 + Age34.37 + Age38. + ActualWorkingHours,
               family = "binomial", data = train)
summary(model_8)
vif(model_8)


# remove StockOptionLevel1 since it has high p-value
model_9 <- glm(Attrition ~ EnvironmentSatisfaction.0.19 + 
                 EnvironmentSatisfaction.0.09 + EnvironmentSatisfaction0.56 + 
                 JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance.0.1 + 
                 WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 MaritalStatusSingle +  NumCompaniesWorked0.36 + 
                 TrainingTimesLastYear6 + JobInvolvement2 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                 YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                 Age26.33 + Age34.37 + Age38. + ActualWorkingHours,
               family = "binomial", data = train)
summary(model_9)
vif(model_9)


# remove BusinessTravelTravel_Rarely since it is corelated to BusinessTravelTravel_Frequently
model_10 <- glm(Attrition ~ EnvironmentSatisfaction.0.19 + 
                 EnvironmentSatisfaction.0.09 + EnvironmentSatisfaction0.56 + 
                 JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance.0.1 + 
                 WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                 EducationFieldMedical + JobLevel2 + JobRoleManager + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 MaritalStatusSingle +  NumCompaniesWorked0.36 + 
                 TrainingTimesLastYear6 + JobInvolvement2 + JobInvolvement3 + TotalWorkingYears10.12 + 
                 TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                 YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                 YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                 Age26.33 + Age34.37 + Age38. + ActualWorkingHours,
               family = "binomial", data = train)
summary(model_10)
vif(model_10)

# remove JobInvolvement2 since it is corelated 
model_11 <- glm(Attrition ~ EnvironmentSatisfaction.0.19 + 
                  EnvironmentSatisfaction.0.09 + EnvironmentSatisfaction0.56 + 
                  JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance.0.1 + 
                  WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel2 + JobRoleManager + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + 
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                  Age26.33 + Age34.37 + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_11)
vif(model_11)


# remove EnvironmentSatisfaction.0.19  since it is corelated to EnvironmentSatisfaction.0.09
model_12 <- glm(Attrition ~ EnvironmentSatisfaction.0.09 + EnvironmentSatisfaction0.56 + 
                  JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance.0.1 + 
                  WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel2 + JobRoleManager + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + 
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                  Age26.33 + Age34.37 + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_12)
vif(model_12)


# remove EnvironmentSatisfaction.0.09  since it is insignificant
model_13 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + 
                  JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance.0.1 + 
                  WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel2 + JobRoleManager + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + 
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                  Age26.33 + Age34.37 + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_13)
vif(model_13)


# remove WorkLifeBalance.0.1 since it is corelated to WorkLifeBalance0.87
model_14 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + 
                  JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  EducationFieldMedical + JobLevel2 + JobRoleManager + 
                  JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + 
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                  Age26.33 + Age34.37 + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_14)
vif(model_14)


# remove EducationFieldMedical since it is corelated to EducationFieldLife.Sciences
model_15 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + 
                  JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + 
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                  Age26.33 + Age34.37 + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_15)
vif(model_15)


# remove Age26.33 since it is corelated to Age34.37 and Age38.
model_16 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + 
                  JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + 
                  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + 
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                  Age34.37 + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_16)
vif(model_16)

# remove DepartmentSales since it is corelated to DepartmentResearch...Development.
model_17 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + 
                  JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development +  EducationFieldLife.Sciences + 
                  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + 
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                  Age34.37 + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_17)
vif(model_17)

# remove EducationFieldLife.Sciences since it is insignificant.
model_18 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + 
                  JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development +  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + 
                  TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears13.16 + TotalWorkingYears17.22 + TotalWorkingYears23. + 
                  YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                  Age34.37 + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_18)
vif(model_18)


# remove TotalWorkingYears13.16  since iti is related to TotalWorkingYears23.
model_19 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development +  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears17.22 + TotalWorkingYears23. + YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                  Age34.37 + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_19)
vif(model_19)


# remove TotalWorkingYears23. since it is related to TotalWorkingYears10.12
model_20 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development +  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears17.22 + YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                  Age34.37 + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_20)
vif(model_20)


# remove Age34.37 since it is related to Age38.
model_21 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development +  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears17.22 + YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                  Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_21)
vif(model_21)


# remove DepartmentResearch...Development since it is insignificant
model_22 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears17.22 + YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager4.8 + YearsWithCurrManager9. + 
                  Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_22)
vif(model_22)


# remove YearsWithCurrManager4.8 since it is related to YearsWithCurrManager1.2
model_23 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears17.22 + YearsAtCompany5.8 + YearsSinceLastPromotion4. + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager9. + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_23)
vif(model_23)


# remove YearsSinceLastPromotion4. since it is insignificant
model_24 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + JobSatisfaction0.028 + JobSatisfaction0.43 + WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears17.22 + YearsAtCompany5.8 + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager9. + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_24)
vif(model_24)


# remove JobSatisfaction0.028 since it is related to JobSatisfaction0.43
model_25 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + JobSatisfaction0.43 + WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears17.22 + YearsAtCompany5.8 + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + YearsWithCurrManager9. + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_25)
vif(model_25)


# remove YearsWithCurrManager9. since it is related to YearsWithCurrManager1.2
model_26 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + JobSatisfaction0.43 + WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  JobLevel2 + JobRoleManager + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears17.22 + YearsAtCompany5.8 + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_26)
vif(model_26)


# remove JobRoleManager since it became insignificant
model_27 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + JobSatisfaction0.43 + WorkLifeBalance0.87 + BusinessTravelTravel_Frequently + 
                  JobLevel2 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + TrainingTimesLastYear6 + JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears17.22 + YearsAtCompany5.8 + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_27)
vif(model_27)




final_model <- model_27


########################## Model Evaluation ###########################


# predicted probabilities of Churn 1 for test data
test_pred = predict(final_model, test[,-1],type = "response")
summary(test_pred)
test$prob <- test_pred

# probability greaer than .5 is 1 (employee will leave)
test_pred_attrition_50 <- factor(ifelse(test_pred >= 0.50, 1,0))


# confusion matrix
test_conf <- confusionMatrix(test_pred_attrition_50, test$Attrition, positive = "1")

#Sensitivity : 0.25854         
#Specificity : 0.97581      
#Accuracy : 0.8645        
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
prob_seq = seq(.01,.87,length=100)
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
test_pred_attrition <- factor(ifelse(test_pred >= 0.19, 1,0))


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


plot(performance_measures_test)