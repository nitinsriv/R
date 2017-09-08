
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library("MASS")
library(car)
library(caret)
library(ROCR)

# read all data into R
emp_survey <- read.csv("C:\\IIITB\\HR Analytics Case Study\\PA-I_Case_Study_HR_Analytics\\employee_survey_data.csv", stringsAsFactors=F)
gen_data <- read.csv("C:\\IIITB\\HR Analytics Case Study\\PA-I_Case_Study_HR_Analytics\\general_data.csv", stringsAsFactors=F)
in_time <- read.csv("C:\\IIITB\\HR Analytics Case Study\\PA-I_Case_Study_HR_Analytics\\in_time.csv", stringsAsFactors=F,header=F)
mgr_survey <- read.csv("C:\\IIITB\\HR Analytics Case Study\\PA-I_Case_Study_HR_Analytics\\manager_survey_data.csv", stringsAsFactors=F)
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


# Barcharts for Attrition versus categorical features 
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(emp_ef, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(), 
          ggplot(emp_ef, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_ef, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_ef, aes(x=Age,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_ef, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_ef, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")


plot_grid(ggplot(emp_ef, aes(x=EducationField,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(emp_ef, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_ef, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_ef, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_ef, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_ef, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")



plot_grid(ggplot(emp_ef, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar() + bar_theme1, 
              align = "h")



# find actual number of employees for whome attrition is 'Yes' (who left)
# 711 employees actual left in given dataset
emp_left <- emp_ef[which(emp_ef$Attrition == "Yes"),]
no_attrition <- length(which(emp_ef$Attrition == "Yes"))


plot_grid(ggplot(emp_left, aes(x=EnvironmentSatisfaction))+ geom_bar(), 
          ggplot(emp_left, aes(x=JobSatisfaction))+ geom_bar()+bar_theme1,
          ggplot(emp_left, aes(x=WorkLifeBalance))+ geom_bar()+bar_theme1,
          ggplot(emp_left, aes(x=Age))+ geom_bar()+bar_theme1,
          ggplot(emp_left, aes(x=BusinessTravel))+ geom_bar()+bar_theme1,
          ggplot(emp_left, aes(x=Department))+ geom_bar()+bar_theme1,
          align = "h")


plot_grid(ggplot(emp_left, aes(x=EducationField))+ geom_bar() + bar_theme1, 
          ggplot(emp_left, aes(x=Gender))+ geom_bar()+bar_theme1,
          ggplot(emp_left, aes(x=JobLevel))+ geom_bar()+bar_theme1,
          ggplot(emp_left, aes(x=JobRole))+ geom_bar()+bar_theme1,
          ggplot(emp_left, aes(x=MaritalStatus))+ geom_bar()+bar_theme1,
          ggplot(emp_left, aes(x=NumCompaniesWorked))+ geom_bar()+bar_theme1,
          align = "h")



plot_grid(ggplot(emp_left, aes(x=StockOptionLevel))+ geom_bar() + bar_theme1, 
          align = "h")



########################## Missing Value Imputation ##########################

# find columns containing NA with number of NA
sapply(emp_ef, function(x) sum(is.na(x)))


# Plots for the NA rows to understand impact of removing rows on overall result

# For EnvironmentSatisfaction, No. of NA found 25, ignoring them.
# No. of NA is very less compared to those who who left
# removing NA rows with 'Yes' attriion value very low 
# and will not have an overall impact on result if these are removed.
env_na <- emp_ef[(which(is.na(emp_ef$EnvironmentSatisfaction) == TRUE)),]
plot_grid(ggplot(env_na, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(),
          ggplot(env_na, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(env_na, aes(x=Age,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(env_na, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(env_na, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")


plot_grid(ggplot(env_na, aes(x=EducationField,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(env_na, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(env_na, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(env_na, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(env_na, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(env_na, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")


plot_grid(ggplot(env_na, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar() + bar_theme1, 
          align = "h")


# For JobSatisfaction, No. of NA found 20, ignoring them.
# No. of NA is very less compared to those who who left
# removing NA rows with 'Yes' attriion value very low 
# and will not have an overall impact on result if these are removed.
job_na <- emp_ef[(which(is.na(emp_ef$JobSatisfaction) == TRUE)),]
plot_grid(ggplot(job_na, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(), 
          ggplot(job_na, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(job_na, aes(x=Age,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(job_na, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(job_na, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")


plot_grid(ggplot(job_na, aes(x=EducationField,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(job_na, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(job_na, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(job_na, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(job_na, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(job_na, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")


plot_grid(ggplot(job_na, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar() + bar_theme1, 
          align = "h")



# For WorkLifeBalance, No. of NA found 38, ignoring them.
# No. of NA is very less compared to those who who left
# removing NA rows with 'Yes' attriion value very low 
# and will not have an overall impact on result if these are removed.
worklifbalance_na <- emp_ef[(which(is.na(emp_ef$WorkLifeBalance) == TRUE)),]
plot_grid(ggplot(worklifbalance_na, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(), 
          ggplot(worklifbalance_na, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(worklifbalance_na, aes(x=Age,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(worklifbalance_na, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(worklifbalance_na, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")


plot_grid(ggplot(worklifbalance_na, aes(x=EducationField,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(worklifbalance_na, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(worklifbalance_na, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(worklifbalance_na, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(worklifbalance_na, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(worklifbalance_na, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")

plot_grid(ggplot(worklifbalance_na, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar() + bar_theme1, 
          align = "h")



# For NumCompaniesWorked, No. of NA found 19, ignoring them.
# No. of NA is very less compared to those who who left
# removing NA rows with 'Yes' attriion value very low 
# and will not have an overall impact on result if these are removed.
numCompaniesWorked_na <- emp_ef[(which(is.na(emp_ef$NumCompaniesWorked) == TRUE)),]
plot_grid(ggplot(numCompaniesWorked_na, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(), 
          ggplot(numCompaniesWorked_na, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(numCompaniesWorked_na, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(numCompaniesWorked_na, aes(x=Age,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(numCompaniesWorked_na, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(numCompaniesWorked_na, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")


plot_grid(ggplot(numCompaniesWorked_na, aes(x=EducationField,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(numCompaniesWorked_na, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(numCompaniesWorked_na, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(numCompaniesWorked_na, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(numCompaniesWorked_na, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")


plot_grid(ggplot(numCompaniesWorked_na, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar() + bar_theme1, 
          align = "h")



# For TotalWorkingYears, No. of NA found 9, ignoring them.
totalWorkingYears_na <- emp_ef[(which(is.na(emp_ef$TotalWorkingYears) == TRUE)),]


# create dataframe from emp_ef without NA values
# number of rows removed .03 % of total observations (4410)
emp_no_na <- na.omit(emp_ef)




######################### outliner treatment ##############################

# outliner check for MonthlyIncome
quantile(emp_no_na$MonthlyIncome,seq(0,1,.01))
# jump at 90% to 91%, replacing all greater than 137756.0 with 137756.0
emp_no_na$MonthlyIncome[which(emp_no_na$MonthlyIncome>137756.0)] <- 137756.0


# plot TotalWorkingYears, YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager, Age,  to understand binning
plot_grid(ggplot(emp_no_na, aes(x=TotalWorkingYears,fill=Attrition))+ geom_bar() +bar_theme1,
          ggplot(emp_no_na, aes(x=YearsAtCompany,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_no_na, aes(x=YearsSinceLastPromotion,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_no_na, aes(x=YearsWithCurrManager,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_no_na, aes(x=Age,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_no_na, aes(x=PercentSalaryHike,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")

# check quantile distribution for TotalWorkingYears 
quantile(emp_no_na$TotalWorkingYears,seq(0,1,.01))

# binning values of Totalworkingyears
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears==0 | emp_no_na$TotalWorkingYears==1)] <- '0-1'
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears>=2 & emp_no_na$TotalWorkingYears<=6)] <- '2-6'
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears>=7 & emp_no_na$TotalWorkingYears<=9)] <- '7-10'
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears>=11 & emp_no_na$TotalWorkingYears<=13)] <- '11-13'
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears>=14 & emp_no_na$TotalWorkingYears<=19)] <- '14-19'
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears>=20 & emp_no_na$TotalWorkingYears<=25)] <- '20-25'
# replace all values greater than 26 years with 26+ years 
emp_no_na$TotalWorkingYears[which(emp_no_na$TotalWorkingYears>=26)] <- '26+'



# check quantile distribution for YearsAtCompany 
quantile(emp_no_na$YearsAtCompany,seq(0,1,.01))

# binning values of YearsAtCompany
temp_yrs <- emp_no_na$YearsAtCompany
emp_no_na$YearsAtCompany[which(temp_yrs>=0 & temp_yrs<=2)] <- '0-2'
emp_no_na$YearsAtCompany[which(temp_yrs>=3 & temp_yrs<=5)] <- '3-5'
emp_no_na$YearsAtCompany[which(temp_yrs>=6 & temp_yrs<=11)] <- '6-11'
emp_no_na$YearsAtCompany[which(temp_yrs>=12 & temp_yrs<=14)] <- '12-14'
emp_no_na$YearsAtCompany[which(temp_yrs>=15 & temp_yrs<=19)] <- '15-19'
emp_no_na$YearsAtCompany[which(temp_yrs>=20 & temp_yrs<=21)] <- '20-21'

# replace all values greater than 22 years with 22+ years 
emp_no_na$YearsAtCompany[which(temp_yrs>=22)] <- '22+'



# check quantile distribution for YearsAtCompany 
quantile(emp_no_na$YearsSinceLastPromotion,seq(0,1,.01))

# binning values of YearsAtCompany
temp_yrsPromotion <- emp_no_na$YearsSinceLastPromotion
emp_no_na$YearsSinceLastPromotion[which(temp_yrsPromotion>=0 & temp_yrsPromotion<=2)] <- '0-2'
emp_no_na$YearsSinceLastPromotion[which(temp_yrsPromotion>=3 & temp_yrsPromotion<=7)] <- '3-7'
emp_no_na$YearsSinceLastPromotion[which(temp_yrsPromotion>=8 & temp_yrsPromotion<=10)] <- '8-10'

# replace all values greater than 11 years with 11+ years 
emp_no_na$YearsSinceLastPromotion[which(temp_yrsPromotion>=11)] <- '11+'



# check quantile distribution for YearsWithCurrManager 
quantile(emp_no_na$YearsWithCurrManager,seq(0,1,.01))

# binning values of YearsWithCurrManager
temp_yrsCurMgr <- emp_no_na$YearsWithCurrManager
emp_no_na$YearsWithCurrManager[which(temp_yrsCurMgr>=0 & temp_yrsCurMgr<=2)] <- '0-2'
emp_no_na$YearsWithCurrManager[which(temp_yrsCurMgr>=3 & temp_yrsCurMgr<=6)] <- '3-6'
emp_no_na$YearsWithCurrManager[which(temp_yrsCurMgr>=7 & temp_yrsCurMgr<=11)] <- '7-11'

# replace all values greater than 12 years with 22+ years 
emp_no_na$YearsWithCurrManager[which(temp_yrsCurMgr>=12)] <- '12+'



# check quantile distribution for PercentSalaryHike 
quantile(emp_no_na$PercentSalaryHike,seq(0,1,.01))

# binning values of PercentSalaryHike
temp_perHike <- emp_no_na$PercentSalaryHike
emp_no_na$PercentSalaryHike[which(temp_perHike>=10 & temp_perHike<=14)] <- '10-14'
emp_no_na$PercentSalaryHike[which(temp_perHike>=15 & temp_perHike<=17)] <- '15-17'
emp_no_na$PercentSalaryHike[which(temp_perHike>=18 & temp_perHike<=22)] <- '18-22'

# replace all values greater than 26 years with 23+ years 
emp_no_na$PercentSalaryHike[which(temp_perHike>=23)] <- '23+'


# check quantile distribution for DistanceFromHome 
quantile(emp_no_na$DistanceFromHome,seq(0,1,.01))
ggplot(emp_no_na, aes(x=DistanceFromHome,fill=Attrition))+ geom_bar()

# binning values of DistanceFromHome
temp_dist <- emp_no_na$DistanceFromHome
emp_no_na$DistanceFromHome[which(temp_dist>=1 & temp_dist<=3)] <- '1-3'
emp_no_na$DistanceFromHome[which(temp_dist>=4 & temp_dist<=7)] <- '4-7'
emp_no_na$DistanceFromHome[which(temp_dist>=8 & temp_dist<=10)] <- '8-10'
emp_no_na$DistanceFromHome[which(temp_dist>=11 & temp_dist<=14)] <- '11-14'
emp_no_na$DistanceFromHome[which(temp_dist>=15 & temp_dist<=19)] <- '15-19'

# replace all values greater than 20  with 20+  
emp_no_na$DistanceFromHome[which(temp_dist>=20)] <- '20+'


# check quantile distribution for DistanceFromHome 
quantile(emp_no_na$Age,seq(0,1,.01))
ggplot(emp_no_na, aes(x=Age,fill=Attrition))+ geom_bar()

# binning values of DistanceFromHome
temp_age <- emp_no_na$Age
emp_no_na$Age[which(temp_age>=18 & temp_age<=20)] <- '18-20'
emp_no_na$Age[which(temp_age>=21 & temp_age<=23)] <- '21-23'
emp_no_na$Age[which(temp_age>=24 & temp_age<=27)] <- '24-27'
emp_no_na$Age[which(temp_age>=28 & temp_age<=33)] <- '28-33'
emp_no_na$Age[which(temp_age>=33 & temp_age<=38)] <- '33-38'
emp_no_na$Age[which(temp_age>=39 & temp_age<=41)] <- '39-41'
emp_no_na$Age[which(temp_age>=42 & temp_age<=45)] <- '42-45'
emp_no_na$Age[which(temp_age>=46 & temp_age<=50)] <- '46-50'
emp_no_na$Age[which(temp_age>=50 & temp_age<=57)] <- '50-57'


# replace all values greater than 20  with 20+  
emp_no_na$Age[which(temp_age>=58)] <- '58+'



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
dummy_PerformanceRating <- data.frame(model.matrix( ~PerformanceRating, data = emp_no_na))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_PerformanceRating <- dummy_PerformanceRating[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-12], dummy_PerformanceRating)


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

# set Attrition value 'Yes' to 1 and 'No' to 0
emp_no_na$Attrition <- factor(ifelse(emp_no_na$Attrition == 'Yes', 1,0))

# final dataframe to be used for model generation
emp_final <- merge(emp_no_na,actual_workHours,by="EmployeeID", all = F)


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

# remove StockOptionLevel3 since it has high p-value
model_3 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationFieldLife.Sciences + EducationFieldMedical + 
                 JobLevel2 + JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusMarried + 
                 MaritalStatusSingle + NumCompaniesWorked1 + NumCompaniesWorked3 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                 NumCompaniesWorked9 + StockOptionLevel1 +  
                 TrainingTimesLastYear4 + TrainingTimesLastYear6 + JobInvolvement3 + 
                 PercentSalaryHike18.22 + PercentSalaryHike23. + TotalWorkingYears10 + 
                 TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                 TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany15.19 + 
                 YearsAtCompany20.21 + YearsAtCompany22. + YearsAtCompany3.5 + 
                 YearsAtCompany6.11 + YearsSinceLastPromotion11. + YearsSinceLastPromotion3.7 + 
                 YearsSinceLastPromotion8.10 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                 DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + 
                 Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                 JobRoleLaboratory.Technician, family = "binomial", data = train)
summary(model_3)
vif(model_3)


# remove NumCompaniesWorked1 because og high p-value
model_4 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + 
                 DepartmentSales + EducationFieldLife.Sciences + EducationFieldMedical + 
                 JobLevel2 + JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusMarried + 
                 MaritalStatusSingle + NumCompaniesWorked3 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + 
                 NumCompaniesWorked9 + StockOptionLevel1 +  
                 TrainingTimesLastYear4 + TrainingTimesLastYear6 + JobInvolvement3 + 
                 PercentSalaryHike18.22 + PercentSalaryHike23. + TotalWorkingYears10 + 
                 TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                 TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany15.19 + 
                 YearsAtCompany20.21 + YearsAtCompany22. + YearsAtCompany3.5 + 
                 YearsAtCompany6.11 + YearsSinceLastPromotion11. + YearsSinceLastPromotion3.7 + 
                 YearsSinceLastPromotion8.10 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                 DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + 
                 Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                 JobRoleLaboratory.Technician, family = "binomial", data = train)
summary(model_4)
vif(model_4)


# remove MaritalStatusMarried since it has high p-value (insignificant)
model_5 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + 
      EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
      JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
      WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
      BusinessTravelTravel_Rarely + DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + EducationFieldMedical + 
      JobLevel2 + JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
      JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + NumCompaniesWorked3 + 
      NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  TrainingTimesLastYear4 + TrainingTimesLastYear6 + 
      JobInvolvement3 + PercentSalaryHike18.22 + PercentSalaryHike23. + TotalWorkingYears10 + TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
      TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany15.19 + YearsAtCompany20.21 + YearsAtCompany22. + YearsAtCompany3.5 + YearsAtCompany6.11 + YearsSinceLastPromotion11. + YearsSinceLastPromotion3.7 + 
      YearsSinceLastPromotion8.10 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
      DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
      JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_5)
vif(model_5)


# remove JobLever2 because it is insgnificant
model_6 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + EducationFieldMedical + 
                 JobLevel5 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + NumCompaniesWorked3 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  TrainingTimesLastYear4 + TrainingTimesLastYear6 + 
                 JobInvolvement3 + PercentSalaryHike18.22 + PercentSalaryHike23. + TotalWorkingYears10 + TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                 TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany15.19 + YearsAtCompany20.21 + YearsAtCompany22. + YearsAtCompany3.5 + YearsAtCompany6.11 + YearsSinceLastPromotion11. + YearsSinceLastPromotion3.7 + 
                 YearsSinceLastPromotion8.10 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                 DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                 JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_6)
vif(model_6)


# remove JobLever2 because it is insgnificant
model_7 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + EducationFieldMedical + 
                 JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + NumCompaniesWorked3 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  TrainingTimesLastYear4 + TrainingTimesLastYear6 + 
                 JobInvolvement3 + PercentSalaryHike18.22 + PercentSalaryHike23. + TotalWorkingYears10 + TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                 TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany15.19 + YearsAtCompany20.21 + YearsAtCompany22. + YearsAtCompany3.5 + YearsAtCompany6.11 + YearsSinceLastPromotion11. + YearsSinceLastPromotion3.7 + 
                 YearsSinceLastPromotion8.10 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                 DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                 JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_7)
vif(model_7)


# remove PercentSalaryHike18.22 (low p-value)
model_8 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + 
                 EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction2 + 
                 JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + EducationFieldMedical + 
                 JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + NumCompaniesWorked3 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +  TrainingTimesLastYear4 + TrainingTimesLastYear6 + 
                 JobInvolvement3 + PercentSalaryHike23. + TotalWorkingYears10 + TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                 TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany15.19 + YearsAtCompany20.21 + YearsAtCompany22. + YearsAtCompany3.5 + YearsAtCompany6.11 + YearsSinceLastPromotion11. + YearsSinceLastPromotion3.7 + 
                 YearsSinceLastPromotion8.10 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                 DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                 JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_8)
vif(model_8)


# remove TrainingTimesLastYear4 (low p-value)
model_9 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + EducationFieldMedical + 
                 JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + NumCompaniesWorked3 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                 TrainingTimesLastYear6 + JobInvolvement3 + PercentSalaryHike23. + TotalWorkingYears10 + TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                 TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany15.19 + YearsAtCompany20.21 + YearsAtCompany22. + YearsAtCompany3.5 + YearsAtCompany6.11 + YearsSinceLastPromotion11. + YearsSinceLastPromotion3.7 + 
                 YearsSinceLastPromotion8.10 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                 DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                 JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_9)
vif(model_9)


# remove BusinessTravelTravel_Rarely because it has high VIF and comparably higher p-value
model_10 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                 JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                 DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + EducationFieldMedical + 
                 JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + NumCompaniesWorked3 + 
                 NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                 TrainingTimesLastYear6 + JobInvolvement3 + PercentSalaryHike23. + TotalWorkingYears10 + TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                 TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany15.19 + YearsAtCompany20.21 + YearsAtCompany22. + YearsAtCompany3.5 + YearsAtCompany6.11 + YearsSinceLastPromotion11. + YearsSinceLastPromotion3.7 + 
                 YearsSinceLastPromotion8.10 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                 DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                 JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_10)
vif(model_10)


# remove YearsAtCompany20.21 because it has high VIF and comparably higher p-value
model_11 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + EducationFieldMedical + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + NumCompaniesWorked3 + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + JobInvolvement3 + PercentSalaryHike23. + TotalWorkingYears10 + TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                  TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany15.19 + YearsAtCompany22. + YearsAtCompany3.5 + YearsAtCompany6.11 + YearsSinceLastPromotion11. + YearsSinceLastPromotion3.7 + 
                  YearsSinceLastPromotion8.10 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                  DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_11)
vif(model_11)


# remove YearsAtCompany22. because it has high VIF and comparably higher p-value
model_12 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + EducationFieldMedical + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + NumCompaniesWorked3 + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + JobInvolvement3 + PercentSalaryHike23. + TotalWorkingYears10 + TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                  TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany15.19 + YearsAtCompany3.5 + YearsAtCompany6.11 + YearsSinceLastPromotion11. + YearsSinceLastPromotion3.7 + 
                  YearsSinceLastPromotion8.10 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                  DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_12)
vif(model_12)


# remove YearsSinceLastPromotion8.10 because it has high VIF and comparably higher p-value
model_13 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + EducationFieldMedical + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + NumCompaniesWorked3 + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + JobInvolvement3 + PercentSalaryHike23. + TotalWorkingYears10 + TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                  TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany15.19 + YearsAtCompany3.5 + YearsAtCompany6.11 + YearsSinceLastPromotion11. + YearsSinceLastPromotion3.7 + 
                  YearsWithCurrManager12. + DistanceFromHome11.14 + 
                  DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_13)
vif(model_13)


# remove YearsSinceLastPromotion11. because it has high VIF and comparably higher p-value
model_14 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + EducationFieldMedical + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + NumCompaniesWorked3 + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + JobInvolvement3 + PercentSalaryHike23. + TotalWorkingYears10 + TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                  TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany15.19 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                  DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_14)
vif(model_14)


# remove YearsAtCompany15.19 since it has high p-value and slightly corelated to YearsWithCurrManager12.
model_15 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + EducationFieldMedical + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle + NumCompaniesWorked3 + 
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + JobInvolvement3 + PercentSalaryHike23. + TotalWorkingYears10 + TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                  TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                  DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_15)
vif(model_15)


# remove NumCompaniesWorked3 since it has high p-value 
model_16 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences + EducationFieldMedical + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle +  
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + JobInvolvement3 + PercentSalaryHike23. + TotalWorkingYears10 + TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                  TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                  DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_16)
vif(model_16)


# remove EducationFieldMedical since it is corelated to EducationFieldLife.Sciences
model_17 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + EducationFieldLife.Sciences +  
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle +  
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + JobInvolvement3 + PercentSalaryHike23. + TotalWorkingYears10 + TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                  TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                  DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_17)
vif(model_17)


# remove EducationFieldLife.Sciences since it is insignificant
model_18 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + JobLevel5 + JobRoleResearch.Director +
                  JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle +  
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + JobInvolvement3 + PercentSalaryHike23. + TotalWorkingYears10 + TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                  TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                  DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_18)
vif(model_18)


# remove JobInvolvement3 since it is insignificant
model_19 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + DepartmentSales + JobLevel5 + JobRoleResearch.Director +
                  JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle +  
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                  TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                  DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_19)
vif(model_19)


# remove DepartmentSales since it is correlated to DepartmentResearch...Development
model_20 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + JobLevel5 + JobRoleResearch.Director +
                  JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle +  
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                  TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                  DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_20)
vif(model_20)


# remove WorkLifeBalance2 since it is corelated to WorkLifeBalance3
model_21 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance3 + WorkLifeBalance4 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + JobLevel5 + JobRoleResearch.Director +
                  JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle +  
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                  TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                  DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_21)
vif(model_21)


# remove WorkLifeBalance4 since it became insignificant
model_22 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + 
                  JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + JobLevel5 + JobRoleResearch.Director +
                  JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle +  
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                  TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome11.14 + 
                  DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_22)
vif(model_22)


# remove DistanceFromHome11.14 since it is insignificant
model_23 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 +
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + JobLevel5 + JobRoleResearch.Director +
                  JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle +  
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                  TotalWorkingYears26. + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_23)
vif(model_23)


# remove TotalWorkingYears26. since it is correlated to TotalWorkingYears2.6
model_24 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 +
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + JobLevel5 + JobRoleResearch.Director +
                  JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle +  
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears11.13 + TotalWorkingYears14.19 + TotalWorkingYears2.6 + 
                  YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_24)
vif(model_24)


# remove TotalWorkingYears11.13 since it became insignificant
model_25 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 +
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + JobLevel5 + JobRoleResearch.Director +
                  JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle +  
                  NumCompaniesWorked5 + NumCompaniesWorked6 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears14.19 + TotalWorkingYears2.6 + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_25)
vif(model_25)


# remove NumCompaniesWorked6 since it became insignificant
model_26 <- glm(Attrition ~ Education3 + Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 +
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + WorkLifeBalance3 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + JobLevel5 + JobRoleResearch.Director +
                  JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle +  
                  NumCompaniesWorked5 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears14.19 + TotalWorkingYears2.6 + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_26)
vif(model_26)


# remove Education3 since it is correlated to Education4
model_27 <- glm(Attrition ~ Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 +
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  WorkLifeBalance3 + BusinessTravelTravel_Frequently + 
                  DepartmentResearch...Development + JobLevel5 + JobRoleResearch.Director +
                  JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle +  
                  NumCompaniesWorked5 + NumCompaniesWorked7 + NumCompaniesWorked9 + StockOptionLevel1 +   
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears14.19 + TotalWorkingYears2.6 + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_27)
vif(model_27)


# remove StockOptionLevel1 since it it became insignificant
model_28 <- glm(Attrition ~ Education4 + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 +
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  WorkLifeBalance3 + BusinessTravelTravel_Frequently + DepartmentResearch...Development + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle +  NumCompaniesWorked5 + NumCompaniesWorked7 + NumCompaniesWorked9 +    
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears14.19 + TotalWorkingYears2.6 + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. + ActualWorkingHours + 
                  JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_28)
vif(model_28)


# remove Education4 since it became insignificant
model_29 <- glm(Attrition ~ EnvironmentSatisfaction2 + EnvironmentSatisfaction3 +
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  WorkLifeBalance3 + BusinessTravelTravel_Frequently + DepartmentResearch...Development + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle +  NumCompaniesWorked5 + NumCompaniesWorked7 + NumCompaniesWorked9 +    
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears14.19 + TotalWorkingYears2.6 + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + 
                  DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours + JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_29)
vif(model_29)


# remove EnvironmentSatisfaction3 since it is related to EnvironmentSatisfaction4
model_30 <- glm(Attrition ~ EnvironmentSatisfaction2 + EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  WorkLifeBalance3 + BusinessTravelTravel_Frequently + DepartmentResearch...Development + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle +  NumCompaniesWorked5 + NumCompaniesWorked7 + NumCompaniesWorked9 +    
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears14.19 + TotalWorkingYears2.6 + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + 
                  DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours + JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_30)
vif(model_30)


# remove EnvironmentSatisfaction2 since it became insignificant
model_31 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + JobSatisfaction4 + 
                  WorkLifeBalance3 + BusinessTravelTravel_Frequently + DepartmentResearch...Development + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle +  NumCompaniesWorked5 + NumCompaniesWorked7 + NumCompaniesWorked9 +    
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears14.19 + TotalWorkingYears2.6 + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + 
                  DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours + JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_31)
vif(model_31)


# remove JobSatisfaction3 since it is related to JobSatisfaction4
model_32 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction4 + 
                  WorkLifeBalance3 + BusinessTravelTravel_Frequently + DepartmentResearch...Development + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle +  NumCompaniesWorked5 + NumCompaniesWorked7 + NumCompaniesWorked9 +    
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears14.19 + TotalWorkingYears2.6 + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + 
                  DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours + JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_32)
vif(model_32)


# remove JobSatisfaction2 since it became insignificant
model_33 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + 
                  WorkLifeBalance3 + BusinessTravelTravel_Frequently + DepartmentResearch...Development + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle +  NumCompaniesWorked5 + NumCompaniesWorked7 + NumCompaniesWorked9 +    
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears10 + 
                  TotalWorkingYears14.19 + TotalWorkingYears2.6 + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + 
                  DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours + JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_33)
vif(model_33)


# remove TotalWorkingYears10 since it is corelated to TotalWorkingYears2.6
model_34 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle +  NumCompaniesWorked5 + NumCompaniesWorked7 + NumCompaniesWorked9 +    
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears14.19 + 
                  TotalWorkingYears2.6 + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + 
                  DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours + JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_34)
vif(model_34)


# remove TotalWorkingYears14.19 since it became insignificant
model_35 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle +  NumCompaniesWorked5 + NumCompaniesWorked7 + NumCompaniesWorked9 +    
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears2.6 + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + 
                  DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours + JobRoleLaboratory.Technician, family = "binomial", data = train)

summary(model_35)
vif(model_35)


# remove JobRoleLaboratory.Technician since it became insignificant
model_36 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle +  NumCompaniesWorked5 + NumCompaniesWorked7 + NumCompaniesWorked9 +    
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears2.6 + YearsAtCompany12.14 + YearsAtCompany3.5 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + 
                  DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours , family = "binomial", data = train)

summary(model_36)
vif(model_36)


# remove YearsAtCompany3.5 since it is related to YearsAtCompany6.11
model_37 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development + 
                  JobLevel5 + JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle +  NumCompaniesWorked5 + NumCompaniesWorked7 + NumCompaniesWorked9 +    
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears2.6 + 
                  YearsAtCompany12.14 + YearsAtCompany6.11 + 
                  YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + 
                  DistanceFromHome4.7 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours , family = "binomial", data = train)

summary(model_37)
vif(model_37)


# remove DistanceFromHome4.7 since it became insignificant
model_38 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development + JobLevel5 + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle +  NumCompaniesWorked5 + NumCompaniesWorked7 + NumCompaniesWorked9 +    
                  TrainingTimesLastYear6 + PercentSalaryHike23. + TotalWorkingYears2.6 + YearsAtCompany12.14 + 
                  YearsAtCompany6.11 + YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + 
                  Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours , family = "binomial", data = train)

summary(model_38)
vif(model_38)


# remove NumCompaniesWorked9 since it became insignificant
model_39 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development + JobLevel5 + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle +  NumCompaniesWorked5 + NumCompaniesWorked7 + TrainingTimesLastYear6 + 
                  PercentSalaryHike23. + TotalWorkingYears2.6 + YearsAtCompany12.14 + 
                  YearsAtCompany6.11 + YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + 
                  Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours , family = "binomial", data = train)

summary(model_39)
vif(model_39)


# remove TotalWorkingYears2.6 since it is related to YearsAtCompany6.11
model_40 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development + JobLevel5 + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle +  NumCompaniesWorked5 + NumCompaniesWorked7 + TrainingTimesLastYear6 + 
                  PercentSalaryHike23. + YearsAtCompany12.14 + 
                  YearsAtCompany6.11 + YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + DistanceFromHome15.19 + 
                  Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours , family = "binomial", data = train)

summary(model_40)
vif(model_40)


# remove JobRoleResearch.Director since it became insignificant
model_41 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development + JobLevel5 + 
                  JobRoleResearch.Scientist + JobRoleSales.Executive + MaritalStatusSingle +
                  NumCompaniesWorked5 + NumCompaniesWorked7 + TrainingTimesLastYear6 + PercentSalaryHike23. + 
                  YearsAtCompany12.14 + YearsAtCompany6.11 + YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + 
                  DistanceFromHome15.19 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours , family = "binomial", data = train)

summary(model_41)
vif(model_41)


# remove JobRoleSales.Executive since it became insignificant
model_42 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development + JobLevel5 + 
                  JobRoleResearch.Scientist + MaritalStatusSingle +
                  NumCompaniesWorked5 + NumCompaniesWorked7 + TrainingTimesLastYear6 + PercentSalaryHike23. + 
                  YearsAtCompany12.14 + YearsAtCompany6.11 + YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + 
                  DistanceFromHome15.19 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours , family = "binomial", data = train)

summary(model_42)
vif(model_42)


# remove JobRoleResearch.Scientist since it became insignificant
model_43 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development + JobLevel5 + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7 + TrainingTimesLastYear6 + PercentSalaryHike23. + 
                  YearsAtCompany12.14 + YearsAtCompany6.11 + YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + 
                  DistanceFromHome15.19 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours , family = "binomial", data = train)

summary(model_43)
vif(model_43)


# remove YearsAtCompany12.14 since it became insignificant
model_44 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development + JobLevel5 + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7 + TrainingTimesLastYear6 + PercentSalaryHike23. + 
                  YearsAtCompany6.11 + YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + 
                  DistanceFromHome15.19 + Age33.38 + Age39.41 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours , family = "binomial", data = train)

summary(model_44)
vif(model_44)


# remove Age39.41 since it is related to Age33.38
model_45 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development + JobLevel5 + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7 + TrainingTimesLastYear6 + PercentSalaryHike23. + 
                  YearsAtCompany6.11 + YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + 
                  DistanceFromHome15.19 + Age33.38 + Age42.45 + Age46.50 + Age50.57 + Age58. +
                  ActualWorkingHours , family = "binomial", data = train)

summary(model_45)
vif(model_45)


# remove Age50.57 since it is related to Age33.38
model_46 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development + JobLevel5 + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7 + TrainingTimesLastYear6 + PercentSalaryHike23. + 
                  YearsAtCompany6.11 + YearsSinceLastPromotion3.7 + YearsWithCurrManager12. + 
                  DistanceFromHome15.19 + Age33.38 + Age42.45 + Age46.50 + Age58. +
                  ActualWorkingHours , family = "binomial", data = train)

summary(model_46)
vif(model_46)


# remove YearsSinceLastPromotion3.7 since it became insignificant
model_47 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development + JobLevel5 + 
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7 + TrainingTimesLastYear6 + PercentSalaryHike23. + 
                  YearsAtCompany6.11 + YearsWithCurrManager12. + 
                  DistanceFromHome15.19 + Age33.38 + Age42.45 + Age46.50 + Age58. +
                  ActualWorkingHours , family = "binomial", data = train)

summary(model_47)
vif(model_47)

# remove JobLevel5 since it became insignificant
model_48 <- glm(Attrition ~ EnvironmentSatisfaction4 + JobSatisfaction4 + WorkLifeBalance3 + 
                  BusinessTravelTravel_Frequently + DepartmentResearch...Development +  
                  MaritalStatusSingle + NumCompaniesWorked5 + NumCompaniesWorked7 + TrainingTimesLastYear6 + PercentSalaryHike23. + 
                  YearsAtCompany6.11 + YearsWithCurrManager12. + 
                  DistanceFromHome15.19 + Age33.38 + Age42.45 + Age46.50 + Age58. +
                  ActualWorkingHours , family = "binomial", data = train)

summary(model_48)
vif(model_48)


final_model <- model_48


########################## Model Evaluation ###########################


# predicted probabilities of Churn 1 for test data
test_pred = predict(final_model, test[,-1],type = "response")
summary(test_pred)
test$prob <- test_pred

# probability greaer than .5 is 1 (employee will leave)
test_pred_attrition_50 <- factor(ifelse(test_pred >= 0.50, 1,0))


# confusion matrix
test_conf_50 <- caret::confusionMatrix(test_pred_attrition_50, test$Attrition, positive = '1')

# Sensitivity : 0.15455       
# Specificity : 0.98224       
# Accuracy : 0.8411        
test_conf_50


# compute optimal probalility cutoff for better model reliability
perform_fn <- function(cutoff) 
{
  pred_attrition <- factor(ifelse(test_pred >= cutoff, 1,0))
  conf <- caret::confusionMatrix(pred_attrition, test$Attrition, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.
prob_seq = seq(.01,.85,length=100)
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
# value: .16
cutoff <- prob_seq[which(abs(OUT[,1]-OUT[,2])<0.01)]


# probability greaer than .16 is 1 (employee will leave)
test_pred_attrition <- factor(ifelse(test_pred >= 0.17, 1,0))


# confusion matrix
test_conf <- caret::confusionMatrix(test_pred_attrition, test$Attrition, positive = "1")

# Accuracy : 0.7155  
# Sensitivity : 0.7364      
# Specificity : 0.7112 
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