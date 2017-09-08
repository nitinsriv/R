
#--------------------------XYZ Company----------------------------#
  
  # Total Number of employees 4000
  # 15% leave every year - 600 employees in total
  
  # Current thoughts on why attrition is bad:
  
  # 1. Project delays leading to missing timelines hence loss of goodwill
  #    in front of consumers and partners.
  # 2. Having to maintain a sizeable HR team for recruitment all the time.
  # 3. New employees need to be trained to be job ready
  
# Scope of work

# 1. XYZ wants to know what changes they should make to their workplace so that most of their employees stay.
# 2. Also from the given variables they want to know which one is the most important that needs to be addressed.

# Table of Content

# Chapter 1 (Merging of Datasets)
# Chapter 2 (Understanding the variables)
# Chapter 3 (Data Manipulation)
# Chapter 4 (Creating the Model)
# Chapter 5 (Validating the Model)
#----------------------------------------------------------------#

# Loading the required library 

library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library("MASS")
library(car)
library(caret)
library(ROCR)

#--------------------------------------------------#
# read all data into R

emp_survey <- read.csv("C:\\IIITB\\HR Analytics Case Study\\PA-I_Case_Study_HR_Analytics\\employee_survey_data.csv")
gen_data <- read.csv("C:\\IIITB\\HR Analytics Case Study\\PA-I_Case_Study_HR_Analytics\\general_data.csv")
in_time <- read.csv("C:\\IIITB\\HR Analytics Case Study\\PA-I_Case_Study_HR_Analytics\\in_time.csv", stringsAsFactors=F,header=F)
mgr_survey <- read.csv("C:\\IIITB\\HR Analytics Case Study\\PA-I_Case_Study_HR_Analytics\\manager_survey_data.csv")
out_time <- read.csv("C:\\IIITB\\HR Analytics Case Study\\PA-I_Case_Study_HR_Analytics\\out_time.csv", stringsAsFactors=F,header=F)



# Adding "IN" to dates present in first row of in_time and "OUT"
# to dates present in first row of out_time dataset's respectively 
in_char <- "IN"
in_time[1,] <- sapply(in_time[1,], function(x) x <- paste(x,in_char,sep="_"))

out_char <- "OUT"
out_time[1,] <- sapply(out_time[1,], function(x) x <- paste(x,out_char,sep="_"))



# Making the first row of in_time and out_time dataset's as the header
# and removing the first row from each of the dataset's
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



# Storing the employee id from in_time dataset to a new variable diff_hours
diff_hours <- as.numeric(in_time$EmployeeID)



# Creating a function to calculate the difference in time between in_time
# and out_time dataset's, using strptime to convert to required date format
# Function will give the time difference and then merge it with the employee id
# stored in diff_hours variable

for (i in 2:250){
  act_workHours <- as.numeric(difftime(strptime(out_time[,i],"%Y-%m-%d %H:%M:%S"),
                                       strptime(in_time[,i],"%Y-%m-%d %H:%M:%S")))
  
  diff_hours <- cbind(diff_hours,act_workHours)
}
diff_hours <- as.data.frame(diff_hours)

# Renaming the first column of diff_hours dataframe to employee id
colnames(diff_hours)[1] <- 'EmployeeID'

# Finding average of all the columns except the employee id column
diff_hours$ActualWorkingHours <- apply(diff_hours[,-1],1,function(x) mean(x,na.rm=TRUE))

# Taking only the employee id and actual working hours columns to form a new
# dataframe that hold the average working hours of each employee
actual_workHours <- diff_hours[,c('EmployeeID','ActualWorkingHours')]



# notice number of rows in EmployeeID column for dataframes  - 4410.
length(unique(emp_survey$EmployeeID)) # confirm EmployeeID can be a key to merge different dataframe
length(unique(gen_data$EmployeeID)) # confirm EmployeeID can be a key to merge different dataframe
length(unique(mgr_survey$EmployeeID)) # confirm EmployeeID can be a key to merge different dataframe
length(unique(in_time$EmployeeID)) # confirm EmployeeID can be a key to merge different dataframe
length(unique(out_time$EmployeeID)) # confirm EmployeeID can be a key to merge different dataframe



# check if all values of employeeID are same in all dataframes
setdiff(emp_survey$EmployeeID,gen_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(gen_data$EmployeeID,in_time$EmployeeID) # Identical customerID across these datasets
setdiff(in_time$EmployeeID,mgr_survey$EmployeeID) # Identical customerID across these datasets
setdiff(mgr_survey$EmployeeID,out_time$EmployeeID) # Identical customerID across these datasets



# merge into single dataframe, joined by EmployeeID values.
emp_ef <- merge(emp_survey,gen_data,by="EmployeeID", all = F)
emp_ef <- merge(emp_ef,mgr_survey,by="EmployeeID", all = F)


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


#-----------Understanding the variables-----------#

#-------------A.Univariate Analysis---------------#

#Understanding the variables EnvironmentSatisfaction & Attrition  
env_sat_attrition <- ggplot(emp_ef,aes(emp_ef$EnvironmentSatisfaction ,fill=factor(emp_ef$Attrition)))
labels_env_sat <- labs(x = "Survey Buckets", y = "No. of Employees", title = "Environment Satisfaction Distribution", fill = "Attrition")
env_sat_attrition_final <- env_sat_attrition + labels_env_sat + geom_bar(position = "dodge")

#Understanding the variables JobSatisfaction & Attrition
Job_sat_attrition <- ggplot(emp_ef,aes(emp_ef$JobSatisfaction ,fill=factor(emp_ef$Attrition)))
labels_job_sat <- labs(x = "Survey Buckets", y = "No. of Employees", title = "Job Satisfaction Distribution", fill = "Attrition")
job_sat_attrition_final <- Job_sat_attrition + labels_job_sat + geom_bar(position = "dodge")

#Understanding the variables WorkLifeBalance & Attrition
work_life_attrition <- ggplot(emp_ef,aes(emp_ef$WorkLifeBalance ,fill=factor(emp_ef$Attrition)))
labels_work_life <- labs(x = "Survey Buckets", y = "No. of Employees", title = "Worklife Balance Distribution", fill = "Attrition")
work_life_attrition_final <- work_life_attrition + labels_work_life + geom_bar(position = "dodge")

#Understanding the variables Age & Attrition
age_attrition <- ggplot(emp_ef,aes(emp_ef$Age ,fill=factor(emp_ef$Attrition)))
labels_age <- labs(x = "Age", y = "No. of Employees", title = "Age Distribution", fill = "Attrition")
age_attrition_final <- age_attrition + labels_age + geom_bar(position = "dodge")

#Understanding the variables BusinessTravel & Attrition
travel_attrition <- ggplot(emp_ef,aes(emp_ef$BusinessTravel ,fill=factor(emp_ef$Attrition)))
labels_travel <- labs(x = "Travel", y = "No. of Employees", title = "Business Travel Distribution", fill = "Attrition")
travel_attrition_final <- travel_attrition + labels_travel + geom_bar(position = "dodge")

#Understanding the variables Department & Attrition
dept_attrition <- ggplot(emp_ef,aes(emp_ef$Department ,fill=factor(emp_ef$Attrition)))
labels_dept <- labs(x = "Departments", y = "No. of Employees", title = "Department Distribution", fill = "Attrition")
dept_attrition_final <- dept_attrition + labels_dept + geom_bar(position = "dodge")

#Understanding the variables EducationField & Attrition
edu_attrition <- ggplot(emp_ef,aes(emp_ef$EducationField ,fill=factor(emp_ef$Attrition)))
labels_edu <- labs(x = "Education", y = "No. of Employees", title = "Education Level Distribution", fill = "Attrition")
edu_attrition_final <- edu_attrition + labels_edu + geom_bar(position = "dodge")

#Understanding the variables Gender & Attrition
gender_attrition <- ggplot(emp_ef,aes(emp_ef$Gender ,fill=factor(emp_ef$Attrition)))
labels_gender <- labs(x = "Gender", y = "No. of Employees", title = "Gender Distribution", fill = "Attrition")
gender_attrition_final <- gender_attrition + labels_gender + geom_bar(position = "dodge")

#Understanding the variables JobLevel & Attrition
job_level_attrition <- ggplot(emp_ef,aes(emp_ef$JobLevel ,fill=factor(emp_ef$Attrition)))
labels_job_level <- labs(x = "Job Level", y = "No. of Employees", title = "Job Level Distribution", fill = "Attrition")
job_level_attrition_final <- job_level_attrition + labels_job_level + geom_bar(position = "dodge")

#Understanding the variables JobRole & Attrition
job_role_attrition <- ggplot(emp_ef,aes(emp_ef$Attrition ,fill=factor(emp_ef$JobRole)))
labels_job_role <- labs(x = "Attrition", y = "No. of Employees", title = "Job Role Distribution", fill = "Attrition")
job_role_attrition_final <- job_role_attrition + labels_job_role + geom_bar(position = "dodge")

#Understanding the variables MaritalStatus & Attrition
married_attrition <- ggplot(emp_ef,aes(emp_ef$MaritalStatus ,fill=factor(emp_ef$Attrition)))
labels_married <- labs(x = "Marital Status", y = "No. of Employees", title = "Marital Status Distribution", fill = "Attrition")
married_attrition_final <- married_attrition + labels_married + geom_bar(position = "dodge")

#Understanding the variables NumCompaniesWorked & Attrition
comp_worked_attrition <- ggplot(emp_ef,aes(emp_ef$NumCompaniesWorked ,fill=factor(emp_ef$Attrition)))
labels_comp_worked <- labs(x = "Years Of Experience", y = "No. of Employees", title = "Companies Worked Distribution", fill = "Attrition")
comp_worked_attrition_final <- comp_worked_attrition + labels_comp_worked + geom_histogram(binwidth = 1)

#Understanding the variables StockOptionLevel & Attrition
stock_opt_attrition <- ggplot(emp_ef,aes(emp_ef$StockOptionLevel ,fill=factor(emp_ef$Attrition)))
labels_stock_opt <- labs(x = "Stock Option", y = "No. of Employees", title = "Stock Option Distribution", fill = "Attrition")
stock_opt_attrition_final <- stock_opt_attrition + labels_stock_opt + geom_bar(position = "dodge")


#---------------B.Segmented Analysis------------------#

# Section_1.Understanding the impact of Attrition as "Yes" on each variable
# Section_2.Impact of missing value "NA" on the overall dataset

#---------------------Section_1-----------------------#
# Finding the actual number of employees for whom attrition is 'Yes' (who left)
emp_left <- emp_ef[which(emp_ef$Attrition == "Yes"),]

# 711 employees actually left the organization
no_attrition <- length(which(emp_ef$Attrition == "Yes"))

#-----------------------------------------------------#
# creating the plots to understand the distribution of people who
# left the organization, where attrition = "Yes"
plot_grid(ggplot(emp_left, aes(x=EnvironmentSatisfaction))+ geom_bar(fill = "orange"), 
          ggplot(emp_left, aes(x=JobSatisfaction))+ geom_bar(fill = "orange"),
          ggplot(emp_left, aes(x=WorkLifeBalance))+ geom_bar(fill = "orange"),
          ggplot(emp_left, aes(x=Age))+ geom_bar(fill = "orange"),
          ggplot(emp_left, aes(x=BusinessTravel))+ geom_bar(fill = "orange"),
          ggplot(emp_left, aes(x=Gender))+ geom_bar(fill = "orange"),
          align = "h")

plot_grid(ggplot(emp_left, aes(x=StockOptionLevel))+ geom_bar(fill = "orange"), 
          ggplot(emp_left, aes(x=Department))+ geom_bar(fill = "orange"),
          ggplot(emp_left, aes(x=JobLevel))+ geom_bar(fill = "orange"),
          ggplot(emp_left, aes(x=MaritalStatus))+ geom_bar(fill = "orange"),
          align = "h")

ggplot(emp_left, aes(x=JobRole))+ geom_bar(fill = "orange")
ggplot(emp_left, aes(x=EducationField))+ geom_bar(fill = "orange")
ggplot(emp_left, aes(x=NumCompaniesWorked))+ geom_bar(fill = "orange")



#---------------------Section_2-----------------------#
# Finding the number of columns containing NA with number of NA
sapply(emp_ef, function(x) sum(is.na(x)))
# We see that EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance
# NumCompaniesWorked & TotalWorkingYears have 25, 20, 38, 19, 9 NA's respectively

# We will look into these five variables now.

#-----------------------------------------------------#
# For EnvironmentSatisfaction, No. of NA found are 25, ignoring them.
# No. of NA is very less compared to those who have left the organization
# Removing NA will not have any drastic impact on overall result.

# Creating a subset where EnvironmentSatisfaction = NA
env_na <- emp_ef[(which(is.na(emp_ef$EnvironmentSatisfaction) == TRUE)),] 

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(env_na, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(),
          ggplot(env_na, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(),
          ggplot(env_na, aes(x=Age,fill=Attrition))+ geom_bar(),
          ggplot(env_na, aes(x=BusinessTravel,fill=Attrition))+ geom_bar() + bar_theme1,
          ggplot(env_na, aes(x=Department,fill=Attrition))+ geom_bar(),
          ggplot(env_na, aes(x=Gender,fill=Attrition))+ geom_bar(),
          align = "h")

plot_grid(ggplot(env_na, aes(x=EducationField,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(env_na, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(),
          ggplot(env_na, aes(x=JobLevel,fill=Attrition))+ geom_bar(),
          ggplot(env_na, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(env_na, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(),
          ggplot(env_na, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar(),
          align = "h")

#------------------------------------------------------#
# For JobSatisfaction, No. of NA found are 20, ignoring them.
# No. of NA is very less compared to those who have left the organization
# Removing NA will not have any drastic impact on overall result.

# Creating a subset where JobSatisfaction = NA
job_na <- emp_ef[(which(is.na(emp_ef$JobSatisfaction) == TRUE)),]

plot_grid(ggplot(job_na, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(), 
          ggplot(job_na, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(),
          ggplot(job_na, aes(x=Age,fill=Attrition))+ geom_bar(),
          ggplot(job_na, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(job_na, aes(x=Department,fill=Attrition))+ geom_bar(),
          ggplot(job_na, aes(x=Gender,fill=Attrition))+ geom_bar(),
          align = "h")


plot_grid(ggplot(job_na, aes(x=EducationField,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(job_na, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(),
          ggplot(job_na, aes(x=JobLevel,fill=Attrition))+ geom_bar(),
          ggplot(job_na, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(job_na, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(),
          ggplot(job_na, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar(),
          align = "h")

#------------------------------------------------------#
# For WorkLifeBalance, No. of NA found are 38, ignoring them.
# No. of NA is very less compared to those who have left the organization
# Removing NA will not have any drastic impact on overall result.

# Creating a subset where WorkLifeBalance = NA
worklifbalance_na <- emp_ef[(which(is.na(emp_ef$WorkLifeBalance) == TRUE)),]

plot_grid(ggplot(worklifbalance_na, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(), 
          ggplot(worklifbalance_na, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(),
          ggplot(worklifbalance_na, aes(x=Age,fill=Attrition))+ geom_bar(),
          ggplot(worklifbalance_na, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(worklifbalance_na, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(worklifbalance_na, aes(x=Gender,fill=Attrition))+ geom_bar(),
          align = "h")


plot_grid(ggplot(worklifbalance_na, aes(x=EducationField,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(worklifbalance_na, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(),
          ggplot(worklifbalance_na, aes(x=JobLevel,fill=Attrition))+ geom_bar(),
          ggplot(worklifbalance_na, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(worklifbalance_na, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(),
          ggplot(worklifbalance_na, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar(),
          align = "h")

#------------------------------------------------------#
# For NumCompaniesWorked, No. of NA found are 19, ignoring them.
# No. of NA is very less compared to those who have left the organization
# Removing NA will not have any drastic impact on overall result.

# Creating a subset where NumCompaniesWorked = NA
numCompaniesWorked_na <- emp_ef[(which(is.na(emp_ef$NumCompaniesWorked) == TRUE)),]

plot_grid(ggplot(numCompaniesWorked_na, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(), 
          ggplot(numCompaniesWorked_na, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(),
          ggplot(numCompaniesWorked_na, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(),
          ggplot(numCompaniesWorked_na, aes(x=Age,fill=Attrition))+ geom_bar(),
          ggplot(numCompaniesWorked_na, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(numCompaniesWorked_na, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")


plot_grid(ggplot(numCompaniesWorked_na, aes(x=EducationField,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(numCompaniesWorked_na, aes(x=Gender,fill=Attrition))+ geom_bar(),
          ggplot(numCompaniesWorked_na, aes(x=JobLevel,fill=Attrition))+ geom_bar(),
          ggplot(numCompaniesWorked_na, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(numCompaniesWorked_na, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(),
          ggplot(numCompaniesWorked_na, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(),
          align = "h")

#-------------------------------------------------------#
  
########################## Missing Value Imputation ##########################

# find columns containing NA with number of NA
sapply(emp_ef, function(x) sum(is.na(x)))

# assign numeric levels 1 and 0 to Attrition column
levels(emp_ef$Attrition) <-c(0,1)
emp_ef$Attrition <- as.numeric(levels(emp_ef$Attrition))[emp_ef$Attrition]

# create WOE and IV table for all variables
IV <- create_infotables(emp_ef[,-1], y="Attrition", bins=10, parallel=FALSE)



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
y <- IV$Tables$EnvironmentSatisfaction$WOE[2:5]
ggplot(IV$Tables$EnvironmentSatisfaction[-1,],aes(x,y))+geom_smooth()


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


######################### outliner treatment ##############################

# outliner check for MonthlyIncome
quantile(emp_no_na$MonthlyIncome,seq(0,1,.01))
# jump at 90% to 91%, replacing all greater than 137756.0 with 137756.0
emp_no_na$MonthlyIncome[which(emp_no_na$MonthlyIncome>137756.0)] <- 137756.0


# binning values of Totalworkingyears based on WOE shown above
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

# binning values of YearsAtCompany
# YearsAtCompany   N    Percent         WOE         IV
#1          [0,0] 132 0.02993197  1.08952957 0.04881279
#2          [1,1] 513 0.11632653  1.00818393 0.20821653
#3          [2,2] 381 0.08639456  0.33981204 0.21936952
#4          [3,4] 714 0.16190476  0.01940218 0.21943087
#5          [5,6] 816 0.18503401 -0.43859498 0.24999523
#6          [7,8] 510 0.11564626 -0.36575766 0.26362567
#7          [9,9] 246 0.05578231 -0.57547819 0.27873827
#8        [10,14] 624 0.14149660 -0.38773657 0.29733555
#9        [15,40] 474 0.10748299 -0.68161061 0.33667579

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


temp_yrs <- emp_no_na$YearsAtCompany
emp_no_na$YearsAtCompany[which(temp_yrs>=0 & temp_yrs<=1)] <- '0-1'
emp_no_na$YearsAtCompany[which(temp_yrs>=3 & temp_yrs<=4)] <- '3-4'
emp_no_na$YearsAtCompany[which(temp_yrs>=5 & temp_yrs<=8)] <- '5-8'
emp_no_na$YearsAtCompany[which(temp_yrs>=9 & temp_yrs<=14)] <- '9-14'


# replace all values greater than 15 years with 15+ years 
emp_no_na$YearsAtCompany[which(temp_yrs>=15)] <- '15+'



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


# for coarse classing, compute WOE 1 to 3 for binning
YearsSinceLastPromotion_123 <- emp_no_na[which(emp_no_na$YearsSinceLastPromotion==1 | emp_no_na$YearsSinceLastPromotion==2 |
                                         emp_no_na$YearsSinceLastPromotion==3),c(23,6)]
loc_good <- length(YearsSinceLastPromotion_123$Attrition[which(YearsSinceLastPromotion_123$Attrition==1)])
loc_bad <- length(YearsSinceLastPromotion_123$Attrition[which(YearsSinceLastPromotion_123$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.09


# for coarse classing, compute WOE 4 to 15 for binning
YearsSinceLastPromotion_4_15 <- emp_no_na[which(emp_no_na$YearsSinceLastPromotion==4 | emp_no_na$YearsSinceLastPromotion==5 |
                                                  emp_no_na$YearsSinceLastPromotion==6 | emp_no_na$YearsSinceLastPromotion==7 |
                                                emp_no_na$YearsSinceLastPromotion==8|emp_no_na$YearsSinceLastPromotion==9|
                                                  emp_no_na$YearsSinceLastPromotion==10| emp_no_na$YearsSinceLastPromotion==11 |
                                                  emp_no_na$YearsSinceLastPromotion==12 | emp_no_na$YearsSinceLastPromotion==13 |
                                                  emp_no_na$YearsSinceLastPromotion==14 | emp_no_na$YearsSinceLastPromotion==15 ),c(23,6)]
loc_good <- length(YearsSinceLastPromotion_4_15$Attrition[which(YearsSinceLastPromotion_4_15$Attrition==1)])
loc_bad <- length(YearsSinceLastPromotion_4_15$Attrition[which(YearsSinceLastPromotion_4_15$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.24


temp_yrsPromotion <- emp_no_na$YearsSinceLastPromotion
emp_no_na$YearsSinceLastPromotion[which(temp_yrsPromotion>=1 & temp_yrsPromotion<=3)] <- '1-3'

# replace all values greater than 11 years with 4+ years 
emp_no_na$YearsSinceLastPromotion[which(temp_yrsPromotion>=4)] <- '4+'


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


# for coarse classing, comvine 1 and 2 ro make WOE tend monotonic
YearsWithCurrManager_12 <- emp_no_na[which(emp_no_na$YearsWithCurrManager==1 | 
                                             emp_no_na$YearsWithCurrManager==2),c(24,6)]
loc_good <- length(YearsWithCurrManager_12$Attrition[which(YearsWithCurrManager_12$Attrition==1)])
loc_bad <- length(YearsWithCurrManager_12$Attrition[which(YearsWithCurrManager_12$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.12

YearsWithCurrManager_4_8 <- emp_no_na[which(emp_no_na$YearsWithCurrManager==4 | emp_no_na$YearsWithCurrManager==5 | emp_no_na$YearsWithCurrManager==7 |
                                             emp_no_na$YearsWithCurrManager==6 |emp_no_na$YearsWithCurrManager==8),c(24,6)]
loc_good <- length(YearsWithCurrManager_4_8$Attrition[which(YearsWithCurrManager_4_8$Attrition==1)])
loc_bad <- length(YearsWithCurrManager_4_8$Attrition[which(YearsWithCurrManager_4_8$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.30

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
#1           [11,11] 630 0.14285714 -0.14261411 0.002767159
#2           [12,12] 594 0.13469388 -0.07362124 0.003479133
#3           [13,13] 627 0.14217687  0.01071991 0.003495530
#4           [14,14] 603 0.13673469 -0.13105328 0.005741036
#5           [15,16] 537 0.12176871  0.08592029 0.006666333
#6           [17,18] 513 0.11632653  0.01850524 0.006706418
#7           [19,20] 393 0.08911565  0.10250835 0.007675652
#8           [21,25] 513 0.11632653  0.18040733 0.011696557


# for coarse classing, comvine 13 and 14 ro make WOE tend monotonic
PercentSalaryHike_13_14 <- emp_no_na[which(emp_no_na$PercentSalaryHike==13 | 
                                             emp_no_na$PercentSalaryHike==14),c(18,6)]
loc_good <- length(PercentSalaryHike_13_14$Attrition[which(PercentSalaryHike_13_14$Attrition==1)])
loc_bad <- length(PercentSalaryHike_13_14$Attrition[which(PercentSalaryHike_13_14$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.06


# for coarse classing, comvine 15 till 18 ro make WOE tend monotonic
PercentSalaryHike_15_18 <- emp_no_na[which(emp_no_na$PercentSalaryHike==15 | emp_no_na$PercentSalaryHike==16 | emp_no_na$PercentSalaryHike==17 |
                                        emp_no_na$PercentSalaryHike==18),c(18,6)]
loc_good <- length(PercentSalaryHike_15_18$Attrition[which(PercentSalaryHike_15_18$Attrition==1)])
loc_bad <- length(PercentSalaryHike_15_18$Attrition[which(PercentSalaryHike_15_18$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # .05


# binning values of PercentSalaryHike
temp_perHike <- emp_no_na$PercentSalaryHike
emp_no_na$PercentSalaryHike[which(temp_perHike>=13 & temp_perHike<=14)] <- '13-14'
emp_no_na$PercentSalaryHike[which(temp_perHike>=15 & temp_perHike<=18)] <- '15-18'
emp_no_na$PercentSalaryHike[which(temp_perHike>=19 & temp_perHike<=20)] <- '19-20'


# replace all values greater than 21 years with 21+  
emp_no_na$PercentSalaryHike[which(temp_perHike>=21)] <- '21+'


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

# for coarse classing, comvine 11 till 29 to make WOE tend monotonic
DistanceFromHome_11_29 <- emp_no_na[which(emp_no_na$DistanceFromHome>=11 & emp_no_na$DistanceFromHome<=29),c(9,6)]
loc_good <- length(DistanceFromHome_11_29$Attrition[which(DistanceFromHome_11_29$Attrition==1)])
loc_bad <- length(DistanceFromHome_11_29$Attrition[which(DistanceFromHome_11_29$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.02

# for coarse classing, comvine 3 till 10 to make WOE tend monotonic
DistanceFromHome_3_10 <- emp_no_na[which(emp_no_na$DistanceFromHome>=3 & emp_no_na$DistanceFromHome<=10),c(9,6)]
loc_good <- length(DistanceFromHome_3_10$Attrition[which(DistanceFromHome_3_10$Attrition==1)])
loc_bad <- length(DistanceFromHome_3_10$Attrition[which(DistanceFromHome_3_10$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.01

# for coarse classing, comvine 1 till 2 to make WOE tend monotonic
DistanceFromHome_12 <- emp_no_na[which(emp_no_na$DistanceFromHome>=1 & emp_no_na$DistanceFromHome<=2),c(9,6)]
loc_good <- length(DistanceFromHome_12$Attrition[which(DistanceFromHome_12$Attrition==1)])
loc_bad <- length(DistanceFromHome_12$Attrition[which(DistanceFromHome_12$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # .04

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


# for coarse classing, comvine 1 till 2 to make WOE tend monotonic
Age_26_33 <- emp_no_na[which(emp_no_na$Age>=26 & emp_no_na$Age<=33),c(5,6)]
loc_good <- length(Age_26_33$Attrition[which(Age_26_33$Attrition==1)])
loc_bad <- length(Age_26_33$Attrition[which(Age_26_33$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # .35


# for coarse classing, comvine 1 till 2 to make WOE tend monotonic
Age_ <- emp_no_na[which(emp_no_na$Age>=34 & emp_no_na$Age<=37),c(5,6)]
loc_good <- length(Age_$Attrition[which(Age_$Attrition==1)])
loc_bad <- length(Age_$Attrition[which(Age_$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.40


# for coarse classing, comvine 1 till 2 to make WOE tend monotonic
Age_ <- emp_no_na[which(emp_no_na$Age>=38 & emp_no_na$Age<=60),c(5,6)]
loc_good <- length(Age_$Attrition[which(Age_$Attrition==1)])
loc_bad <- length(Age_$Attrition[which(Age_$Attrition==0)])
combined_woe <- computeWoE(loc_good,loc_bad) # -.50

 
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

#This column should be removed from the newly created dummy_education dataframe.
dummy_education <- dummy_education[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-10], dummy_education)


# converting EnvironmentSatisfaction into factor.
# Converting "EnvironmentSatisfaction" into dummies . 
emp_no_na$EnvironmentSatisfaction <- as.factor(emp_no_na$EnvironmentSatisfaction)
dummy_EnvironmentSatisfaction <- data.frame(model.matrix( ~EnvironmentSatisfaction, data = emp_no_na))

#This column should be removed from the newly created dummy_EnvironmentSatisfaction dataframe.
dummy_EnvironmentSatisfaction <- dummy_EnvironmentSatisfaction[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-2], dummy_EnvironmentSatisfaction)


# converting JobSatisfaction into factor.
# Converting "JobSatisfaction" into dummies . 
emp_no_na$JobSatisfaction <- as.factor(emp_no_na$JobSatisfaction)
dummy_JobSatisfaction <- data.frame(model.matrix( ~JobSatisfaction, data = emp_no_na))

#This column should be removed from the newly created dummy_JobSatisfaction dataframe.
dummy_JobSatisfaction <- dummy_JobSatisfaction[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-2], dummy_JobSatisfaction)



# converting WorkLifeBalance into factor.
# Converting "WorkLifeBalance" into dummies . 
emp_no_na$WorkLifeBalance <- as.factor(emp_no_na$WorkLifeBalance)
dummy_WorkLifeBalance <- data.frame(model.matrix( ~WorkLifeBalance, data = emp_no_na))

#This column should be removed from the newly created dummy_WorkLifeBalance dataframe.
dummy_WorkLifeBalance <- dummy_WorkLifeBalance[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-2], dummy_WorkLifeBalance)


# converting BusinessTravel into factor.
# Converting "BusinessTravel" into dummies . 
emp_no_na$BusinessTravel <- as.factor(emp_no_na$BusinessTravel)
dummy_BusinessTravel <- data.frame(model.matrix( ~BusinessTravel, data = emp_no_na))

#This column should be removed from the newly created dummy_BusinessTravel dataframe.
dummy_BusinessTravel <- dummy_BusinessTravel[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-4], dummy_BusinessTravel)


# converting Department into factor.
# Converting "Department" into dummies . 
emp_no_na$Department <- as.factor(emp_no_na$Department)
dummy_Department <- data.frame(model.matrix( ~Department, data = emp_no_na))

#This column should be removed from the newly created dummy_Department dataframe.
dummy_Department <- dummy_Department[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-4], dummy_Department)


# converting EducationField into factor.
# Converting "EducationField" into dummies . 
emp_no_na$EducationField <- as.factor(emp_no_na$EducationField)
dummy_EducationField <- data.frame(model.matrix( ~EducationField, data = emp_no_na))

#This column should be removed from the newly created dummy_EducationField dataframe.
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

#This column should be removed from the newly created dummy_JobLevel dataframe.
dummy_JobLevel <- dummy_JobLevel[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-6], dummy_JobLevel)


# converting JobRole into factor.
# Converting "JobRole" into dummies . 
emp_no_na$JobRole <- as.factor(emp_no_na$JobRole)
dummy_JobRole <- data.frame(model.matrix( ~JobRole, data = emp_no_na))

#This column should be removed from the newly created dummy_JobRole dataframe.
dummy_JobRole <- dummy_JobRole[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-6], dummy_JobRole)


# converting MaritalStatus into factor.
# Converting "MaritalStatus" into dummies . 
emp_no_na$MaritalStatus <- as.factor(emp_no_na$MaritalStatus)
dummy_MaritalStatus <- data.frame(model.matrix( ~MaritalStatus, data = emp_no_na))

#This column should be removed from the newly created dummy_MaritalStatus dataframe.
dummy_MaritalStatus <- dummy_MaritalStatus[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-6], dummy_MaritalStatus)


# converting NumCompaniesWorked into factor.
# Converting "NumCompaniesWorked" into dummies . 
emp_no_na$NumCompaniesWorked <- as.factor(emp_no_na$NumCompaniesWorked)
dummy_NumCompaniesWorked <- data.frame(model.matrix( ~NumCompaniesWorked, data = emp_no_na))

#This column should be removed from the newly created dummy_NumCompaniesWorked dataframe.
dummy_NumCompaniesWorked <- dummy_NumCompaniesWorked[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-7], dummy_NumCompaniesWorked)


# converting StockOptionLevel into factor.
# Converting "StockOptionLevel" into dummies . 
emp_no_na$StockOptionLevel <- as.factor(emp_no_na$StockOptionLevel)
dummy_StockOptionLevel <- data.frame(model.matrix( ~StockOptionLevel, data = emp_no_na))

#This column should be removed from the newly created dummy_StockOptionLevel dataframe.
dummy_StockOptionLevel <- dummy_StockOptionLevel[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-8], dummy_StockOptionLevel)


# converting TrainingTimesLastYear into factor.
# Converting "TrainingTimesLastYear" into dummies . 
emp_no_na$TrainingTimesLastYear <- as.factor(emp_no_na$TrainingTimesLastYear)
dummy_TrainingTimesLastYear <- data.frame(model.matrix( ~TrainingTimesLastYear, data = emp_no_na))

#This column should be removed from the newly created dummy_TrainingTimesLastYear dataframe.
dummy_TrainingTimesLastYear <- dummy_TrainingTimesLastYear[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-9], dummy_TrainingTimesLastYear)


# converting JobInvolvement into factor.
# Converting "JobInvolvement" into dummies . 
emp_no_na$JobInvolvement <- as.factor(emp_no_na$JobInvolvement)
dummy_JobInvolvement <- data.frame(model.matrix( ~JobInvolvement, data = emp_no_na))

#This column should be removed from the newly created dummy_JobInvolvement dataframe.
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

#This column should be removed from the newly created dummy_PercentSalaryHike dataframe.
dummy_PercentSalaryHike <- dummy_PercentSalaryHike[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-7], dummy_PercentSalaryHike)


# converting TotalWorkingYears into factor.
# Converting "TotalWorkingYears" into dummies . 
emp_no_na$TotalWorkingYears <- as.factor(emp_no_na$TotalWorkingYears)
dummy_TotalWorkingYears <- data.frame(model.matrix( ~TotalWorkingYears, data = emp_no_na))

#This column should be removed from the newly created dummy_TotalWorkingYears dataframe.
dummy_TotalWorkingYears <- dummy_TotalWorkingYears[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-7], dummy_TotalWorkingYears)


# converting YearsAtCompany into factor.
# Converting "YearsAtCompany" into dummies . 
emp_no_na$YearsAtCompany <- as.factor(emp_no_na$YearsAtCompany)
dummy_YearsAtCompany <- data.frame(model.matrix( ~YearsAtCompany, data = emp_no_na))

#This column should be removed from the newly created dummy_YearsAtCompany dataframe.
dummy_YearsAtCompany <- dummy_YearsAtCompany[,-1]

emp_no_na <- cbind(emp_no_na[,-7], dummy_YearsAtCompany)



# converting YearsSinceLastPromotion into factor.
# Converting "YearsSinceLastPromotion" into dummies . 
emp_no_na$YearsSinceLastPromotion <- as.factor(emp_no_na$YearsSinceLastPromotion)
dummy_YearsSinceLastPromotion <- data.frame(model.matrix( ~YearsSinceLastPromotion, data = emp_no_na))

#This column should be removed from the newly created dummy_YearsSinceLastPromotion dataframe.
dummy_YearsSinceLastPromotion <- dummy_YearsSinceLastPromotion[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-7], dummy_YearsSinceLastPromotion)
# Combine the dummy variables to the main data set, after removing the original  column


# converting YearsWithCurrManager into factor.
# Converting "YearsWithCurrManager" into dummies . 
emp_no_na$YearsWithCurrManager <- as.factor(emp_no_na$YearsWithCurrManager)
dummy_YearsWithCurrManager <- data.frame(model.matrix( ~YearsWithCurrManager, data = emp_no_na))

#This column should be removed from the newly created dummy_YearsWithCurrManager dataframe.
dummy_YearsWithCurrManager <- dummy_YearsWithCurrManager[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-7], dummy_YearsWithCurrManager)


# converting DistanceFromHome into factor.
# Converting "DistanceFromHome" into dummies . 
emp_no_na$DistanceFromHome <- as.factor(emp_no_na$DistanceFromHome)
dummy_DistanceFromHome <- data.frame(model.matrix( ~DistanceFromHome, data = emp_no_na))

#This column should be removed from the newly created dummy_DistanceFromHome dataframe.
dummy_DistanceFromHome <- dummy_DistanceFromHome[,-1]

# Combine the dummy variables to the main data set, after removing the original  column
emp_no_na <- cbind(emp_no_na[,-4], dummy_DistanceFromHome)


# converting Age into factor.
# Converting "Age" into dummies . 
emp_no_na$Age <- as.factor(emp_no_na$Age)
dummy_Age <- data.frame(model.matrix( ~Age, data = emp_no_na))

#This column should be removed from the newly created dummy_Age dataframe.
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
model_27 <- glm(Attrition ~ EnvironmentSatisfaction0.56 + JobSatisfaction0.43 + WorkLifeBalance0.87 + 
                  BusinessTravelTravel_Frequently + 
                  JobLevel2 + JobRoleManufacturing.Director + JobRoleResearch.Director + 
                  MaritalStatusSingle +  NumCompaniesWorked0.36 + TrainingTimesLastYear6 + 
                  JobInvolvement3 + TotalWorkingYears10.12 + 
                  TotalWorkingYears17.22 + YearsAtCompany5.8 + YearsWithCurrManager1.2 + 
                  YearsWithCurrManager3 + Age38. + ActualWorkingHours,
                family = "binomial", data = train)
summary(model_27)
vif(model_27)


# Variables affexting attrition of an employee:
# EnvironmentSatisfaction0.56 <- EnvironmentSatisfaction 'Low'
# JobSatisfaction0.43 <- JobSatisfaction 'Low'
# WorkLifeBalance0.87 <- WorkLifeBalance 'Low'
# BusinessTrave Frequently
# JobLevel at scale 2
# JobRole Manufacturing Director
# Jobrole Research Director
# Marital Status Single
# NumCompaniesWorked 0.36- those who have not worked in any company before
# TrainingTimesLastYear6 <- 6 trainings conducted for emmployee last year
# JobInvolvement3 <- level of involvement in job is 'High'
# TotalWorkingYears10.12 <- Total Working Years is 10-12
# TotalWorkingYears17.22 <- Total Working Years is 17-22
# YearsAtCompany5.8 <- Years at company spent 5-8
# YearsWithCurrManager1.2 <- years with current manager 1-2
# YearsWithCurrManager3 <- years with current manager 3
# Age38. - Age 38 and above
# ActualWorkingHours


final_model <- model_27


########################## Model Evaluation ###########################

# predicted probabilities of Attrition for test data
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
prob_seq = seq(.002,.87,length=100)
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
test_pred_attrition <- factor(ifelse(test_pred >= 0.19489, 1,0))


# confusion matrix with cutoff value equal 0.19489
test_conf <- confusionMatrix(test_pred_attrition, test$Attrition, positive = "1")

#Accuracy : 0.7396 
#Sensitivity : 0.7317          
# Specificity : 0.7410 
test_conf



########################## ROC Curve ############

test_pred_numeric <- as.numeric(test_pred_attrition)
pred_object_test<- prediction(test_pred_numeric, test$Attrition)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)

# plot ROC curve
plot(performance_measures_test)
abline(0,1,col="grey")



################ KS- Statistic table and Lift Chart #######################

test1 <- test[,c(1,grep("prob", colnames(test)))]
test1 <- test1[order(-test1[,2],test1[,1]),]

gainchart <- data.frame(Decile=integer(),AttritionCount=integer(),CumulativeAttrition=integer(),GainPercent=integer(),NonAttritionCount=integer(),CumulativeNonAttrition=integer(),GainPercentNonAttrition=integer(),KS_stat=integer())
gainchart[c(1:10),] <- 0
strt<-1
stp<-nrow(test1)/10
obs<-nrow(test1)/10
for (i in 1:10){
  gainchart[i,1]  <- i
  gainchart[i,2]  <- sum(as.numeric(as.character(test1[c(strt:stp), 1])))
  ifelse ((i>=2), gainchart[i,3] <- gainchart[i-1,3] + gainchart[i,2], gainchart[i,3] <- gainchart[i,2])
  gainchart[i,5]  <- obs - gainchart[i,2]
  ifelse ((i>=2), gainchart[i,6] <- gainchart[i-1,6] + gainchart[i,5], gainchart[i,6] <- gainchart[i,5])
  strt <- strt + nrow(test1)/10
  stp <- stp + nrow(test1)/10
}
totalattri <- sum(gainchart$AttritionCount)
totalnonattri <- sum(gainchart$NonAttritionCount)
for (i in 1:10){
  gainchart[i,4]  <- gainchart[i,3]/totalattri*100
  gainchart[i,7]  <- gainchart[i,6]/totalnonattri*100
  gainchart[i,8]  <- gainchart[i,4] - gainchart[i,7]
}
gainchart$GainRandomModel <- c(10,20,30,40,50,60,70,80,90,100)
gainchart$lift <- gainchart$GainPercent/gainchart$GainRandomModel
view(gainchart)

# draw lift chart
ggplot(gainchart, aes(Decile)) + geom_line(aes(y = 1, colour = "GainRandomModel")) + 
  geom_line(aes(y = lift, colour = "lift")) +geom_point(aes(y=1)) + geom_point(aes(y = lift, colour = "lift"))
