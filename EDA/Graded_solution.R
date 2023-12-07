#################Graded Question Solution###################
############################################################
# Loading dataset 
grades <- read.csv("grades.csv",stringsAsFactors = F)

# Structure of dataset
str(grades)

## Percentage of .zip files in the dataset
library(stringr)

# Counting ".zip" in the submission column 
sum(str_count(grades$submission,".zip"))

############################################################
#Q1.

# Percentage of .zip files
percentage <- (sum(str_count(grades$submission,".zip"))/nrow(grades))*100

percentage
# Ans:95.45455 
############################################################

# converting to submit_time column to date time format

grades$submit_time<- as.POSIXlt(grades$submit_time, format = "%m/%d/%y-%H:%M:%S")

# Extract hour, minute, second
grades$sec <- format(grades$submit_time, "%S")
grades$min <- format(grades$submit_time, "%M")
grades$hour <- format(grades$submit_time, "%H")
grades$date <- as.Date(grades$submit_time)
grades$day <- format(grades$date, "%d")
grades$month <- format(grades$date, "%m")

############################################################
# Q2.
# Is penalized
grades$penalized <- ifelse(grades$date >"2017-01-03",1,0)

# Sum of total penalized submission

sum(grades$penalized)

# Ans: 44
############################################################
# some analysis!
library(ggplot2)
#Q3
# On which date did the most students submit the assignment?
 ggplot(grades, aes(date)) + geom_histogram(stat = "count")

 #or
 which(data.frame(table(grades$date))$Freq==max(data.frame(table(grades$date))$Freq))
 data.frame(table(grades$date))[10,1]
 
 #or
 
 table(grades$date) # Manually check : #2017-01-03
 
 #Ans: 2017-01-03
############################################################

 #Q4 & Q5
#In which hour of the day did most students submit the solution?
 
ggplot(grades, aes(hour)) + geom_histogram(stat = "count")

# Ans:23:00 & Most people submit the assignment in the late evening (i.e. after 8 PM) 


# Q6.

#If you see the distribution of submissions by the minutes,
#do you find any pattern of distribution?

ggplot(grades, aes(min)) + geom_histogram(stat = "count")

# Ans: No, the distribution is Randomly distributed