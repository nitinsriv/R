timespent_bank = list()

# load dplyr packahe
library(dplyr)

#load data with around 45000 rows
bank_data <- read.csv("bank.csv", stringsAsFactors = F)

# measure filter performance
timespent_bank[["filter"]] = system.time(
filter_result <- filter(bank_data,balance > 1000 & marital == "married"))
print(timespent_bank[["filter"]])

# user  system elapsed 
# 0.01    0.00    0.02 


#measure subset performance
timespent_bank[["subset"]] = system.time(
subset_result <- subset(bank_data,balance > 1000 & marital == "married"))
print(timespent_bank[["subset"]])

# user  system elapsed 
# 0.01    0.00    0.02 