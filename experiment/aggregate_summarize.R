timespent_bank = list()

#load data with around 45000 rows
bank_data <- read.csv("bank.csv", stringsAsFactors = F)

# measure aggregate performance
timespent_bank[["aggregate"]] = system.time(
aggregate_result <- aggregate(bank_data$salary,list(bank_data$job),mean))
print(timespent_bank[["aggregate"]])

# user  system elapsed 
# 0.08    0.00    0.08  


# load dplyr library
library(dplyr)

# measure summarize performance
timespent_bank[["summarize"]] = system.time(
summarize_result <-  bank_data %>% group_by(job) %>% summarise(m =mean(salary)))
print(timespent_bank[["summarize"]])

# user  system elapsed 
# 0.01    0.00    0.01


