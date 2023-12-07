library(tidyr)
library(dplyr)

#read config file from working directory
config_file <-  read.csv("config.csv",sep="=",stringsAsFactors = F)

#read value of root_dir
base_dir <- config_file$value[1]

#initialize data folder and data file
data_dir <- "data/"
data_file <- "popularity.csv"

#construct data file path from config file
abs_datafile_path <- paste(base_dir,data_dir,data_file, sep="")

# read data file
# adopting this approach deployment team is free 
# to configure the location of files in Production environment
popularity_data <- read.csv(abs_datafile_path, stringsAsFactors = F)


# convert days information from wide to long format
# final data frame contains only asymmetric values
data <- popularity_data
newdata <- gather(data, day, my_val, weekday_is_monday:weekday_is_sunday)
newdata <- newdata[!(newdata$my_val == 0),]

newdata <- gather(newdata, category, val,  data_channel_is_lifestyle:data_channel_is_world)
newdata <- newdata[!(newdata$val == 0),]

avg_weekdays <- newdata %>% group_by(is_weekend) %>% summarise(avg = mean(shares))

sum_weekdays <-  newdata %>% group_by(is_weekend) %>% summarise(s = n())

avg_days <- newdata %>% group_by(day) %>% summarise(avg = mean(shares))

sum_categ <- newdata %>% group_by(category) %>% summarise(avg = mean(shares))
