
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