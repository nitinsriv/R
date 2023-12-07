# Table of contents:
# 1. Reading from csv files - read.csv, read.delim
# 2. Reading from txt files
# 3. Reading from xlsx files
# 4. Reading from SQL database

# 1. csv files
?read.table
data1.1 <- read.csv("popularity.csv")
data1.2 <- read.delim("popularity.csv", sep=",")

# 2. txt files
data2.1 <- read.delim("popularity.txt", sep=",")

# 3. xlsx files - the best way is to convert to txt or csv and then read
# R wiki: https://en.wikibooks.org/wiki/R_Programming/Importing_and_exporting_data#Excel_.28xls.2Cxlsx.29

# install.packages("gdata")
library(gdata)
?read.xls
IPLdata_sheet1 <- read.xls("IPL_demo.xlsx", sheet=1)


# 4. SQL database
## Connecting to a database

library(RMySQL)
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "companydb",
                 host=  "127.0.0.1",
                 port = 3306,
                 user = "root",
                 password = "pgdda-iiitb")

query <- "SELECT * from employee"
empdf <- dbGetQuery(con,query)

query <- "Select * from department"
deptdf <- dbGetQuery(con,query)
