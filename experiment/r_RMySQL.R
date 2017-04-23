
library(RMySQL)
library(dplyr)

#Establish connection
mydb <- dbConnect(MySQL(), user='<user>', password='<pwd>', 
                  dbname='superstoresdb', host='localhost',port=3308)  


# display in console table names
dbListTables(mydb)

#get results of sql query as a MySQLResult object
rs <- dbSendQuery(mydb, "select * from market_fact")
class(rs)

#fetch all MySQLResult result set object and return as dataframe
data <- fetch(rs, n=-1)


#get results of sql query as a dataframe
rs_df <- dbGetQuery(mydb,"select * from market_fact")


# apply filet directly on table column
filter_frame <- rs_df %>% filter(Prod_ID == "Prod_16")


subset_frame <- subset(rs_df,Prod_ID == "Prod_16")
#Error in eval(expr, envir, enclos) : object 'Prod_ID' not found
