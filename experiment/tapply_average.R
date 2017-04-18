supp <- c("OJ","VC","OJ","VC","OJ","VC")
dose <- c(0.5,0.5,1.0,1.0,2.0,2.0)
len <- c(13.23,7.98,22.70,16.77,26.06,26.14)

df <- data.frame(supp,dose,len)

timespent = list()

#usage of aggregate
#note class of return type. It's dataframe.
timespent[["aggregate"]] = system.time(
              aggregate_result <- aggregate(df,list(df$supp),mean))
print(timespent[["aggregate"]])
print(class(aggregate_result))


#failure : you can't put objects with different length
tapply_result <- tapply(df,df$supp,mean)
#Error in tapply(df, df$supp, mean) : arguments must have same length


##########################################################

#This implies, I need to write more code to retireve above result
#individual grouping to be don on len and dose attributes.
#then combine the results into one dataframe


#grouping and combining on len
#notice class of return type. It is an array.
tapply_len_result <- tapply(df$len,df$supp,mean)
print(tapply_len_result)
print(class(tapply_len_result))

#grouping on supp
tapply_dose_result <- tapply(df$dose,df$supp,mean)
print(tapply_dose_result)
print(class(tapply_dose_result))

#combine above results
tapply_combine_result <- data.frame(tapply_len_result,tapply_dose_result)

############################################################




