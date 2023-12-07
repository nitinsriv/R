library(tidyr)
library(dplyr)
# In case you have an SQL hangover 
library(sqldf)

# read data
pop <- read.csv("OnlineNewsPopularity.csv")
str(pop)


# ------- Part 1 and 2: Weekends and weekdays analysis --------
# checking the syntax of gather()
?gather

# gathering weekdays columns into one column day_of_week
pop_days <- gather(pop, key=day_of_week, value=is_day, weekday_is_monday:weekday_is_sunday )
str(pop_days)

# removing rows where is_weekday = 0
pop_days <- pop_days[-which(pop_days$is_day == 0), ]
str(pop_days)
pop_days <- pop_days[, -56]

# 1. Analysis of weekday versus weekends
# Note that there is a variable 'is_weekend', let's check it out
summary(pop_days$is_weekend)

# Convert to a factor type
pop_days$is_weekend <- as.factor(pop_days$is_weekend)
summary(pop_days$is_weekend)

# There are 5190 articles published on weekends, 34454 on weekdays
# Let's compare the average shares 
# Method 1: Base package
aggregate(shares~is_weekend, data=pop_days, mean)

# Method 2: dplyr group by
?group_by
weekend_grouping <- group_by(pop_days, is_weekend)
summarise(weekend_grouping, mean(shares))

# Method 3: sqldf
sqldf("select is_weekend, avg(shares) as avg_shares
      from pop_days
      group by is_weekend;")

# Conclusion: Avg shares on weekend seem to be higher than on weekdays

# 2. Analysis of weekdays
# subset all the weekday rows
?subset
pop_weekdays <- subset(pop_days, is_weekend==0)

# aggregate shares by weekdays
summary(pop_weekdays$day_of_week)
pop_weekdays$day_of_week <- as.factor(pop_weekdays$day_of_week)
summary(pop_weekdays$day_of_week)

# Can again use any of the three methods mentioned above
aggregate(shares~day_of_week, data=pop_weekdays, mean)

# Conclusion: No, Mondays seem to be more conducive for popularity. 
# No wonder Mondays are often unproductive

# ------------Part 3: Channel type analysis -------

# Again, channels are all in wide format, converting to long
pop_channels <- gather(pop, key=channel, value=is_channel, data_channel_is_lifestyle:data_channel_is_world)
str(pop_channels)
pop_channels <- pop_channels[-which(pop_channels$is_channel==0), ]
str(pop_channels)
pop_channels <- pop_channels[, -57]

summary(pop_channels$channel)
pop_channels$channel <- as.factor(pop_channels$channel)


# Aggregating shares by channel

# Method 1: Base package 
aggregate(shares~channel, data=pop_channels, mean)

# Method 2: group by from dplyr
channel_groups <- group_by(pop_channels, channel)
summarise(channel_groups, mean(shares))

# Method 3: sqldf
sqldf("select channel, avg(shares) as avg_shares from pop_channels group by channel;")

# Conclusion: Yes, artciles of lifestyle and social media seem to be shared more 
# than ther other channels

# Recommendations: 
# 1. More articles should be published on weekends than weekdays
# 2. In weekdays, most articles should be published on Mondays
# 3. Among channel types, lifestyle and social media articles should be published more than other channels

