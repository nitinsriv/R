# Tidyr
# install.packages("tidyr")
# install.packages("dplyr")
library(tidyr)
library(dplyr)

# Reading the data
popularity <- read.csv("popularity.csv")
data <- popularity

## Basic functions
class(data)
names(data)

# Understanding the attributes
str(data)
summary(data)

## Wide and Long data frames
# 1. The columns weekday_is_monday - weekday_is_sunday are called 'wide format'
# Problems: How to group by / aggregate / order by weekdays? 

# 2. Similary, the columns data_channel_is_lifestyle-data_channel_is_world are wide

## TidyR functions
#gather the data together. We need to have a key value pair. The key is the day and the value is my_val
?gather
newdata <- gather(data, day, my_val, weekday_is_monday:weekday_is_sunday)
str(newdata)

# the new column named 'day' contains values like 'weekday_is_monday', 'weekday_is_tuesday' etc.

# The number of rows increased drastically. This is because the column my_val has a lot of 
# 0's. A value of 0 denotes that the article was not published on the corresponding weekday.
newdata <- newdata[!(newdata$my_val == 0),]


# Now you can also remove the last column my_val from the data frame. 
newdata <- newdata[, -56]

# Similarly, we can gather the columns data_channel_is_lifestyle etc.
newdata_long <- gather(newdata, channel, channel_val, data_channel_is_lifestyle:data_channel_is_world)
newdata_long <- newdata_long[!(newdata_long$channel_val == 0), ]
newdata_long <- newdata_long[, -51]

# Spreading the data (from long to wide)
newdata1 <- spread(newdata, day, my_val)

# Notice that a lot of NA's have been introduced. Those are not actually missing values.
# They should be 0's. 



## Separate and unite are used to separate/unite data in a column. 
# separate() is used to separate one column into multiple columns. 

# let's look at the 'day' column in newdata (now in long format)
# say you want to separate weekday_is_monday into weekday and monday
head(newdata$day)
?separate
newdata_separated <- separate(newdata, day, into=c("type_of _day", "day"), sep="_is_")
head(newdata_separated)
# Let's look at some urls
data[1:10, 1]
newdata3 <- separate(data, url, into=c("link", "site"), sep = "-")
newdata3[1:10, 1:2]
#
newdata4 <- separate(data, url, c("link", "site", "x", "y", "z"), sep = "-")
newdata4[1:10, 1:5]

# Unite is used to do the reverse - unite columns into one
newdata6 <- unite(newdata4, url , link, site, x, y, z)

# dplyr: https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
library(dplyr)
str(newdata)

# 1. filter()
# 2. arrange()
# 3. select()
# 4. distinct()
# 5. mutate()
# 6. summarise()
# 7. group_by()

# 1. filter 
# analogous to subset() from base
# note the double equal ==
?filter
filter(newdata_long, day == "weekday_is_monday", channel == "data_channel_is_lifestyle")

# 2. arrange()
?arrange
newdata_arranged <- arrange(newdata_long, day, channel)
head(newdata_arranged)

# can arrange in decreasing order
newdata_arranged <- arrange(newdata_long, desc(shares))
head(newdata_arranged)

# sort by two variables
newdata_arranged <- arrange(newdata_long, desc(shares, n_tokens_title))
head(newdata_arranged)

# 3. select()
?select 
# selects columns of interest
select(newdata_long, day, channel, shares)

# another way of doing the same thing
select(newdata_long, shares:channel)

# 4. distinct()
?distinct
distinct(newdata_long, day)
distinct(newdata_long, channel)

# 5. mutate()
?mutate
newdata_long <- mutate(newdata_long, 
                       rate_positivity = rate_positive_words/rate_negative_words, 
                       gain = n_tokens_title - n_tokens_content)
head(newdata_long)

# 6. summarise()
?summarise 
summarise(newdata_long, shares_avg = mean(shares, na.rm = T))
mean(newdata_long$shares, na.rm=T)

# 7. grouping 
?group_by

# The average number of shares day-wise
day_groups <- group_by(newdata_long, day)
summarise(day_groups, 
          mean(shares, na.rm = T))

# The average number of shares day and channel wise
day_channel_groups <- group_by(newdata_long, day, channel)
day_ch_summary <- summarise(day_channel_groups, mean(shares, na.rm = T))
head(day_ch_summary)
