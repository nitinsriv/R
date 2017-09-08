library(tidyr)
library(dplyr)
library(ggplot2)

#read data from csv
uber <- read.csv("C:\\IIITB\\Uber Case Study\\Uber Request Data.csv")

#split Request.timestamp column into two columns 'Request Date' and 'Request Time'
uber_split_req <- separate(uber, Request.timestamp,c("Request Date","Request Time"), sep =" ",extra="merge", fill="right",remove=FALSE)

#split Drop.timestamp column into two columns 'Drop Date' and 'Drop Time'
uber_split_drop <- separate(uber_split_req, Drop.timestamp,c("Drop Date","Drop Time"), sep =" ",extra="merge", fill="right",remove=FALSE)

uber_split_drop1 <- uber_split_drop

# format Request.Date in %Y-%m-%d
drop1_req_slash <- as.Date(uber_split_drop1$`Request Date`,"%d/%m/%Y")
drop1_req_dash <- as.Date(uber_split_drop1$`Request Date`,"%d-%m-%Y")
uber_split_drop1$`Request Date` <- as.Date(ifelse(is.na(drop1_req_dash),drop1_req_slash,drop1_req_dash),origin = "1970-01-01")


# format Drop.Date in %Y-%m-%d
drop1_drp_slash <- as.Date(uber_split_drop1$`Drop Date`,"%d/%m/%Y")
drop1_drp_dash <- as.Date(uber_split_drop1$`Drop Date`,"%d-%m-%Y")
uber_split_drop1$`Drop Date` <- as.Date(ifelse(is.na(drop1_drp_dash),drop1_drp_slash,drop1_drp_dash),origin = "1970-01-01")

# plot a graph to understand PickUp points distribution 
# add Status information to plot.
pickup_plot <- ggplot(uber_split_drop1,aes(uber_split_drop1$Pickup.point,fill=uber_split_drop1$Status))+geom_histogram(stat="count")

# add x,y labels, plot title and legend name
pickup_plot_label <- pickup_plot + labs(x = "PickUp Point", y = "No. of Rides", title = "PickUp Point Distribution", fill = "Status")

# pickup_plot_label Plot shows:
# No. of Trips from City To Airport are more compared to Airport to City.
# No. of Trips 'Completed' from City To Airport are more compared to Airport to City.


# add a column to capture hour of trip
f1 <- as.numeric(substr(uber_split_drop1$`Request Time`,1,2))
f2 <- as.numeric(substr(uber_split_drop1$`Request Time`,1,1))
hour <- ifelse(is.na(f1),f2,f1)
uber_split_drop1 <- cbind(uber_split_drop1,hour)



# Insights for  City to Airport Trips

# create dataframe with only PickUp point City and
# trips which were not completed.


tAirportRides_notCompleted <- uber_split_drop1 %>% filter((uber_split_drop1$Status %in% c("Cancelled" , "No Cars Available")) & (uber_split_drop1$Pickup.point %in% c("City")))

# plot fill plot to identify pattern in cancelled/no cars available
notCompleted_plot <- ggplot(tAirportRides_notCompleted,aes(hour,fill=tAirportRides_notCompleted$Status))+geom_bar(position="fill")

# add labels to plot
notCompleted_plot <- notCompleted_plot + labs(x = "hour", y = "fraction", title = "Revenue Loss", fill = "Status")

# plot fill plot to identify distribution of Cancelled/No Cars Available
notCompletedFraction_plot <- ggplot(tAirportRides_notCompleted,aes(tAirportRides_notCompleted$Status,fill=tAirportRides_notCompleted$Status))+geom_bar(position="stack")

# add labels to plot
notCompletedFraction_plot <- notCompletedFraction_plot + labs(x = "Status", y = "No. of Rides", title = "Revenue Loss Distribution", fill = "Status")

# notCompletedFraction_plot Plot shows:
# There are more Cancellations compared to 'No Cars Available'.

# notCompleted_plot Plot shows:
# 50% or more of revenue loss between 4 AM till 10 AM is because of 'Cancelled' trips
# Trips cancelled more than 50% at 17 and 21 hours in a day.


# create a dataframe for which records
# no of trips cancelled by each driver in descending order.

driver_trips <- tAirportRides_notCompleted %>% group_by(factor(tAirportRides_notCompleted$Driver.id)) %>% summarise(No. = n())
driver_trips <- driver_trips[order(driver_trips$No.,decreasing=TRUE),]

# driver_trips dataframe shows below first five driver ids
# in descending order of no. of trips cancelled or 'no cars available' trips.
# 1         NA   937
# 2         84    11
# 3         54    10
# 4        142    10
# 5         27     8
# 6        131     8




# Insights for Airport to City Trips

# create dataframe with only PickUp point Airport and
# trips which were not completed.


tCityRides_notCompleted <- uber_split_drop1 %>% filter((uber_split_drop1$Status %in% c("Cancelled" , "No Cars Available")) & (uber_split_drop1$Pickup.point %in% c("Airport")))

# plot fill plot to identify pattern in cancelled/no cars available
notCompleted_plot_Airport <- ggplot(tCityRides_notCompleted,aes(hour,fill=tCityRides_notCompleted$Status))+geom_bar(position="fill")

# add labels to plot
notCompleted_plot_Airport <- notCompleted_plot_Airport + labs(x = "hour", y = "fraction", title = "Revenue Loss", fill = "Status")


# plot fill plot to identify distribution of Cancelled/No Cars Available
notCompletedFraction_plot_Airport <- ggplot(tCityRides_notCompleted,aes(tCityRides_notCompleted$Status,fill=tCityRides_notCompleted$Status))+geom_bar(position="stack")

# add labels to plot
notCompletedFraction_plot_Airport <- notCompletedFraction_plot_Airport + labs(x = "Status", y = "No. of Rides", title = "Revenue Loss Distribution", fill = "Status")
