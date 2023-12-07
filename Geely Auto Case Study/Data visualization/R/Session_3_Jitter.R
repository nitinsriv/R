##Jitter 
library(ggplot2)

sunflowerplot(iris$Petal.Length,iris$Petal.Width,col=iris$Species,size=.03)
ggplot(iris,aes(iris$Petal.Length,iris$Petal.Width,col=iris$Species))+geom_point()


sunflowerplot(diamonds$clarity,diamonds$carat,col=diamonds$price,size=.03)

# Plot the cyl on the x-axis and wt on the y-axis
# Note that points on 3 vertical lines are not easy to read





# Use geom_jitter() instead of geom_point(); Jitter is a type of point plot used to avoid overplotting,
# especially for categorical variables like cyl. Jitter can be written as a position in the geom_point layer,
# which means the code below is same as geom_point(position = "jitter") 

# Note that jitter plot scatters the points a little too much




# Let's avoid the random scatter made by jitter using a width argument in jitter
# You can define a position object and put it inside 
# Define a position object using position_jitter(): 



# Use jitter_posn object inside the geom_point in geom_point() or geom_jitter
# Both commands below are exactly the same




 ggplot(diamonds, aes(x = diamonds$clarity, y = carat)) + geom_point(col = diamonds$price)

ggplot(diamonds, aes(x = clarity, y = carat, col = price)) + geom_bar()
ggplot(diamonds, aes(x = clarity, y = carat, col = price)) +geom_point()
ggplot(diamonds, aes(x = clarity, y = carat, col = price)) + stat_sum()

ggplot(diamonds, aes(x = clarity, y = carat, col = price)) +geom_point(position="jitter")