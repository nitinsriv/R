#----------Creating Visualizations Using ggplot()------------

library(ggplot2)
# --------Plotting using ggplot2: mtcars data frame----------

# mtcars has observations of 32 cars with miles per gallon, cylinders, weight, no of gears etc.

m <- mtcars

# Check structure of mtcars 
str(mtcars)


# This is the first plot using ggplot2 using the built-in data frame mtcars.
# Plot cyl on x and mpg on y axis
ggplot(m,aes(x=factor(am), y=mpg)) + geom_point(shape=4, size=2)



# Identify which elements are in data, aes() and geom layers 
# Note that there are no 5 or 7 cylinder cars, but the plot shows 5 and 7 on x axis. Why?

# It is because cyl as x axis is not declared as categorical variable.



# Use factor(cyl) this time, don't change anything else in the plot above
# ggplot can convert a num variable to a factor variable. Looks better, right?
ggplot(mtcars, aes(x = wt,y = mpg, col = factor(cyl))) + geom_point()
ggplot(mtcars, aes(x = wt,y = mpg, col = cyl)) + geom_point()


# Reverse the order of variables: plot mpg on x and factor(cyl) on y 
ggplot(mtcars, aes(x = mpg,y = factor(cyl))) + geom_point()


# Plot am versus mpg (on x and y axes respectively)
ggplot(mtcars, aes(x = am,y = mpg)) + geom_point()


# Should you use factor() here? - Yes 
ggplot(mtcars, aes(x = factor(am),y = mpg)) + geom_point()


df <- data.frame(x = 1:10 , y = 1:10)
f <- ggplot(df, aes(x, y)) + geom_line(linetype = "3313")

ggplot(mtcars,aes(wt,mpg,col=hp))+geom_point()

ggplot(mtcars,aes(wt,mpg,size=disp,col=hp))+geom_point()
