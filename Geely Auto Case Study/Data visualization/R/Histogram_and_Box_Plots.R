
##-------------Histogram and Box Plots Using Base Package-------------


# Plot histogram of Sepal.Width using hist(); 
hist(iris$Sepal.Width)



# type ?hist in R console and read up more about histograms




# Plot a histogram for Petal width and compare it with the plot abovcom <- 
hist(iris$Petal.Width)



# Make a boxplot of Petal.Length; read up on boxplots also. 
boxplot(iris$Petal.Length)


par(mfrow=c(1,2))
hist(iris$Petal.Width)
hist(iris$Sepal.Width)


ir <- InsectSprays
boxplot(count ~ spray,ir )

hist(iris$Species)