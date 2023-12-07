set.seed(100)
trainindices <- sample(1:nrow(mtcars),0.7*nrow(mtcars))
train_data <- mtcars[trainindices,]
test_data <- mtcars[-trainindices,]
model <- lm(train_data$mpg~train_data$wt,data=train_data)
summary(model)
