cars <- mtcars
#cars$hp_cyl <- cars$hp/cars$cyl
#cars$wt_cyl <- cars$wt/cars$cyl
cars$random_var = sample(1:100, nrow(mtcars), replace = T)
model <- lm(mpg~.,data=cars)
summary(model)

cor(cars)