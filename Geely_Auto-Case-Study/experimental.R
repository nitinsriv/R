
library(corrplot)
library(car)
library(MASS)
library(ggplot2)
price <- read.csv("CarPrice_Assignment.csv", header=TRUE)


########################### DATA PREPARATION #############################


# variables with 2 levels are  assigned 1 and 0.

# fueltype: diesle - 1; gas - 0
levels(price$fueltype) <-c(1,0)
price$fueltype <- as.numeric(levels(price$fueltype))[price$fueltype]

# aspiration: std - 1; turbo - 0
levels(price$aspiration) <-c(1,0)
price$aspiration <- as.numeric(levels(price$aspiration))[price$aspiration]

# doornumber:  four - 1; two - 2;
levels(price$doornumber) <-c(1,0)
price$doornumber <- as.numeric(levels(price$doornumber))[price$doornumber]

# enginelocation: front - 1; rear - 2
levels(price$enginelocation) <-c(1,0)
price$enginelocation <- as.numeric(levels(price$enginelocation))[price$enginelocation]


##################### Dummy Variables Creation ######################

#Converting "carbody" into dummies . 
dummy_carbody <- data.frame(model.matrix( ~carbody, data = price))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_carbody <- dummy_carbody[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
price <- cbind(price[,-7], dummy_carbody)

price$carbodyconvertible <- 0
price[which(price$carbodyhardtop==0 & price$carbodyhatchback==0 & price$carbodysedan==0 & price$carbodywagon==0),]$carbodyconvertible <- 1


#Converting "drivewheel" into dummies . 
dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = price))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_drivewheel <- dummy_drivewheel[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
price <- cbind(price[,-7], dummy_drivewheel)

price$drivewheel4wd <- 0
price[which(price$drivewheelfwd==0 & price$drivewheelrwd==0),]$drivewheel4wd <- 1


#Converting "enginetype" into dummies . 
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = price))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_enginetype <- dummy_enginetype[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
price <- cbind(price[,-13], dummy_enginetype)

price$enginetypedohc <- 0
price[which(price$enginetypedohcv==0 & price$enginetypel==0 & price$enginetypeohc==0 & price$enginetypeohcf==0)
      & price$enginetypeohcv==0 & price$enginetyperotor==0,]$enginetypedohc <- 1



#Converting "cylindernumber" into dummies . 
dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = price))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_cylindernumber <- dummy_cylindernumber[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
price <- cbind(price[,-13], dummy_cylindernumber)

price$cylindernumbereight <- 0
price[which(price$cylindernumberfive ==0 & price$cylindernumberfour==0 & price$cylindernumbersix==0 
            & price$cylindernumberthree==0 & price$cylindernumbertwelve==0 & price$cylindernumbertwo==0),]$cylindernumbereight <- 1


#Converting "fuelsystem" into dummies . 
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = price))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_fuelsystem <- dummy_fuelsystem[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
price <- cbind(price[,-14], dummy_fuelsystem)

price$fuelsystem1bbl <- 0
price[which(price$fuelsystem2bbl==0 & price$fuelsystem4bbl==0 & price$fuelsystemidi==0 & price$fuelsystemmfi==0 & price$fuelsystemmpfi==0 & price$fuelsystemspdi==0 & price$fuelsystemspfi==0),]$fuelsystem1bbl <- 1


View(price)

# check how price variable is distributed
ggplot(price,aes(price))+geom_histogram(bins=20)
#price$price <- log(price$price)
#price$peakrpm <- log(price$peakrpm)
#price$curbweight <- log(price$curbweight)

price[,-c(1,3)]<- scale(price[,-c(1,3)],center = FALSE,scale=TRUE)

#price <- price[-which(price$price>40000),]
#price <- price[-which(price$horsepower>250),]
#price <- price[-which(price$compressionratio>20),]
#price <- price[-which(price$enginesize>250),]

# Divide into training and test data set
#set the seed to 100. 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(price), 0.7*nrow(price))
# generate the train data set
train = price[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = price[-trainindices,]


# Find correlation between quantitative variables.
# create dataframe with numeric variables

sink("Correlation Coef.txt",append=FALSE,split=FALSE)
options(max.print=999999)
cor(price[,-c(1,3)])
sink()

cor_matrix_dataframe <- price[,-c(1,3)]
cor_matrix <- round(cor(cor_matrix_dataframe),2)
png(height=2100, width=2100, pointsize=30, file="Correlation Plot.png")
color <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                            "cyan", "#007FFF", "blue","#00007F")) 
#cor_plot <- corrplot(cor_matrix, na.label="NA",method="number",number.cex=1,addCoef.col = "red", title = "Correalation Matrix")
cor_plot <- corrplot(cor_matrix, na.label="NA",method="color",col=color(10),
                     order="hclust",title = "Correalation Matrix")
dev.off()

##############

#Execute the first model_1 multilinear model in the training set. 
f_stat <- qf(.95,6,143)
model_1 <-lm(price~enginesize,data=train)

# Check the summary of model. 
#looking at f_stat and F-statistic of model, reject null hypothesis that coefficients for variables can be zero.

summary(model_1)


#Execute the first model_1 multilinear model in the training set. 
model_2 <-lm(price~enginesize+curbweight,data=train)

# Check the summary of model. 
summary(model_2)
vif(model_2)
# curbweight is correlated to enginesize.
# drop curbweight since it has smaller value of p and also the model has better adj. R square value.


#Execute the first model_1 multilinear model in the training set. 
model_3 <-lm(price~enginesize+horsepower,data=train)

# Check the summary of model. 
summary(model_3)
vif(model_3)

#Execute the first model_1 multilinear model in the training set. 
model_4 <-lm(price~enginesize+horsepower+curbweight,data=train)

# Check the summary of model. 
summary(model_4)
vif(model_4)

#Execute the first model_1 multilinear model in the training set. 
model_5 <-lm(price~enginesize+horsepower+curbweight+carwidth,data=train)

# Check the summary of model. 
summary(model_5)
# Note curbweight and carwidth become insignificant since they are correlated.
# remove curbweight and carwidth are corelated to enginesize
vif(model_5)

#Execute the first model_1 multilinear model in the training set. 
model_6 <-lm(price~enginesize+highwaympg+horsepower,data=train)

# Check the summary of model. 
summary(model_6)
# Note horsepower vif value.
# remove horsepower since it is corelated to enginesize
vif(model_6)


#Execute the first model_1 multilinear model in the training set. 
model_7 <-lm(price~enginesize+highwaympg,data=train)

# Check the summary of model. 
summary(model_7)
# Note horsepower vif value.
# remove horsepower since it is corelated to enginesize
vif(model_7)

#Execute the first model_1 multilinear model in the training set. 
model_8 <-lm(price~enginesize+highwaympg+peakrpm,data=train)

# Check the summary of model. 
summary(model_8)
# Note highwaympg becomes insignificant.
# remove highwaympg since it is corelated to enginesize
vif(model_8)

#Execute the first model_1 multilinear model in the training set. 
model_9 <-lm(price~enginesize+peakrpm,data=train)

# Check the summary of model. 
summary(model_9)
# Note highwaympg becomes insignificant.
# remove highwaympg since it is corelated to enginesize
vif(model_9)

#Execute the first model_1 multilinear model in the training set. 
model_10 <-lm(price~enginesize+peakrpm+compressionratio,data=train)

# Check the summary of model. 
summary(model_10)
vif(model_10)

#Execute the first model_1 multilinear model in the training set. 
model_11 <-lm(price~enginesize+peakrpm+compressionratio+carheight,data=train)

# Check the summary of model. 
summary(model_11)
vif(model_11)

model_12 <-lm(price~enginesize+peakrpm+compressionratio+carheight+fueltype,data=train)
# Check the summary of model. 
summary(model_12)
# Note compressionration become insignificant by introducing fueltype
# remove compressionration in next model
vif(model_12)


model_14 <-lm(price~enginesize+peakrpm+carheight+fueltype,data=train)
# Check the summary of model. 
summary(model_14)
# Note compressionration become insignificant by introducing fueltype
# remove compressionration in next model
vif(model_14)

model_15 <-lm(price~enginesize+peakrpm+carheight+fueltype+stroke,data=train)
# Check the summary of model. 
summary(model_15)
# Note compressionratio become insignificant by introducing fueltype
# remove compressionration in next model
vif(model_15)

model_16 <-lm(price~enginesize+peakrpm+carheight+fueltype+stroke+enginelocation,data=train)
# Check the summary of model. 
summary(model_16)
vif(model_16)

# plot Residuals vs Fitted
plot(model_16,pch=16,which=1)


model_17 <-lm(price~enginesize+peakrpm+fueltype+stroke+enginetypeohcv,data=train)
# Check the summary of model. 
summary(model_17)
vif(model_17)

# enginetypeohcv related to enginesize, removed
model_18 <-lm(price~enginesize+peakrpm+fueltype+stroke,data=train)
# Check the summary of model. 
summary(model_18)
vif(model_18)


model_19 <-lm(price~enginesize+peakrpm+fueltype+stroke+cylindernumberfour+carbodyhatchback,data=train)
# Check the summary of model. 
summary(model_19)
# Residuals more normally distributed.

vif(model_19)


model_20 <-lm(price~enginesize+peakrpm+stroke+cylindernumberfour+carbodyhatchback,data=train)
# Check the summary of model. 
summary(model_20)
# Residuals more normally distributed.

vif(model_20)

#remove stroke
model_21 <-lm(price~enginesize+peakrpm+cylindernumberfour+carbodyhatchback+enginelocation+drivewheelfwd
              +highwaympg,data=train)
# Check the summary of model. 
summary(model_21)
# Residuals more normally distributed.

vif(model_21)

#replace peakrpm with fueltype, remove drivewheelfwd
model_22 <-lm(price~enginesize+fueltype+cylindernumberfour+carbodyhatchback+enginelocation
              ,data=train)
# Check the summary of model. 
summary(model_22)
# Residuals more normally distributed.

vif(model_22)

model_23 <-lm(price~enginesize+highwaympg+carbodyhatchback+enginelocation
              ,data=train)
# Check the summary of model. 
summary(model_23)
# Residuals more normally distributed.

vif(model_23)

model_24 <-lm(price~enginesize+carbodyhatchback+enginelocation
              ,data=train)
# Check the summary of model. 
summary(model_24)
# Residuals more normally distributed.

vif(model_24)




# Predict the house prices in the testing dataset
Predict_1 <- predict(model_24,test[,-1])
test$predicted_price <- Predict_1

test$error <-  (test$price - test$predicted_price)

# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(test$car_ID, test$price)) + geom_line(aes(color = "blue" )) + geom_line(aes(car_ID,predicted_price, color="red"))

