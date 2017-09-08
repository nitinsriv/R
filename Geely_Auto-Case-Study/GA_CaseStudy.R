
library(stringr)
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
price[which(price$enginetypedohcv==0 & price$enginetypel==0 & price$enginetypeohc==0 & price$enginetypeohcf==0
            & price$enginetypeohcv==0 & price$enginetyperotor==0),]$enginetypedohc <- 1



#Converting "cylindernumber" into dummies . 
dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = price))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_cylindernumber <- dummy_cylindernumber[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
price <- cbind(price[,-13], dummy_cylindernumber)

price$cylindernumbereight <- 0
price[which(price$cylindernumberfive ==0 & price$cylindernumberfour==0 & price$cylindernumbersix==0 
            & price$cylindernumberthree==0 & price$cylindernumbertwelve==0 & price$cylindernumbertwo==0),]$cylindernumbereight <- 1


price$fuelsystem <- str_replace(price$fuelsystem,"mfi","mpfi")

#Converting "fuelsystem" into dummies . 
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = price))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_fuelsystem <- dummy_fuelsystem[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
price <- cbind(price[,-14], dummy_fuelsystem)

price$fuelsystem1bbl <- 0
price[which(price$fuelsystem2bbl==0 & price$fuelsystem4bbl==0 & price$fuelsystemidi==0 & price$fuelsystemmpfi==0 & price$fuelsystemspdi==0 & price$fuelsystemspfi==0),]$fuelsystem1bbl <- 1

price$CarName <- gsub("\\s.*","",price$CarName)
price$CarName <- gsub("-.*","",price$CarName)
price$CarName <- tolower(as.factor(price$CarName))

price$CarName <- str_replace(price$CarName,"porcshce","porsche")
price$CarName <- str_replace(price$CarName,"vokswagen","volkswagen")
price$CarName <- str_replace(price$CarName,"toyouta","toyota")


#Converting "carName" into dummies . 
dummy_carName <- data.frame(model.matrix( ~CarName, data = price))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_carName <- dummy_carName[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
price <- cbind(price[,-3], dummy_carName)

price$CarNamealpha <- 0
price[which(price$CarNameaudi==0 & price$CarNamebmw==0 & price$CarNamebuick==0 & price$CarNamechevrolet==0 
            & price$CarNamedodge==0 & price$CarNamehonda==0 & price$CarNameisuzu==0 & price$CarNamejaguar==0
            & price$CarNamemaxda==0 & price$CarNamemazda==0 & price$CarNamemercury==0 & price$CarNamemitsubishi==0
            & price$CarNamenissan==0 & price$CarNamepeugeot==0 & price$CarNameplymouth==0 & price$CarNameporsche==0
            & price$CarNamesaab==0 & price$CarNamerenault==0 & price$CarNamesubaru==0 & price$CarNametoyota==0
            & price$CarNamevolkswagen==0 & price$CarNamevolvo==0 & price$CarNamevw==0),]$CarNamealpha <- 1


# check for duplicates
price <- unique(price)


# check for NA values
na_count <- sum(is.na(price))


# check summary of price dataframe to understand distribution of variables
sumry <- summary(price)

#quantile
#housing$crime_rate[which()]<-25.0216600
#binning
#step
View(price)

# check how price variable is distributed
ggplot(price,aes(price))+geom_histogram(bins=20)
#price$price <- log(price$price)
#price$peakrpm <- log(price$peakrpm)
#price$curbweight <- log(price$curbweight)

#price<- scale(price,center = FALSE,scale=TRUE)

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
cor(price)
sink()

cor_matrix_dataframe <- price
cor_matrix <- round(cor(cor_matrix_dataframe),2)
png(height=2100, width=2100, pointsize=30, file="Correlation Plot.png")
color <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                            "cyan", "#007FFF", "blue","#00007F")) 
#cor_plot <- corrplot(cor_matrix, na.label="NA",method="number",number.cex=1,addCoef.col = "red", title = "Correalation Matrix")
cor_plot <- corrplot(cor_matrix, na.label="NA",method="color",col=color(10),
                     number.cex = 1, order="hclust",title = "Correalation Matrix")
dev.off()

##############

#Execute the first model_1 multilinear model in the training set. 
f_stat <- qf(.95,6,143)
model_1 <-lm(price~.,data=train)

# Check the summary of model. 
# looking at f_stat and F-statistic of model, reject null hypothesis that coefficients for variables can be zero.
# NA values suggest of linear dependiencies among variables.
summary(model_1)


#Execute the first model_1 with stepAIC in the training set. 
step <- stepAIC(model_1, direction="both")


# retaining variables selected by stepAIC. 
#stepAIC gives below list of variables
#compressionratio+CarNameaudi+fuelsystemmpfi+boreratio+fueltype+fuelsystemidi+CarNamejaguar
#+CarNamealpha+enginetypeohc+fuelsystemspdi+CarNamebuick+enginetypedohc+cylindernumberfour+doornumber
#+wheelbase+symboling+cylindernumbersix+highwaympg+horsepower+enginetypeohcv+drivewheelfwd+drivewheel4wd
#+cylindernumbereight+carlength+carheight
model_2 <- lm(price~fuelsystem1bbl+compressionratio+CarNameaudi+fuelsystemmpfi+boreratio+fueltype
              +fuelsystemidi+CarNamejaguar+CarNamealpha+enginetypeohc+fuelsystemspdi+CarNamebuick
              +enginetypedohc+cylindernumberfour+doornumber+wheelbase+symboling+cylindernumbersix
              +highwaympg+horsepower+enginetypeohcv+drivewheelfwd+drivewheel4wd+cylindernumbereight
              +carlength+carheight,data=train)
summary(model_2)


# remove fuelsystemidi .
# Execute the model_3 multilinear model in the training set. 
model_3 <-lm(price~fuelsystem1bbl+compressionratio+CarNameaudi+fuelsystemmpfi+boreratio+fueltype
             +CarNamejaguar+CarNamealpha+enginetypeohc+fuelsystemspdi+CarNamebuick
             +enginetypedohc+cylindernumberfour+doornumber+wheelbase+symboling+cylindernumbersix
             +highwaympg+horsepower+enginetypeohcv+drivewheelfwd+drivewheel4wd+cylindernumbereight
             +carlength+carheight,data=train)

# Check the summary of model. 
summary(model_3)
vif(model_3)

# remove fuelsystemspdi since it has higest p-value
# Execute the model_4 multilinear model in the training set. 
model_4 <-lm(price~fuelsystem1bbl+compressionratio+CarNameaudi+fuelsystemmpfi+boreratio+fueltype
             +CarNamejaguar+CarNamealpha+enginetypeohc+CarNamebuick+enginetypedohc+cylindernumberfour
             +doornumber+wheelbase+symboling+cylindernumbersix+highwaympg+horsepower+enginetypeohcv
             +drivewheelfwd+drivewheel4wd+cylindernumbereight+carlength+carheight,data=train)

# Check the summary of model. 
summary(model_4)
vif(model_4)

# refer Correlation Coef.txt file created above
# cylindernumberfour corelated to horsepower.
# dropping cylindernumberfour. it has high p-value
# Execute the  model_5 multilinear model in the training set. 
model_5 <-lm(price~fuelsystem1bbl+compressionratio+CarNameaudi+fuelsystemmpfi+boreratio+fueltype
             +CarNamejaguar+CarNamealpha+enginetypeohc+CarNamebuick+enginetypedohc+doornumber+wheelbase
             +symboling+cylindernumbersix+highwaympg+horsepower+enginetypeohcv+drivewheelfwd
             +drivewheel4wd+cylindernumbereight+carlength+carheight,data=train)

# Check the summary of model. 
summary(model_5)
vif(model_5)


# a marginal increase in adj R square observed
# fueltype has high p-value and VIF.
# refer Correlation Coef.txt file created above
# fueltype is highly correlated to compressionratio
# drop fueltype
# Execute the model_6 multilinear model in the training set. 
model_6 <-lm(price~fuelsystem1bbl+compressionratio+CarNameaudi+fuelsystemmpfi+boreratio+CarNamejaguar
             +CarNamealpha+enginetypeohc+CarNamebuick+enginetypedohc+doornumber+wheelbase+symboling
             +cylindernumbersix+highwaympg+horsepower+enginetypeohcv+drivewheelfwd+drivewheel4wd
             +cylindernumbereight+carlength+carheight,data=train)

# Check the summary of model. 
summary(model_6)
vif(model_6)


# drivewheel4wd has high p-value
# drop drivewheel4wd
# Execute the first model_7 multilinear model in the training set. 
model_7 <-lm(price~fuelsystem1bbl+compressionratio+CarNameaudi+fuelsystemmpfi+boreratio+CarNamejaguar
             +CarNamealpha+enginetypeohc+CarNamebuick+enginetypedohc+doornumber+wheelbase+symboling
             +cylindernumbersix+highwaympg+horsepower+enginetypeohcv+drivewheelfwd+cylindernumbereight
             +carlength+carheight,data=train)

# Check the summary of model. 
summary(model_7)
vif(model_7)


# drivewheelfwd has high p-value
# refer Correlation Coef.txt file created above
# drivewheelfwd also correlated to highwaympg
# drop drivewheelfwd
# Execute the model_8 multilinear model in the training set. 
model_8 <-lm(price~fuelsystem1bbl+compressionratio+CarNameaudi+fuelsystemmpfi+boreratio+CarNamejaguar
             +CarNamealpha+enginetypeohc+CarNamebuick+enginetypedohc+doornumber+wheelbase+symboling
             +cylindernumbersix+highwaympg+horsepower+enginetypeohcv+cylindernumbereight+carlength
             +carheight,data=train)

# Check the summary of model. 
summary(model_8)
vif(model_8)


# wheelbase has high p-value and VIF
# refer Correlation Coef.txt file created above
# wheelbase is correlated to carlenght
# drop wheelbase
# Execute the model_9 multilinear model in the training set. 
model_9 <-lm(price~fuelsystem1bbl+compressionratio+CarNameaudi+fuelsystemmpfi+boreratio+CarNamejaguar
             +CarNamealpha+enginetypeohc+CarNamebuick+enginetypedohc+doornumber+symboling+cylindernumbersix
             +highwaympg+horsepower+enginetypeohcv+cylindernumbereight+carlength+carheight,data=train)

# Check the summary of model. 
summary(model_9)
vif(model_9)



# doornumber has high p-value
# refer Correlation Coef.txt file created above
# doornumber is correlated to carheight and symboling
# drop doornumber
# Execute the model_10 multilinear model in the training set. 
model_10 <-lm(price~fuelsystem1bbl+compressionratio+CarNameaudi+fuelsystemmpfi+boreratio+CarNamejaguar
              +CarNamealpha+enginetypeohc+CarNamebuick+enginetypedohc+symboling+cylindernumbersix
              +highwaympg+horsepower+enginetypeohcv+cylindernumbereight+carlength+carheight,data=train)

# Check the summary of model. 
summary(model_10)
vif(model_10)


# highwaympg has high p-value
# refer Correlation Coef.txt file created above
# highwaympg is correlated to horsepower
# horsepower has low p-value, drop highwaympg
# Execute the model_11 multilinear model in the training set. 
model_11 <-lm(price~fuelsystem1bbl+compressionratio+CarNameaudi+fuelsystemmpfi+boreratio+CarNamejaguar
              +CarNamealpha+enginetypeohc+CarNamebuick+enginetypedohc+symboling+cylindernumbersix
              +horsepower+enginetypeohcv+cylindernumbereight+carlength+carheight,data=train)

# Check the summary of model. 
summary(model_11)
vif(model_11)


# removing highwaympg didnot make any major change in adj R square
# also VIF of horsepower marginally decreased.
# remove enginetypeohc with high p-value 
model_12 <-lm(price~fuelsystem1bbl+compressionratio+CarNameaudi+fuelsystemmpfi+boreratio+CarNamejaguar
              +CarNamealpha+CarNamebuick+enginetypedohc+symboling+cylindernumbersix
              +horsepower+enginetypeohcv+cylindernumbereight+carlength+carheight,data=train)
# Check the summary of model. 
summary(model_12)
vif(model_12)


# fuelsystemmpfi has high p-value
# refer Correlation Coef.txt file created above
# fuelsystemmpfi is correlated to horsepower
# horsepower high VIF suggests colinearity 
# drop fuelsystemmpfi
model_13 <-lm(price~fuelsystem1bbl+compressionratio+CarNameaudi+boreratio+CarNamejaguar+CarNamealpha
              +CarNamebuick+enginetypedohc+symboling+cylindernumbersix+horsepower+enginetypeohcv
              +cylindernumbereight+carlength+carheight,data=train)
# Check the summary of model. 
summary(model_13)
vif(model_13)



# fuelsystem1bbl has high p-value
# remove fuelsystem1bbl
model_14 <-lm(price~compressionratio+CarNameaudi+boreratio+CarNamejaguar+CarNamealpha
              +CarNamebuick+enginetypedohc+symboling+cylindernumbersix+horsepower+enginetypeohcv
              +cylindernumbereight+carlength+carheight,data=train)
# Check the summary of model. 
summary(model_14)
vif(model_14)


# refer Correlation Coef.txt file created above
# horsepower is correlated to boreratio
# drop boreratio
model_15 <-lm(price~compressionratio+CarNameaudi+CarNamejaguar+CarNamealpha+CarNamebuick+enginetypedohc
              +symboling+cylindernumbersix+horsepower+enginetypeohcv+cylindernumbereight+carlength
              +carheight,data=train)
# Check the summary of model. 
summary(model_15)
vif(model_15)



# refer Correlation Coef.txt file created above
# horsepower is correlated to carlength
# drop carlength
model_16 <-lm(price~compressionratio+CarNameaudi+CarNamejaguar+CarNamealpha+CarNamebuick+enginetypedohc
              +symboling+cylindernumbersix+horsepower+enginetypeohcv+cylindernumbereight+carheight,data=train)
# Check the summary of model. 
summary(model_16)
vif(model_16)


# symboling has high p-value
# drop symboling
model_17 <-lm(price~compressionratio+CarNameaudi+CarNamejaguar+CarNamealpha+CarNamebuick+enginetypedohc
              +cylindernumbersix+horsepower+enginetypeohcv+cylindernumbereight+carheight,data=train)
# Check the summary of model. 
summary(model_17)
vif(model_17)


# CarNamealpha has high p-value
# drop CarNamealpha
model_18 <-lm(price~compressionratio+CarNameaudi+CarNamejaguar+CarNamebuick+enginetypedohc
              +cylindernumbersix+horsepower+enginetypeohcv+cylindernumbereight+carheight,data=train)
# Check the summary of model. 
summary(model_18)
vif(model_18)


# refer Correlation Coef.txt file created above
# cylindernumbersix and horsepower are correlated
# drop cylindernumbersix
model_19 <-lm(price~compressionratio+CarNameaudi+CarNamejaguar+CarNamebuick+enginetypedohc
              +horsepower+enginetypeohcv+cylindernumbereight+carheight,data=train)
# Check the summary of model. 
summary(model_19)
vif(model_19)


# enginetypeohcv has high p-value
# drop enginetypeohcv
model_20 <-lm(price~compressionratio+CarNameaudi+CarNamejaguar+CarNamebuick+enginetypedohc
              +horsepower+cylindernumbereight+carheight,data=train)
# Check the summary of model. 
summary(model_20)
vif(model_20)


# cylindernumbereight has high p-value
# drop cylindernumbereight
model_21 <-lm(price~compressionratio+CarNameaudi+CarNamejaguar+CarNamebuick+enginetypedohc
              +horsepower+carheight,data=train)
# Check the summary of model. 
summary(model_21)
vif(model_21)


# CarNameaudi has high p-value
# drop CarNameaudi
model_22 <-lm(price~compressionratio+CarNamejaguar+CarNamebuick+enginetypedohc
              +horsepower+carheight,data=train)
# Check the summary of model. 
summary(model_22)
vif(model_22)


# refer Correlation Coef.txt file created above
# enginetypedohc is correlated to CarNamejaguar and CarNamebuick
# drop enginetypedohc
model_23 <-lm(price~compressionratio+CarNamejaguar+CarNamebuick+horsepower+carheight,data=train)
# Check the summary of model. 
summary(model_23)
vif(model_23)



# plot Residuals vs Fitted
plot(model_23,pch=16,which=1)



# Predict the house prices in the testing dataset
Predict_1 <- predict(model_23,test[,-1])
test$predicted_price <- Predict_1



# calculate r-squared
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared


# notice that the r-squared value for the model is 85.8% 
# the r-squared value for the test dataset is 71%. 
# Generally, a deviation of +(-) 5% in the R-squared value of the model for the test data is acceptable.
# However, if the deviation is significant (>5%) then we need to recheck the model.


# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(test$car_ID, test$price)) + geom_line(aes(color = "blue" )) + geom_line(aes(car_ID,predicted_price, color="red"))


