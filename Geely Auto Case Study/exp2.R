
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

# doornumber:  four - 1; two - 0;
levels(price$doornumber) <-c(1,0)
price$doornumber <- as.numeric(levels(price$doornumber))[price$doornumber]

# enginelocation: front - 1; rear - 0
levels(price$enginelocation) <-c(1,0)
price$enginelocation <- as.numeric(levels(price$enginelocation))[price$enginelocation]

# symboling: convert to factor
# assign scale from 1 to 6
# -2 -> 1, -1 -> 2, 0 -> 3, 1 -> 4, 2 -> 5, 3 -> 6
price$symboling <- as.factor(price$symboling)
levels(price$symboling) <-c(1:6)
price$symboling <- as.numeric(levels(price$symboling))[price$symboling]


##################### Dummy Variables Creation ######################

#Converting "carbody" into dummies . 
dummy_carbody <- data.frame(model.matrix( ~carbody, data = price))

#This column should be removed from the newly created dummy_carbody dataframe.
dummy_carbody <- dummy_carbody[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
price <- cbind(price[,-7], dummy_carbody)

# computing remaining dummy variable type since number of dummy variables generated  will be one less
price$carbodyconvertible <- 0
price[which(price$carbodyhardtop==0 & price$carbodyhatchback==0 & price$carbodysedan==0 & price$carbodywagon==0),]$carbodyconvertible <- 1


#Converting "symboling" into dummies . 
price$symboling <- as.factor(price$symboling)
dummy_symboling <- data.frame(model.matrix(~symboling, data = price))

#This column should be removed from the newly created dataframe. 
dummy_symboling <- dummy_symboling[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "symboling" column
price <- cbind(price[,-2], dummy_symboling)

# computing remaining dummy variable type since number of dummy variables generated  will be one less
price$symboling1 <- 0
price[which(price$symboling2==0 & price$symboling3==0 & price$symboling4==0 & price$symboling5==0 & price$symboling6==0),]$symboling1 <- 1


#Converting "drivewheel" into dummies . 
dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = price))

#This column should be removed from the newly created dataframe. 
dummy_drivewheel <- dummy_drivewheel[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "drivewheel" column
price <- cbind(price[,-6], dummy_drivewheel)

# computing remaining dummy variable type since number of dummy variables generated  will be one less
price$drivewheel4wd <- 0
price[which(price$drivewheelfwd==0 & price$drivewheelrwd==0),]$drivewheel4wd <- 1


#Converting "enginetype" into dummies . 
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = price))

#This column should be removed from the newly created dataframe. 
dummy_enginetype <- dummy_enginetype[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "enginetype" column
price <- cbind(price[,-12], dummy_enginetype)

# computing remaining dummy variable type since number of dummy variables generated  will be one less
price$enginetypedohc <- 0
price[which(price$enginetypedohcv==0 & price$enginetypel==0 & price$enginetypeohc==0 & price$enginetypeohcf==0
            & price$enginetypeohcv==0 & price$enginetyperotor==0),]$enginetypedohc <- 1


#Converting "cylindernumber" into dummies . 
dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = price))

#This column should be removed from the newly created dataframe. 
dummy_cylindernumber <- dummy_cylindernumber[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "clindernumber" column
price <- cbind(price[,-12], dummy_cylindernumber)

# computing remaining dummy variable type since number of dummy variables generated  will be one less
price$cylindernumbereight <- 0
price[which(price$cylindernumberfive ==0 & price$cylindernumberfour==0 & price$cylindernumbersix==0 
            & price$cylindernumberthree==0 & price$cylindernumbertwelve==0 & price$cylindernumbertwo==0),]$cylindernumbereight <- 1


# Data Cleaning - mfi and mpfi are same. Replace all mfi with mpfi
# price$fuelsystem <- str_replace(price$fuelsystem,"mfi","mpfi")

#Converting "fuelsystem" into dummies . 
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = price))

#This column should be removed from the newly created dataframe. 
dummy_fuelsystem <- dummy_fuelsystem[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "fuelsystem" column
price <- cbind(price[,-13], dummy_fuelsystem)

# computing remaining dummy variable type since number of dummy variables generated  will be one less
price$fuelsystem1bbl <- 0
price[which(price$fuelsystem2bbl==0 & price$fuelsystem4bbl==0 & price$fuelsystemidi==0 & price$fuelsystemmpfi==0 & price$fuelsystemspdi==0 & price$fuelsystemspfi==0),]$fuelsystem1bbl <- 1


# Data Cleaning - convert all CarnNAme values to lowercase
#               - trim all characters after first space to get company name
#               - for values containg "-", trim all characters after first "-"
price$CarName <- gsub("\\s.*","",price$CarName)
price$CarName <- gsub("-.*","",price$CarName)
price$CarName <- tolower(as.factor(price$CarName))

# Data Cleaning - typo - replace all instances of 'porcshce' by 'porsche'
#                      - replace all instances of 'vokswagen' by 'volkswagen'
#                      - replace all instances of 'toyouta' by 'toyota'
price$CarName <- str_replace(price$CarName,"porcshce","porsche")
price$CarName <- str_replace(price$CarName,"vokswagen","volkswagen")
price$CarName <- str_replace(price$CarName,"vw","volkswagen")
price$CarName <- str_replace(price$CarName,"toyouta","toyota")


#Converting "carName" into dummies . 
dummy_carName <- data.frame(model.matrix( ~CarName, data = price))

#This column should be removed from the newly created dataframe. 
dummy_carName <- dummy_carName[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "CarName" column
price <- cbind(price[,-2], dummy_carName)

# computing remaining dummy variable type since number of dummy variables generated  will be one less
price$CarNamealpha <- 0
price[which(price$CarNameaudi==0 & price$CarNamebmw==0 & price$CarNamebuick==0 & price$CarNamechevrolet==0 
            & price$CarNamedodge==0 & price$CarNamehonda==0 & price$CarNameisuzu==0 & price$CarNamejaguar==0
            & price$CarNamemaxda==0 & price$CarNamemazda==0 & price$CarNamemercury==0 & price$CarNamemitsubishi==0
            & price$CarNamenissan==0 & price$CarNamepeugeot==0 & price$CarNameplymouth==0 & price$CarNameporsche==0
            & price$CarNamesaab==0 & price$CarNamerenault==0 & price$CarNamesubaru==0 & price$CarNametoyota==0
            & price$CarNamevolkswagen==0 & price$CarNamevolvo==0),]$CarNamealpha <- 1


# check for duplicates
price <- unique(price)

# check for NA values
na_count <- sum(is.na(price))

# drop carID since it is not a predictor variable
price <- price[,-1]

# check summary of price dataframe to understand distribution of variables
sumry <- summary(price)

#################### Check for Outliners ######################

# outliner check for wheelbase
# suggests no outliner
quantile(price$wheelbase,seq(0,1,.01))


# outliner check for curbweight
quantile(price$curbweight,seq(0,1,.01))
# jump at 95% to 96%, replacing all greater than 3503.00 with 3503.00
price$curbweight[which(price$curbweight>3503.00)] <- 3503.00

# outliner check for enginesize
quantile(price$enginesize,seq(0,1,.01))
# jump at 93% to 94%, replacing all greater than 194.00 with 194.00
price$enginesize[which(price$enginesize>194.00)] <- 194.00

# outliner check for compressionratio
quantile(price$compressionratio,seq(0,1,.01))
# jump at 90% to 91%, replacing all greater than 10.9400 with 10.94
price$compressionratio[which(price$compressionratio>10.94)] <- 10.94

# outliner check for horsepower
quantile(price$horsepower,seq(0,1,.01))
# jump at 93% to 94%, replacing all greater than 162.00 with 162.00
price$horsepower[which(price$horsepower>162.00)] <- 162.00

# outliner check for peakrpm
quantile(price$peakrpm,seq(0,1,.01))
# jump at 93% to 94%, replacing all greater than 5800 with 5800
price$peakrpm[which(price$peakrpm>5800)] <- 5800

# outliner check for citympg
quantile(price$citympg,seq(0,1,.01))
# jump at 98% to 99%, replacing all greater than 38.00 with 38.00
price$citympg[which(price$citympg>38.00)] <- 38.00

# outliner check for highwaympg
quantile(price$highwaympg,seq(0,1,.01))
# jump at 96% to 97%, replacing all greater than 43.00 with 43.00
price$highwaympg[which(price$highwaympg>43.00)] <- 43.00


View(price)

# check how price variable is distributed
ggplot(price,aes(price))+geom_histogram(bins=20)


#price<- scale(price,center = FALSE,scale=TRUE)


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

#sink("Correlation_Coef.txt",append=FALSE,split=FALSE)
#options(max.print=999999)
cor_df <- cor(price)
cor_df
#sink()

#generate a visual correlation matrix
cor_matrix_dataframe <- price
cor_matrix <- round(cor(cor_matrix_dataframe),2)
png(height=2100, width=2100, pointsize=30, file="Correlation_Plot.png")
color <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                            "cyan", "#007FFF", "blue","#00007F")) 
#cor_plot <- corrplot(cor_matrix, na.label="NA",method="number",number.cex=1,addCoef.col = "red", title = "Correalation Matrix")
cor_plot <- corrplot(cor_matrix, na.label="NA",method="color",col=color(10),
                     number.cex = 1, order="hclust",title = "Correalation Matrix")
dev.off()



######################## Model Generation ##########################

#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=train)

# Check the summary of model. 
# NA values suggest of linear dependiencies among variables.
summary(model_1)


#Execute the first model_1 with stepAIC in the training set. 
step <- stepAIC(model_1, direction="both")
step

# retaining variables selected by stepAIC. 
model_2 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                carwidth + curbweight + enginesize + stroke + compressionratio + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                symboling3 + symboling4 + symboling5 + drivewheelrwd + enginetypel + 
                enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen, data = train)
summary(model_2)
vif(model_2)

# symboling3, symboling5, CarNamesaab,carbodyhardtop, CarNamehonda,CarNamemercury are insignificant
# remove symboling3 since it is insignificant and VIF suggesting colinearity
# Execute the model_3 multilinear model in the training set. 
model_3 <-lm(price~ aspiration + enginelocation + wheelbase + 
               carwidth + curbweight + enginesize + stroke + compressionratio + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               symboling4 + symboling5 + drivewheelrwd + enginetypel + 
               enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
               CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen ,data=train)

# Check the summary of model. 
summary(model_3)
vif(model_3)


# symboling5, CarNamemercury,carbodyhardtop, CarNamehonda are insignificant
# symboling5 has high VIF and is insignificant
# remove symboling5 
# Execute the model_4 multilinear model in the training set. 
model_4 <-lm(price~aspiration + enginelocation + wheelbase + 
               carwidth + curbweight + enginesize + stroke + compressionratio + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               symboling4 + drivewheelrwd + enginetypel + 
               enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
               CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_4)
vif(model_4)


# CarNamemercury,carbodyhardtop, carbodysedan, stroke, wheelbase, CarNamesaab are insignificant
# dropping wheelbase since it has high VIF compared to stroke 
# Execute the  model_5 multilinear model in the training set. 
model_5 <-lm(price~aspiration + enginelocation +  
               carwidth + curbweight + enginesize + stroke + compressionratio + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               symboling4 + drivewheelrwd + enginetypel + 
               enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
               CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_5)
vif(model_5)


# CarNamemercury, symboling4, carbodywagon, carbodysedan, carbodyhatchback, carbodyhardtop, stroke
# CarNamesaab are insignificant
# dropping stroke since it has high VIF 
# Execute the  model_6 multilinear model in the training set. 
model_6 <-lm(price~aspiration + enginelocation + carwidth + curbweight + enginesize + compressionratio + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               symboling4 + drivewheelrwd + enginetypel + 
               enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
               CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_6)
vif(model_6)


# CarNamemercury, symboling4, carbodysedan, carbodyhatchback, carbodyhardtop, CarNamesaab are insignificant
# dropping carbodysedan since it has highest VIF and least related with price
# Execute the  model_7 multilinear model in the training set. 
model_7 <-lm(price~aspiration + enginelocation + carwidth + curbweight + enginesize + compressionratio + 
               carbodyhardtop + carbodyhatchback + carbodywagon + 
               symboling4 + drivewheelrwd + enginetypel + 
               enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
               CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_7)
vif(model_7)


# CarNamemercury, symboling4, carbodywagon, carbodyhatchback, carbodyhardtop, CarNamesaab are insignificant
# drop CarNamesaab since it has high VIF and correlated to compressionRatio
# Execute the model_8 multilinear model in the training set. 
model_8 <-lm(price~aspiration + enginelocation + carwidth + curbweight + enginesize + compressionratio + 
               carbodyhardtop + carbodyhatchback + carbodywagon + 
               symboling4 + drivewheelrwd + enginetypel + 
               enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
               CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_8)
vif(model_8)


# CarNamemercury, carbodywagon, carbodyhatchback, carbodyhardtop are insignificant
# carbodyhardtop is related to enginelocation and has high VIF 
# drop carbodyhardtop
# Execute the model_9 multilinear model in the training set. 
model_9 <-lm(price~aspiration + enginelocation + carwidth + curbweight + enginesize + compressionratio + 
               carbodyhatchback + carbodywagon + symboling4 + drivewheelrwd + enginetypel + 
               enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
               CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_9)
vif(model_9)


# CarNamemercury, carbodywagon, carbodyhatchback are insignificant
# drop carbodywagon since it is least related to price
# Execute the model_10 multilinear model in the training set. 
model_10 <-lm(price~aspiration + enginelocation + carwidth + curbweight + enginesize + compressionratio + 
                carbodyhatchback + symboling4 + drivewheelrwd + enginetypel + 
                enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_10)
vif(model_10)


# CarNamemercury, carbodyhatchback are insignificant
# drop CarNamemercury since it correlation to price is less
# Execute the model_11 multilinear model in the training set. 
model_11 <-lm(price~aspiration + enginelocation + carwidth + curbweight + enginesize + compressionratio + 
                carbodyhatchback + symboling4 + drivewheelrwd + enginetypel + 
                enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_11)
vif(model_11)


# symboling4, carbodyhatchback are insignificant
# drop symboling4 since it is related to curbweight
# Execute the model_12 multilinear model in the training set. 
model_12 <-lm(price~aspiration + enginelocation + carwidth + curbweight + enginesize + compressionratio + 
                carbodyhatchback + drivewheelrwd + enginetypel + 
                enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_12)
vif(model_12)


# carbodyhatchback are insignificant
# drop carbodyhatchback 
# Execute the model_13 multilinear model in the training set. 
model_13 <-lm(price~aspiration + enginelocation + carwidth + curbweight + enginesize + compressionratio + 
                drivewheelrwd + enginetypel + enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_13)
vif(model_13)


# all variables are significant but some have high VIF
# drop carwidht since it is correlated to curbweight and is less related to price in comparison to carwidth
# Execute the model_14 multilinear model in the training set. 
model_14 <-lm(price~aspiration + enginelocation + curbweight + enginesize + compressionratio + 
                drivewheelrwd + enginetypel + enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_14)
vif(model_14)


# compressionratio is insignificant
# drop compressionratio 
# Execute the model_15 multilinear model in the training set. 
model_15 <-lm(price~aspiration + enginelocation + curbweight + enginesize + drivewheelrwd + 
                enginetypel + enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_15)
vif(model_15)


# all variables are significant but some have high VIF suggesting correlation
# drop curbweight since it is correalted to enginesize 
# Execute the model_16 multilinear model in the training set. 
model_16 <-lm(price~ aspiration + enginelocation + enginesize + drivewheelrwd + 
                enginetypel + enginetypeohc + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_16)
vif(model_16)


# all variables are significant but some have high VIF suggesting correlation
# drop drivewheelrwd since it is correlated to enginesize 
# Execute the model_17 multilinear model in the training set. 
model_17 <-lm(price~ aspiration + enginelocation + enginesize + enginetypel + enginetypeohc + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_17)
vif(model_17)


# all variables are significant but some have high VIF suggesting correlation
# drop cylindernumberfour since iorrealted to enginesize
# Execute the model_18 multilinear model in the training set. 
model_18 <-lm(price~ aspiration + enginelocation + enginesize + enginetypel + enginetypeohc + 
                enginetypeohcf + cylindernumberfive + cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_18)
vif(model_18)


# drop cylindernumberfive since it is insignificant
# Execute the model_19 multilinear model in the training set. 
model_19 <-lm(price~ aspiration + enginelocation + enginesize + enginetypel + enginetypeohc + 
                enginetypeohcf + cylindernumbersix + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_19)
vif(model_19)


# all variables are singnificant but some have high VIF suggesting corelation
# drop cylindernumbersix since it is correlated to enginesize 
# Execute the model_20 multilinear model in the training set. 
model_20 <-lm(price~ aspiration + enginelocation + enginesize + enginetypel + enginetypeohc + 
                enginetypeohcf + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_20)
vif(model_20)


# all variables are singnificant but some have high VIF suggesting corelation
# drop enginetypeohc since it is correlated to enginesize 
# Execute the model_21 multilinear model in the training set. 
model_21 <-lm(price~ aspiration + enginelocation + enginesize + enginetypel +  
                enginetypeohcf + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_21)
vif(model_21)


# aspiration is insignificant
# drop aspiration 
# Execute the model_22 multilinear model in the training set. 
model_22 <-lm(price~ enginelocation + enginesize + enginetypel +  
                enginetypeohcf + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_22)
vif(model_22)


# all variables are singnificant but some have high VIF suggesting corelation
# drop enginetypeohcf since it is correlated to enginelocation 
# Execute the model_23 multilinear model in the training set. 
model_23 <-lm(price~ enginelocation + enginesize + enginetypel +  
                CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_23)
vif(model_23)


# CarNamemazda, enginetypel are insignificant
# drop enginetypel since it is less correlated to price 
# Execute the model_24 multilinear model in the training set. 
model_24 <-lm(price~ enginelocation + enginesize + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_24)
vif(model_24)


# CarNamemazda, CarNamevolkswagen are insignificant
# drop CarNamemazda since it is less correlated to price 
# Execute the model_25 multilinear model in the training set. 
model_25 <-lm(price~ enginelocation + enginesize + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemaxda + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_25)
vif(model_25)


# CarNamemaxda, CarNamevolkswagen are insignificant
# drop CarNamemaxda since it least effect price
# Execute the model_26 multilinear model in the training set. 

model_26 <-lm(price~ enginelocation + enginesize + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota + CarNamevolkswagen,data=train)

# Check the summary of model. 
summary(model_26)
vif(model_26)

# CarNamevolkswagen is insignificant
# drop CarNamevolkswagen since it least effect price
# Execute the model_27 multilinear model in the training set. 

model_27 <-lm(price~ enginelocation + enginesize + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota ,data=train)

# Check the summary of model. 
# Adjusted R-squared:  0.8666 
summary(model_27)
vif(model_27)


# plot Residuals vs Fitted
plot(model_27,pch=16,which=1)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_27,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.8104795
# Difference between predicted r-square and r-square in limits
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

test$carID <- seq(1,62,1)
# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + geom_path(aes(test$carID, test$predicted_price, color="red"))

#generate a visual correlation matrix for car Company
# no company is correlated to another
cor_carName <- cor_df[c(55:77),c(55:77)]
cor_car_matrix <- round(cor(cor_carName),2)
png(height=2100, width=2100, pointsize=30, file="Correlation_CarName_Plot.png")
color_car <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                            "cyan", "#007FFF", "blue","#00007F")) 
#cor_plot <- corrplot(cor_matrix, na.label="NA",method="number",number.cex=1,addCoef.col = "red", title = "Correalation Matrix")
cor_car_plot <- corrplot(cor_car_matrix, na.label="NA",method="color",col=color(10),
                     number.cex = 1, order="hclust",title = "Correalation Matrix")
dev.off()


# all variables are significant but some have high VIF suggesting corelation
# enginelocation is corelated to engine size
# drop enginelocation since it least effect price
# Execute the model_28 multilinear model in the training set. 

model_28 <-lm(price~ enginesize + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNamejaguar + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota,data=train)

# Check the summary of model. 
summary(model_28)
vif(model_28)


# CarNamehonda is insignificant
# drop CarNamehonda 
# Execute the model_29 multilinear model in the training set. 

model_29 <-lm(price~ enginesize + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamejaguar + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota,data=train)

# Check the summary of model. 
# Adjusted R-squared:  0.8302
summary(model_29)
vif(model_29)

# plot Residuals vs Fitted
plot(model_29,pch=16,which=1)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_29,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.7983493
# Difference between predicted r-square and r-square in limits
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared


# Plot - Actual vs Predicted Views Model29
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + geom_path(aes(test$carID, test$predicted_price, color="red"))



# CarNamebuick corelated to enginesize
# drop CarNamebuick
model_30 <-lm(price~ enginesize + CarNamebmw + CarNamedodge + 
                CarNamejaguar + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota,data=train)

# Check the summary of model. 
# Adjusted R-squared:  0.7865
summary(model_30)
vif(model_30)

# plot Residuals vs Fitted
plot(model_30,pch=16,which=1)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_30,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.7620014
# Difference between predicted r-square and r-square in limits
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

# Plot - Actual vs Predicted Views Model29
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + geom_path(aes(test$carID, test$predicted_price, color="red"))


# CarNamejaguar and  CarNamebmw are insignificant
# drop CarNamebmw 
model_31 <-lm(price~ enginesize + CarNamedodge + CarNamejaguar + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota,data=train)

# Check the summary of model. 
# Adjusted R-squared:  0.7829
summary(model_31)
vif(model_31)

# plot Residuals vs Fitted
plot(model_31,pch=16,which=1)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_31,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.7280417
# Difference between predicted r-square and r-square in limits
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

# Plot - Actual vs Predicted Views Model29
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + geom_path(aes(test$carID, test$predicted_price, color="red"))



# CarNamejaguar is insignificant
# drop CarNamejaguar 
model_32 <-lm(price~ enginesize + CarNamedodge + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota,data=train)

# Check the summary of model. 
# Adjusted R-squared:  0.7792
summary(model_32)
vif(model_32)

# plot Residuals vs Fitted
plot(model_32,pch=16,which=1)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_32,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.7120289
# Difference between predicted r-square and r-square in limits
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

# Plot - Actual vs Predicted Views Model
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + geom_path(aes(test$carID, test$predicted_price, color="red"))




# symboling6, carbodyconvertible, carbodyhatchback, symboling2, aspiration added one by one
# all of them are insignificant when added to model_32
# symboling5 when added still significant 
# difference between predicted r-square(0.7084259) and adj r-square of model (0.7866)

model_33 <-lm(price~ enginesize + symboling5 + CarNamedodge + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNametoyota,data=train)

# Check the summary of model. 
summary(model_33)
vif(model_33)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_33,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.7120289
# Difference between predicted r-square and r-square in limits
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

# Plot - Actual vs Predicted Views Model
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + geom_path(aes(test$carID, test$predicted_price, color="red"))



# performing one step at a time of adding and removing variable
model_34 <-lm(price~ enginesize + symboling5 + fuelsystemspdi+   CarNamenissan + CarNametoyota,data=train)

# Check the summary of model. 
# Adjusted R-squared:  0.7587
summary(model_34)
vif(model_34)


# Predict the house prices in the testing dataset
Predict_1 <- predict(model_34,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.7198685
# Difference between predicted r-square and r-square in limits
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared



model_35 <-lm(price~ enginesize + symboling5 + fuelsystemspdi+ fuelsystemmfi+   CarNamenissan +  
                CarNametoyota,data=train)

# Check the summary of model. 
# Adjusted R-squared:  0.7647
summary(model_35)
vif(model_35)


# Predict the house prices in the testing dataset
Predict_1 <- predict(model_35,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.7202201
# Difference between predicted r-square and r-square in limits
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

# Plot - Actual vs Predicted Views Model
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + geom_path(aes(test$carID, test$predicted_price, color="red"))



model_37 <-lm(price~ enginesize + symboling5 + fuelsystemspdi+ fuelsystemmfi + CarNamerenault  + enginelocation+
 +    CarNamenissan +  
                CarNametoyota,data=train)

# Check the summary of model. 
# Adjusted R-squared:  0.7795
summary(model_37)
vif(model_37)

# plot Residuals vs Fitted
plot(model_37,pch=16,which=1)


# Predict the house prices in the testing dataset
Predict_1 <- predict(model_37,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.721641
# Difference between predicted r-square and r-square in limits
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

# Plot - Actual vs Predicted Views Model
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + geom_path(aes(test$carID, test$predicted_price, color="red"))


# plot Residuals vs Fitted
plot(model_36,pch=16,which=1)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_37,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.7120289
# Difference between predicted r-square and r-square in limits
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

test$carID <- seq(1,62,1)
# Plot - Actual vs Predicted Views Model
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + geom_path(aes(test$carID, test$predicted_price, color="red"))



######################## Conclusion ##############################

#  Final model shows below independent variables for dependent variable 'price':

#          - enginelocation (value 1 or 0)
#          - curbweight (continuous variable)
#          - symboling value 1. (symboling with value 4 maps to 1 on original scale. value can be 1 or 0)
#          - enginetypel. (value can be 1 or 0)
#          - carbody, if it is a wagon. (value can be 1 or 0)
#          - car company if Toyota, BMW, Jaguar 
#                 - CarNametoyota (value can be 1 or 0)
#                 - CarNamebmw (value can be 1 or 0)
#                 - CarNamejaguar(value can be 1 or 0)
#
#  
#  Consideration: enginetypel is weakly correalted to curbweight, hence used in model.
#
#
#  - one unit increase in curbweight, increase the price by 1.378e+01 
#    and decrease by same if curbweight decrease by one unit change.
#
#  - if car is from BMW company. price increase by 6.8426+03. BMW cars have higher price than mean predicted price.
#
#  - if car is from jaguar, price increase by 7.296e+03. Jaguars cars have higher price than mean predicted price.
#
#  - if car is form toyota, price decrease by 2.213e+03. Toyota have lower price from mean predicted price.
#
#  - if car has symboling value '1', price increase by 1.517e+03.
#
#  - if enginetype is l style type, price decrease by 4.778e+03. cars with l type engines have low price compared with mean predicted price.
#
#  - if engine is located at front, price decrease by 1.801e+04.
#
#  - if car is a wagon car, its price decrease by 2.930e+03.
#
#
#  Predictor Model Equation:
#
#  price = -3.638e+03 + (1.378e+01)*curbweight + (6.842e+03)*CarNamebmw + (7.296e+03)*CarNamejaguar +
#           (1.517e+03)*symboling4 - (1.801e+04)*enginelocation - (2.213e+03)*CarNametoyota -
#           (4.778e+03)*enginetypel - (2.930e+03)*carbodywagon






model_46 <-lm(price~ enginelocation + curbweight + CarNametoyota + CarNamebmw + enginetypel +   
                CarNamejaguar+ carbodywagon ,data=train)
# Check the summary of model. 
# Adjusted R-squared:  0.8386
summary(model_46)
vif(model_46)

# plot Residuals vs Fitted
plot(model_46,pch=16,which=1)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_46,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.8271527
# Difference between predicted r-square and r-square decreasing
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

test$carID <- seq(1,62,1)
# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + geom_path(aes(test$carID, test$predicted_price, color="red"))



