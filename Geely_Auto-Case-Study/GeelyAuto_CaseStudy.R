
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
price$fuelsystem <- str_replace(price$fuelsystem,"mfi","mpfi")

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
            & price$CarNamevolkswagen==0 & price$CarNamevolvo==0 & price$CarNamevw==0),]$CarNamealpha <- 1


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
model_2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + curbweight + stroke + carbodywagon + symboling4 + 
                symboling5 + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                CarNameaudi + CarNamebmw + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo + boreratio, data = train)
summary(model_2)
vif(model_2)

# remove boreratio since it has high p-value and VIF suggesting colinearity.
# Execute the model_3 multilinear model in the training set. 
model_3 <-lm(price~ aspiration + enginelocation + carlength + 
               carwidth + curbweight + stroke + carbodywagon + symboling4 + 
               symboling5 + drivewheelrwd + enginetypedohcv + enginetypel + 
               enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix + cylindernumberthree + 
               CarNameaudi + CarNamebmw + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
               CarNamevolvo ,data=train)

# Check the summary of model. 
summary(model_3)
vif(model_3)


# cylindernumberthree and symboling4 became insignificant
# cylindernumberthree has high VIF compared to symvoling4
# remove cylindernumberthree 
# Execute the model_4 multilinear model in the training set. 
model_4 <-lm(price~aspiration + enginelocation + carlength + 
               carwidth + curbweight + stroke + carbodywagon + symboling4 + 
               symboling5 + drivewheelrwd + enginetypedohcv + enginetypel + 
               enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix +  
               CarNameaudi + CarNamebmw + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
               CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_4)
vif(model_4)


# CarNameisuzu, drivewheelrwd, symboling4 become insignificant
# dropping drivewheelrwd since it has highest VIF among above
# Execute the  model_5 multilinear model in the training set. 
model_5 <-lm(price~aspiration + enginelocation + carlength + carwidth + curbweight + stroke + 
               carbodywagon + symboling4 + symboling5 + enginetypedohcv + enginetypel + enginetypeohc + 
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + CarNameaudi + CarNamebmw + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
               CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_5)
vif(model_5)


# carbodywagon, symboling4, symboling5 and enginetypeohc become insignificant
# dropping enginetypeohc since it has highest VIF among above
# Execute the  model_6 multilinear model in the training set. 
model_6 <-lm(price~aspiration + enginelocation + carlength + carwidth + curbweight + stroke + 
               carbodywagon + symboling4 + symboling5 + enginetypedohcv + enginetypel +  
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + CarNameaudi + CarNamebmw + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
               CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_6)
vif(model_6)


# symboling4, symboling5 become insignificant
# dropping symboling5 since it has highest VIF among above
# Execute the  model_7 multilinear model in the training set. 
model_7 <-lm(price~aspiration + enginelocation + carlength + carwidth + curbweight + stroke + 
               carbodywagon + symboling4 + enginetypedohcv + enginetypel +  
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + CarNameaudi + CarNamebmw + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
               CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_7)
vif(model_7)


# symboling4 become insignificant since it has high p-value 
# drop symboling4
# Execute the model_8 multilinear model in the training set. 
model_8 <-lm(price~aspiration + enginelocation + carlength + carwidth + curbweight + stroke + 
               carbodywagon + enginetypedohcv + enginetypel +  
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + CarNameaudi + CarNamebmw + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
               CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_8)
vif(model_8)


# carlength is related to curbweight and has high VIF 
# drop carlength
# Execute the model_9 multilinear model in the training set. 
model_9 <-lm(price~aspiration + enginelocation + carwidth + curbweight + stroke + 
               carbodywagon + enginetypedohcv + enginetypel +  
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + CarNameaudi + CarNamebmw + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
               CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_9)
vif(model_9)


# carbodywagon became insignificant i
# drop carbodywagon
# Execute the model_10 multilinear model in the training set. 
model_10 <-lm(price~aspiration + enginelocation + carwidth + curbweight + stroke + 
               enginetypedohcv + enginetypel +  
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + CarNameaudi + CarNamebmw + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
               CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
               CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
               CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_10)
vif(model_10)


# carwidth and curbweight are corelated
# drop carwidth since it correlation to price is less
# Execute the model_11 multilinear model in the training set. 
model_11 <-lm(price~aspiration + enginelocation + curbweight + stroke + 
                enginetypedohcv + enginetypel +  
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarNameaudi + CarNamebmw + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_11)
vif(model_11)


# stroke became insignificant with high VIF
# drop stroke 
# Execute the model_12 multilinear model in the training set. 
model_12 <-lm(price~aspiration + enginelocation + curbweight + enginetypedohcv + enginetypel +  
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CarNameaudi + CarNamebmw + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_12)
vif(model_12)


# cylindernumbersix negatively correlated to cylindernumberfour 
# drop cylindernumbersix since it is less ocrrelated ot price 
# Execute the model_13 multilinear model in the training set. 
model_13 <-lm(price~aspiration + enginelocation + curbweight + enginetypedohcv + enginetypel +  
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                CarNameaudi + CarNamebmw + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_13)
vif(model_13)


# enginetypedohcv, aspiration, enginetyperotor, CarNamebmw, CarNamejaguar are insignificant
# drop enginetyperotor since it is less correlated to price and highest VIF
# Execute the model_14 multilinear model in the training set. 
model_14 <-lm(price~aspiration + enginelocation + curbweight + enginetypedohcv + enginetypel +  
                enginetypeohcf + cylindernumberfive + cylindernumberfour + CarNameaudi + CarNamebmw + 
                CarNamedodge + CarNamehonda +   CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_14)
vif(model_14)


# enginetypedohcv, aspiration, CarNamebmw, CarNamejaguar are insignificant
# drop enginetypedohcv since it is less correlated to price 
# Execute the model_15 multilinear model in the training set. 
model_15 <-lm(price~aspiration + enginelocation + curbweight + enginetypel +  
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                CarNameaudi + CarNamebmw + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_15)
vif(model_15)


# aspiration, CarNamebmw, CarNamejaguar are insignificant
# drop aspiration since it is less correlated to price 
# Execute the model_16 multilinear model in the training set. 
model_16 <-lm(price~ enginelocation + curbweight + enginetypel +  
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                CarNameaudi + CarNamebmw + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_16)
vif(model_16)


# CarNamebmw, CarNamejaguar are insignificant
# drop CarNamebmw since it is less correlated to price 
# Execute the model_17 multilinear model in the training set. 
model_17 <-lm(price~ enginelocation + curbweight + enginetypel +  
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                CarNameaudi + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemaxda + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_17)
vif(model_17)


# CarNamejaguar is insignificant
# drop CarNamejaguar since it is less correlated to price 
# Execute the model_18 multilinear model in the training set. 
model_18 <-lm(price~ enginelocation + curbweight + enginetypel +  enginetypeohcf + cylindernumberfive + 
                cylindernumberfour + CarNameaudi + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemaxda + CarNamemazda + CarNamemercury + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_18)
vif(model_18)


# curbweight related to cylindernumberfour
# drop cylindernumberfour since it is less correlated to price 
# Execute the model_19 multilinear model in the training set. 
model_19 <-lm(price~ enginelocation + curbweight + enginetypel +  enginetypeohcf + cylindernumberfive + 
                CarNameaudi + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemaxda + CarNamemazda + CarNamemercury + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_19)
vif(model_19)


# enginelocation related to enginetypeohcf
# drop enginetypeohcf since it is less correlated to price 
# Execute the model_20 multilinear model in the training set. 
model_20 <-lm(price~ enginelocation + curbweight + enginetypel +  cylindernumberfive + 
                CarNameaudi + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemaxda + CarNamemazda + CarNamemercury + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_20)
vif(model_20)


# CarNamemercury, CarNamemaxda, CarNameaudi, cylindernumberfive are insignificant
# drop CarNamemercury since it is less correlated to price 
# Execute the model_21 multilinear model in the training set. 
model_21 <-lm(price~ enginelocation + curbweight + enginetypel +  cylindernumberfive + 
                CarNameaudi + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemaxda + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_21)
vif(model_21)


# CarNamemaxda, CarNameaudi, cylindernumberfive are insignificant
# drop CarNamemaxda since it is less correlated to price 
# Execute the model_22 multilinear model in the training set. 
model_22 <-lm(price~ enginelocation + curbweight + enginetypel +  cylindernumberfive + 
                CarNameaudi + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_22)
vif(model_22)


# CarNameaudi, cylindernumberfive are insignificant
# drop CarNameaudi since it is less correlated to price 
# Execute the model_23 multilinear model in the training set. 
model_23 <-lm(price~ enginelocation + curbweight + enginetypel +  cylindernumberfive + 
                CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_23)
vif(model_23)


# cylindernumberfive are insignificant
# drop cylindernumberfive since it is less correlated to price 
# Execute the model_24 multilinear model in the training set. 
model_24 <-lm(price~ enginelocation + curbweight + enginetypel +  CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_24)
vif(model_24)


# enginetypel corelated to curbweight
# drop enginetypel  
# Execute the model_25 multilinear model in the training set. 
model_25 <-lm(price~ enginelocation + curbweight +  CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_25)
vif(model_25)


# CarNamesaab, CarNamehonda, CarNamedodge,CarNameisuzu, CarNamemazda, CarNamenissan,CarNameplymouth,CarNamerenault
# CarNamevolkswagen, CarNamevolvo are insignificant
# drop CarNamesaab since it least effect price
# Execute the model_26 multilinear model in the training set. 

model_26 <-lm(price~ enginelocation + curbweight +  CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_26)
vif(model_26)

# CarNamedodge,CarNameisuzu, CarNamemazda, CarNamenissan,CarNameplymouth,CarNamerenault, CarNamehonda
# CarNamevolkswagen, CarNamevolvo are insignificant
# drop CarNamemazda since it least effect price
# Execute the model_27 multilinear model in the training set. 

model_27 <-lm(price~ enginelocation + curbweight +  CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_27)
vif(model_27)


# CarNamedodge,CarNameisuzu, CarNamenissan,CarNameplymouth,CarNamerenault, CarNamehonda
# CarNamevolkswagen, CarNamevolvo are insignificant
# drop CarNamerenault since it least effect price
# Execute the model_28 multilinear model in the training set. 

model_28 <-lm(price~ enginelocation + curbweight +  CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_28)
vif(model_28)


# CarNamedodge,CarNameisuzu, CarNamenissan,CarNameplymouth,CarNamevolkswagen, 
# CarNamehonda, CarNamevolvo are insignificant
# drop CarNameisuzu since it least effect price
# Execute the model_29 multilinear model in the training set. 

model_29 <-lm(price~ enginelocation + curbweight +  CarNamedodge + CarNamehonda + CarNamemitsubishi + 
                CarNamenissan + CarNameplymouth + CarNametoyota + CarNamevolkswagen + 
                CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_29)
vif(model_29)


# CarNamedodge, CarNamenissan,CarNameplymouth,CarNamevolkswagen, CarNamevolvo, CarNamehonda are insignificant
# drop CarNamevolkswagen since it least effect price
# Execute the model_30 multilinear model in the training set. 
model_30 <-lm(price~ enginelocation + curbweight +  CarNamedodge + CarNamehonda + CarNamemitsubishi + 
                CarNamenissan + CarNameplymouth + CarNametoyota + CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_30)
vif(model_30)


# CarNamedodge, CarNameplymouth, CarNamemitsubishi, CarNamevolvo, CarNamehonda are insignificant
# drop CarNamenissan since it least effect price
# Execute the model_31 multilinear model in the training set. 
model_31 <-lm(price~ enginelocation + curbweight +  CarNamedodge + CarNamehonda + CarNamemitsubishi + 
                CarNameplymouth + CarNametoyota + CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_31)
vif(model_31)


# CarNamedodge, CarNameplymouth, CarNamemitsubishi, CarNamevolvo, CarNamehonda are insignificant
# drop CarNameplymouth since it least effect price
# Execute the model_32 multilinear model in the training set. 

model_32 <-lm(price~ enginelocation + curbweight +  CarNamedodge + CarNamehonda + CarNamemitsubishi + 
                CarNametoyota + CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_32)
vif(model_32)


# CarNamedodge, CarNamevolvo, CarNamehonda, CarNamemitsubishi are insignificant
# drop CarNamemitsubishi since it least effect price
# Execute the model_33 multilinear model in the training set. 
model_33 <-lm(price~ enginelocation + curbweight +  CarNamedodge + CarNamehonda + CarNametoyota + CarNamevolvo,data=train)

# Check the summary of model. 
summary(model_33)
vif(model_33)


# CarNamedodge, CarNamevolvo, CarNamehonda are insignificant
# drop CarNamevolvo since it least effect price
# Execute the model_34 multilinear model in the training set. 
model_34 <-lm(price~ enginelocation + curbweight +  CarNamedodge + CarNamehonda + CarNametoyota, data=train)

# Check the summary of model. 
summary(model_34)
vif(model_34)


# CarNamedodge, CarNamehonda are insignificant
# drop CarNamedodge since it least effect price
# Execute the model_35 multilinear model in the training set. 
model_35 <-lm(price~ enginelocation + curbweight + CarNamehonda + CarNametoyota, data=train)

# Check the summary of model. 
summary(model_35)
vif(model_35)


# CarNamehonda are insignificant
# drop CarNamehonda since it least effect price
# Execute the model_36 multilinear model in the training set. 
model_36 <-lm(price~ enginelocation + curbweight + CarNametoyota, data=train)

# Check the summary of model. 
summary(model_36) 
# Adjusted R-squared:  0.7737
# no linear dependedncy
vif(model_36)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_36,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.6426946
# Difference between predicted r-square and r-square of model_36
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared



# Generally, a deviation of +(-) 5% in the R-squared value of the model for the test data is acceptable.
# However, if the deviation is significant (>5%) then we need to recheck the model.


# Add symboling4 and check r squared for model
model_37 <-lm(price~ enginelocation + curbweight + CarNametoyota + symboling4, data=train)

# Check the summary of model.
# Adjusted R-squared:  0.7795
# Predictory variables: all significant
# No multicolinearity between variables
summary(model_37)
vif(model_37)

# plot Residuals vs Fitted
plot(model_37,pch=16,which=1)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_37,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.6503431
# Difference between predicted r-square and r-square of model_36
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

test$carID <- seq(1,62,1)
# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + 
  geom_path(aes(test$carID, test$predicted_price, color="red"))



# Add enginetypel and check r squared for model 
model_38 <-lm(price~ enginelocation + curbweight + CarNametoyota + symboling1 + enginetypel 
              , data=train)

# Check the summary of model. 
summary(model_38)
vif(model_38)

# symboling1 became insignificant
# drop symboling 1
model_39 <-lm(price~ enginelocation + curbweight + CarNametoyota + enginetypel 
              , data=train)

# Check the summary of model. 
# Adjusted R-squared:  0.8005
# Predictor variables: all significant
summary(model_39)
vif(model_39)

# plot Residuals vs Fitted
plot(model_39,pch=16,which=1)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_39,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.6930148
# Difference between predicted r-square and r-square decreeasing
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

test$carID <- seq(1,62,1)
# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + 
  geom_path(aes(test$carID, test$predicted_price, color="red"))


# Add carbodywagon and check r squared for model 
model_40 <-lm(price~ enginelocation + curbweight + CarNametoyota + enginetypel +carbodywagon, data=train)

# Check the summary of model. 
# Adjusted R-squared:  0.8184
summary(model_40)
vif(model_40)

# plot Residuals vs Fitted
plot(model_40,pch=16,which=1)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_40,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.7282913
# Difference between predicted r-square and r-square decreasing
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

test$carID <- seq(1,62,1)
# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + 
  geom_path(aes(test$carID, test$predicted_price, color="red"))



# Add carbodywagon and check r squared for model 
model_41 <-lm(price~ enginelocation + curbweight + CarNametoyota + enginetypel +carbodywagon +aspiration, data=train)

# Check the summary of model. 
# Adjusted R-squared:  0.8264
summary(model_41)
vif(model_41)

# plot Residuals vs Fitted
plot(model_41,pch=16,which=1)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_41,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.7392311
# Difference between predicted r-square and r-square decreasing
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

test$carID <- seq(1,62,1)
# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + 
  geom_path(aes(test$carID, test$predicted_price, color="red"))




# Add carbodywagon and check r squared for model 
model_42 <-lm(price~ enginelocation + curbweight + CarNametoyota + enginetypel +carbodywagon +aspiration
              +symboling4, data=train)

# Check the summary of model. 
# Adjusted R-squared:  0.8304
summary(model_42)
vif(model_42)

# plot Residuals vs Fitted
plot(model_42,pch=16,which=1)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_42,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.7452911
# Difference between predicted r-square and r-square decreasing
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

test$carID <- seq(1,62,1)
# Plot - Actual vs Predicted Views Model9
ggplot(test, aes(test$carID,test$price)) + geom_path(aes(color = "blue" )) + 
  geom_path(aes(test$carID, test$predicted_price, color="red"))



model_43 <-lm(price~ enginelocation + curbweight + CarNametoyota + CarNamebmw + enginetypel +
                carbodywagon+aspiration +symboling4 ,data=train)
# Check the summary of model. 
# Adjusted R-squared:  0.8396
summary(model_43)
vif(model_43)

# plot Residuals vs Fitted
plot(model_43,pch=16,which=1)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_43,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.8059223
# Difference between predicted r-square and r-square decreasing
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared



model_44 <-lm(price~ enginelocation + curbweight + CarNametoyota + CarNamebmw + enginetypel + CarNamejaguar+
                carbodywagon+aspiration +symboling4 ,data=train)
# Check the summary of model. 
summary(model_44)
vif

# aspiration becomes insignificant
# dropping aspiration
model_45 <-lm(price~ enginelocation + curbweight + CarNametoyota + CarNamebmw + enginetypel + 
                CarNamejaguar+ carbodywagon+ symboling4 ,data=train)
# Check the summary of model. 
# Adjusted R-squared:  0.8435
summary(model_45)
vif(model_45)

# plot Residuals vs Fitted
plot(model_45,pch=16,which=1)

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_45,test[,-1])
test$predicted_price <- Predict_1

# calculate r-squared: 0.8306239
# Difference between predicted r-square and r-square decreasing
r <- cor(test$price,test$predicted_price)
rsquared <- cor(test$price,test$predicted_price)^2
rsquared

test$carID <- seq(1,62,1)
# Plot - Actual vs Predicted Views Model9
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


