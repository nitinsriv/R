
library(kernlab)
library(caret)


# read input file for test and train dataset.
train_file <- read.csv("mnist_train.csv")
test_file <- read.csv("mnist_test.csv")

# identiy and name the classifier column as "digit". this is the output variable
colnames(train_file)[1] <- "digit"
colnames(test_file)[1] <- "digit"

# Changing output variable "digit" to factor type 
comb$digit <- as.factor(comb$digit)

# rename rest of the columns in format P_<number>
for(i in 2:ncol(train_file)){
  colnames(train_file)[i] <- paste("P",i,sep="_")
  colnames(test_file)[i] <- paste("P",i,sep="_")
}

# reduced sample for test and train dataset
set.seed(100)

# train dataset sampling
indices_train= sample(1:nrow(train_file), 0.09*nrow(train_file))
train = train_file[indices_train,]

# test dataset sampling
indices_test= sample(1:nrow(test_file), 0.05*nrow(test_file))
test = test_file[indices_test,]

# combine train and test dataset
comb <- rbind(train,test)

# Checking missing value
missing <- sapply(comb, function(x) sum(is.na(x))) 
sum(which(missing ==1)) # No missing values


# split train and test data
train <- comb[1:nrow(train),]
test <- comb[-(1:nrow(train)),]

# remove columns with all zero values
train_nonZero <- train[, colSums(train != 0) > 0]

# remove output column 'digit' from train dataset
train_fil <- train_nonZero[,-1]

# Dimension Reduction
# run PCA on train dataset
prin_comp <- prcomp(train_fil, scale. = T)
names(prin_comp)

# determine PC
prin_comp$rotation

# compute standard dev and variance
stdev <- prin_comp$sdev
var <- stdev^2
var[1:10]


# understand proportion covered by PC
pr_var <- var/sum(var)
pr_var[1:20]


#draw scree plot to determin number of PC
plot(pr_var, xlab = "Principal Component", ylab = "Proportion of Variance Explained")

# First 10 PC cover 98.5% variance
pr_var[10]


#transform training set with principal components
train.data <- data.frame(digit = train$digit, prin_comp$x)

# First 10 PCs filtering
train.data <- train.data[,1:11]
train.data$digit <- as.factor(train.data$digit)

# transforming test data with PC
test.data <- predict(prin_comp, newdata = test)
test.data <- as.data.frame(test.data)

# filtering first 10 PCAs
test.data <- test.data[,1:10]
test.data<- cbind(digit=test$digit,test.data)

model_rbf1 <- ksvm(digit ~ ., data =train.data,scale=FALSE, kernel = "rbfdot")

# Predicting the model results 
Eval_RBF1<- predict(model_rbf1, test.data)

# round the output variable
round_pred <- round(Eval_RBF1)

# combining levels in model and test data
u = union(levels(as.factor(round_pred)), levels(test.data$digit))
t = table(factor(round_pred, u), factor(test.data$digit, u))

# compute model accuracy
confusionMatrix(t)

metric1 <- "Accuracy"

# Hyperparameter tuning and Cross Validation  - Non Linear - SVM 
trainControl1 <- trainControl(method="cv", number=5)


####################### Linear Model #####################################

# making a grid of C values. 
grid_linear <- expand.grid(C=seq(1, 5, by=1))

# Performing 5-fold cross validation
fit.svm_linear <- train(digit~., data=train.data, method="svmLinear", metric=metric1, 
                 tuneGrid=grid_linear, trControl=trainControl1)

# Printing cross validation result
print(fit.svm_linear)
# Best tune at C=1, 
# Accuracy - 0.835

# Plotting "fit.svm_linear" results
plot(fit.svm_linear)


# Accuracy : 0.8297          
# 95% CI : (0.7937, 0.8616)
# No Information Rate : 0.1162          
# P-Value [Acc > NIR] : < 2.2e-16       
# Kappa : 0.8108 

evaluate_linear<- predict(fit.svm_linear, test.data)
confusionMatrix(evaluate_linear, test.data$digit)

####################### Non Linear Evaluation #############################

# Making grid of "sigma" and C values. 
grid_nonlinear1 <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=seq(1, 5, by=1))

# Performing 5-fold cross validation
fit.svm_radial1 <- train(digit~., data=train.data, method="svmRadial", metric=metric1, 
                        tuneGrid=grid_nonlinear1, trControl=trainControl1)

# Printing cross validation result
print(fit.svm_radial1)
# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were sigma = 0.05 and C = 5. Accuracy = .89

# Plotting model results
plot(fit.svm_radial1)


# Validating the model results on test data
# Accuracy : 0.8778          
# 95% CI : (0.8458, 0.9052)
# No Information Rate : 0.1162          
# P-Value [Acc > NIR] : < 2.2e-16       
# Kappa : 0.8641          

evaluate_non_linear<- predict(fit.svm_radial1, test.data)
confusionMatrix(evaluate_non_linear, test.data$digit)


################################################################################3

# Non-linear model gives accuracy of .8778
# The final values used for the model were sigma = 0.05 and C = 5. Accuracy = .89

