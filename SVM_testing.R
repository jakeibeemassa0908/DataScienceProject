#Implementing SVM on USRegionalMorality dataset

install.packages('caret')
library(e1071)
library(ISLR)
library(lattice)
library(dplyr)
library(stringr)
library(class)
library('caret')

# Change Status and Sex into binary values
USRegionalMortality$Status =  ifelse(USRegionalMortality$Status == "Urban", 1, 0)
USRegionalMortality$Sex =  ifelse(USRegionalMortality$Sex == "Male", 1, 0)

# change Mortality cause into numerical categories
USRegionalMortality$Cause = case_when(
  USRegionalMortality$Cause == "Heart disease" ~ 1,  TRUE ~  0 )

#change Region into numerical categories
USRegionalMortality$Region = case_when(
  USRegionalMortality$Region == "HHS Region 01" ~ 1000000000,
  USRegionalMortality$Region == "HHS Region 02" ~ 0100000000,
  USRegionalMortality$Region == "HHS Region 03" ~ 0010000000,
  USRegionalMortality$Region == "HHS Region 04" ~ 0001000000,
  USRegionalMortality$Region == "HHS Region 05" ~ 0000100000,
  USRegionalMortality$Region == "HHS Region 06" ~ 0000010000,
  USRegionalMortality$Region == "HHS Region 07" ~ 0000001000,
  USRegionalMortality$Region == "HHS Region 08" ~ 0000000100,
  USRegionalMortality$Region == "HHS Region 09" ~ 0000000010,
  USRegionalMortality$Region == "HHS Region 10" ~ 0000000001
)

USRegionalMortality

plot(USRegionalMortality)
plot(USRegionalMortality$Region)
plot(USRegionalMortality$Status)
plot(USRegionalMortality$Sex)
plot(USRegionalMortality$Rate)


#Taking out SE because it does not add value.
USRegionalMortality$SE <- NULL
#Look at results
USRegionalMortality

#Testing to see if taking out sex has any significant effect.
USRegionalMortality$Sex <- NULL
#View results
USRegionalMortality

#Testing to see if taking out region has any significant effect.
USRegionalMortality$Region <- NULL
#View results
USRegionalMortality

#Testing to see if taking out status has any significant effect.
USRegionalMortality$Status <- NULL
#View results
USRegionalMortality

#Testing to see if taking out rate has any significant effect.
USRegionalMortality$Rate <- NULL
#View results
USRegionalMortality

#####################################################################
#####################################################################
x = USRegionalMortality
x
y = USRegionalMortality$Cause
y
plot(x, col = (3-y))

dat = data.frame(x=x, y=as.factor(y))

svmfit = svm(y~.,
             data = dat,
             kernel = "linear",
             cost = 10,
             scale = FALSE)

plot(svmfit,dat)

#####################################################################
#using the tune function
set.seed(3033)
tune.out <- tune(svm,
                 Cause ~.,
                 data = USRegionalMortality,
                 kernel = "linear",
                 ranges = list(cost = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5)))
summary(tune.out)

#Plotting the 2D fitted boundary for 
plot(tune.out$best.model, data = USRegionalMortality,
     formula = status ~ sex)

######################################################################
# testing the plot function to make sure it works #
x1 = c(3,2,4,1,2,4,4)
x2 = c(4,2,4,4,1,3,1)
colors = c("red", "red", "red", "red", "blue", "blue", "blue")
plot(x1, x2, col = colors, xlim = c(0,5), ylim = c(0,5))

plot(x1, x2, col = colors, xlim = c(0, 5), ylim = c(0,5))
abline(-0.5,1)


plot(USRegionalMortality)
x = USRegionalMortality$Region
y = USRegionalMortality$Rate
colors = c("red", "blue")
plot(x, y, col = colors, xlim = c(0,11), ylim = c(0,100))

#####################################################################
#####################################################################




#Split data into training/testing. 80% training and 20% testing
set.seed(3033)
intrain <- createDataPartition(y = USRegionalMortality$Cause, p = 0.8, list = FALSE)
training <- USRegionalMortality[intrain,]
testing <- USRegionalMortality[-intrain,]

#check dimensions
dim(training)
dim(testing)

#clean the data by verifying that there are no NA/Nulls. If returns FALSE, then data is good.
anyNA(USRegionalMortality)

#summary check so far
summary(USRegionalMortality)

#make target variable a categorical variable by using factor. For yes or no, like binary 1 or 0 
training[["Cause"]] = factor(training[["Cause"]])

# Implementing the train control method "trctrl". trainControl will return a list 
# which will be put into "trctrl".  
# method: redefines the sampling method, and in this case is repeated Cross-Validation.
# number: stores the amount of resampling iterations on the dataset.
# repeats: this just contains the sets to compute for a repeated cross validation.
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#implenting SVM using the train function
svm_Linear <- train(Cause ~., data = training, method = "svmLinear",
                    trControl = trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)



############################################################
#### testing this way of implementing SVM ####
svm_Linear <- svm(Cause ~.,
                  data = training,
                  kernel = "linear",
                  cost = 10)
summary(svm_Linear)
###########################################################



#check result of train method aka svm_linear
svm_Linear

#Now we are ready to predict classes for our testing set
#using the "predict" function from caret package. Passing in 2 arguments
# svm_Linear is our trained model which we just did. And newdata will hold our testing data frame. 
test_pred <- predict(svm_Linear, newdata = testing)
test_pred   # a 0 means they are fine. a 1 means they have heart disease

#Now testing the accuracy of the model
confusionMatrix(table(test_pred, testing$Cause))

#Improving the performance of the model by custom Cost values at specific c 
grid <-expand.grid(C = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))

#Retraining the model using these C values
svm_Linear_Grid <- train(Cause ~., data = training, method = "svmLinear",
                    trControl = trctrl,
                    preProcess = c("center", "scale"),
                    tuneGrid = grid,
                    tuneLength = 10)

#view the results of model
svm_Linear_Grid   # results: "The final value used for the model was C = 0.01." gave us accuracy = .9038
plot(svm_Linear_Grid)

#check prediction error 
#mean(svm_Linear_Grid != testing)

#summary(svm_Linear_Grid)

#Test the model with the same C values
test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid

confusionMatrix(table(test_pred_grid, testing$Cause))






#Experimenting with plot for SVM fitted boundary
##################################################################

x = USRegionalMortality$Region
y = USRegionalMortality$Rate
colors = c("red", "blue")
plot(x, y, col = colors, xlim = c(0,100), ylim = c(0,100))

x = USRegionalMortality$Region
y = USRegionalMortality$Rate
colors = c("red", "blue")
plot(x, y, col = colors, xlim = c(0,11), ylim = c(0,100))
