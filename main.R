library(lattice)
library(dplyr)
library(stringr)
library(class)
USRegionalMortality

# Change Status and sex into binary values
USRegionalMortality$Status =  ifelse(USRegionalMortality$Status == "Urban", 1, 0)
USRegionalMortality$Sex =  ifelse(USRegionalMortality$Sex == "Male", 1, 0)

# change mortality cause into numerical categories

USRegionalMortality$Cause = case_when(
  USRegionalMortality$Cause == "Heart disease" ~ 1,
  USRegionalMortality$Cause == "Cancer" ~ 2,
  USRegionalMortality$Cause == "Unintentional injuries" ~ 3,
  USRegionalMortality$Cause == "Lower respiratory" ~ 4,
  USRegionalMortality$Cause == "Cerebrovascular diseases" ~ 5,
  USRegionalMortality$Cause == "Alzheimers" ~ 6,
  USRegionalMortality$Cause == "Diabetes" ~ 7,
  USRegionalMortality$Cause == "Flu and pneumonia" ~ 8,
  USRegionalMortality$Cause == "Nephritis" ~ 9,
  USRegionalMortality$Cause == "Suicide" ~ 10
)

#change region into numerical categories

USRegionalMortality$Region = str_sub(USRegionalMortality$Region,start=-1) 

USRegionalMortality


set.seed(1)
n <- nrow(USRegionalMortality)

#Split data into test/train 70 % train and 30 % test
train <- sample(1:n, 0.7*n)
test <- c(1:n)[-train]


X.train <- USRegionalMortality[train, c("Region","Sex","Cause","Rate")]
y.train <- USRegionalMortality[train, "Status"]
X.test <- USRegionalMortality[-train, c("Region","Sex","Cause","Rate")]
y.test <- USRegionalMortality[-train, "Status"]

set.seed(1)
knn.pred <- knn(train=X.train,
                test=X.test,
                cl = y.train,
                k=20)

mean(knn.pred != y.test)

### Now we can thoroughly compare KNN models with DIFFERENT Ks
K.set <- seq(1,200, by=5)
knn.test.err <- numeric(length(K.set))

set.seed(1)
for (j in 1:length(K.set)){
  knn.pred <- knn(train=X.train,
                  test=X.test,
                  cl=y.train,
                  k=K.set[j])
  knn.test.err[j] <- mean(knn.pred != y.test)
}

min(knn.test.err)
which.min(knn.test.err)
K.set[which.min(knn.test.err)]


plot(K.set, knn.test.err, 
     type='b',
     xlab="K",
     ylab="Test error")


## Selecting BOTH variable subset & K.

possible.subsets <- list()
possible.subsets[[1]] <- c("Region","Sex","Cause","Rate")
possible.subsets[[2]] <- c("Region","Cause","Rate")

for (ind in 1:length(possible.subsets)){
  var.subset <- possible.subsets[[ind]] 
  X.train <- USRegionalMortality[train, var.subset]
  y.train <- USRegionalMortality[train, "Status"]
  
  X.test <- USRegionalMortality[test, var.subset]
  y.test <- USRegionalMortality[test, "Status"]
  
  K.set <- seq(1,200, by=5)
  knn.test.err <- numeric(length(K.set))
  
  set.seed(1)
  for (j in 1:length(K.set)){
    knn.pred <- knn(train=X.train,
                    test=X.test,
                    cl=y.train,
                    k=K.set[j])
    knn.test.err[j] <- mean(knn.pred != y.test)
  }
  
  if (ind == 1){
    plot(K.set, knn.test.err,
         type='b',
         xlab="K",
         ylab="Test error",
         ylim=c(0.35,0.60),
         col=length(var.subset))
  }
  
  if (ind > 1){
    lines(K.set, knn.test.err,
          type='b',
          xlab="K",
          ylab="Test error",
          col=length(var.subset))
  }
}


legend("topright",
       legend = c("Region, Sex ,Cause, Rate",
                  "Region ,Cause, Rate"),
       col=c(4:2),
       lty=1)

## BEST: ("Region,Cause, Rate") at K=5






