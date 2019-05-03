library(lattice)
library(dplyr)
library(stringr)
library(class)


# Change Status and sex into binary values
USRegionalMortality$Status =  ifelse(USRegionalMortality$Status == "Urban", 1, 0)
USRegionalMortality$Sex =  ifelse(USRegionalMortality$Sex == "Male", 1, 0)

USRegionalMortality$SE <- NULL

# change mortality cause into numerical categories

USRegionalMortality$Cause = case_when(
  USRegionalMortality$Cause == "Heart disease" ~ 1,
  
  TRUE ~  0
)

#change region into numerical categories

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


set.seed(1)
n <- nrow(USRegionalMortality)

#Split data into test/train 80 % train and 20 % test
train <- sample(1:n, 0.8*n)
test <- c(1:n)[-train]


X.train <- USRegionalMortality[train, c("Region","Sex","Status","Rate")]
y.train <- USRegionalMortality[train, "Cause"]
X.test <- USRegionalMortality[test, c("Region","Sex","Status","Rate")]
y.test <- USRegionalMortality[test, "Cause"]

set.seed(1)
knn.pred <- knn(train=X.train,
                test=X.test,
                cl = y.train,
                k=19)

mean(knn.pred != y.test)

### Now we can thoroughly compare KNN models with DIFFERENT Ks
K.set <- seq(1,201, by=5)
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
possible.subsets[[1]] <- c("Region","Sex","Status","Rate")
possible.subsets[[2]] <- c("Region","Status","Rate")
possible.subsets[[3]] <- c("Sex","Status","Rate")

for (ind in 1:length(possible.subsets)){
  var.subset <- possible.subsets[[ind]] 
  X.train <- USRegionalMortality[train, var.subset]
  y.train <- USRegionalMortality[train, "Status"]
  
  X.test <- USRegionalMortality[test, var.subset]
  y.test <- USRegionalMortality[test, "Status"]
  
  K.set <- seq(1,201, by=5)
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
         ylim=c(0,0.80),
         col=1)
  }
  
  if (ind ==2 ){
    lines(K.set, knn.test.err,
          type='b',
          xlab="K",
          ylab="Test error",
          col=2)
  }
  
  if (ind ==3){
    lines(K.set, knn.test.err,
          type='b',
          xlab="K",
          ylab="Test error",
          col=3)
  }
}


legend("topright",
       legend = c("Region, Sex ,Status, Rate",
                  "Region ,Status, Rate",
                  "Sex,Status, Rate"),
       col=c(1:4),
       lty=2)






