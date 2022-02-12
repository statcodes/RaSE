# RaSE
R Package for Random Subspace Ensemble

# Installation Instructions
To install the package, please first install the devtools packages, and run the following code in R.

```
library(devtools)
install_github("statcodes/RaSE")
```

# Multi RaSE
## Generate Data
```
set.seed(0, kind = "L'Ecuyer-CMRG")
train.data <- RaModel("multi_classification", model.no = 1, n = 100, p = 50, p0 = rep(1/4,4))
test.data <- RaModel("multi_classification", model.no = 1, n = 100, p = 50, p0 = rep(1/4,4))
xtrain <- train.data$x
colnames(xtrain) <- paste0("V",1:dim(xtrain)[2])
ytrain <- train.data$y
xtest <- test.data$x
colnames(xtest) <- paste0("V",1:dim(xtest)[2])
ytest <- test.data$y
```
## Run mRaSE classifier with LDA base classifier and no iteration
```
fit <- SmultiRase(xtrain, ytrain, B1 = 20, B2 = 50, iteration = 0, base = 'lda', cores = 1)
mean(predict(fit, xtest) != ytest)
```
## Run mRaSE classifier with LDA base classifier and one iteration
```
fit <- SmultiRase(xtrain, ytrain, B1 = 20, B2 = 50, iteration = 1,
base = 'lda', cores = 6)
mean(predict(fit, xtest) != ytest)
```

# Super RaSE

Fit a super RaSE classifier by sampling base learner from kNN, LDA and logistic regression with equal probability

```
fit <- SmultiRase(xtrain, ytrain, B1 = 20, B2 = 50, base = c("knn", "lda", "logistic"), iteration = 1, cores = 6)
mean(predict(fit, xtest) != ytest)
```

