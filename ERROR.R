library(readxl)
library(foreach)
library(lava)
d2 <- read_xlsx("diabetes_012_health_indicators_BRFSS2015.xlsx")
set.seed(20)
d <- d2[sample(1:nrow(d2), 5000),]
dy <- d$Diabetes_012
dx <- d[,-1]
nmulti <- 3
train_ind <- as.vector(sapply(0:(nmulti-1), function(j){
  sample(which(dy == j), 30)
}))
nall <- nrow(dx)
xtrain <- dx[train_ind, ]
test_ind <- setdiff(1:nall,train_ind)
xtest <- dx[test_ind, ]
ytrain <- dy[train_ind]
ytest <- dy[test_ind]
base_seq <- c("lda")
B1 = 50
B2 = 20
cores = 8

fit1 <- RaSE(xtrain, ytrain, B1 = B1 , B2 = B2, iteration = 1, base = base_seq, cores = 1)
fit1 <- RaSE(xtrain, ytrain, B1 = B1 , B2 = B2, iteration = 2, base = base_seq, cores = 1)



test.error <- mean(predict(fit1, xtest) != ytest)
train.error <- mean(predict(fit1, xtrain) != ytrain)

##Idea to fix as below. Same idea can be used to fix the qda.

mean(sapply(1:cv, function(j) {
cat(j)
aa <- tryCatch(
  expr = {
    lda(x = xtrain[-folds[[j]], Si, drop = F], grouping = ytrain[-folds[[j]]])
  },
  error = function(jj = 1){
    xx <- xtrain[-folds[[j]], Si, drop = F]
    lda(x = xx + rnorm(length(xx)) * 1e-5, grouping = ytrain[-folds[[j]]])
  }
)
mean(predict(aa, xtrain[folds[[j]], Si, drop = F])$class !=
       ytrain[folds[[j]]], na.rm = TRUE)
})




