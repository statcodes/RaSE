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
B1 = 20
B2 = 50
cores = 8

fit1 <- RaSE(xtrain, ytrain, B1 = B1 , B2 = B2, iteration = 0,base = base_seq, cores = cores)

test.error <- mean(predict(fit1, xtest) != ytest)
train.error <- mean(predict(fit1, xtrain) != ytrain)



