##### mRaSE situation

library(lava)

base <- c("lda","knn","svm","tree","logistic")
criterion <- c("cv","training","auc","ric")

## ind = 1,2,4 (another way for 3)
data.ind = 1
B1 = 5
B2 = 10
n = 50
p = 30

set.seed(0)
train.data <- RaModel("multi_classification", data.ind, n = n, p = p, p0 = rep(1/4,4))
test.data <- RaModel("multi_classification", data.ind, n = n, p = p, p0 = rep(1/4,4))
xtrain <- train.data$x
ytrain <- train.data$y
xtest <- test.data$x
ytest <- test.data$y

ts_tab <- Expand(list(base = base,criterion = criterion))

## knn bug

i = 2

fit_nocn <- RaSE(xtrain, ytrain, B1 = B1, B2 = B2,
                 iteration = 0,base = base[i])
class(fit_nocn)
predict(fit_nocn,xtest)
mean(predict(fit_nocn,xtest) != ytest)

fit_0 <- RaSE(xtrain, ytrain, B1 = B1, B2 = B2,
              iteration = 0, base = base[i])
class(fit_0)
predict(fit_0,xtest)
mean(predict(fit_0,xtest) != ytest)
fit_0$ranking

fit_1 <- RaSE(xtrain, ytrain, B1 = B1, B2 = B2,
              iteration = 1,base = base[i])
class(fit_1)
predict(fit_1,xtest)
mean(predict(fit_1,xtest) != ytest)
fit_1$ranking

fit_2 <- RaSE(xtrain, ytrain, B1 = B1, B2 = B2,
              iteration = 2, base = base[i])
class(fit_2)
predict(fit_2,xtest)
mean(predict(fit_2,xtest) != ytest)
fit_2$ranking


## SmRaSE

base <- c("lda","knn","svm","tree","logistic")
criterion <- c("cv","training","auc","ric")

## ind = 1,2,4 (another way for 3)
data.ind = 1
B1 = 5
B2 = 10
n = 50
p = 30

set.seed(0)
train.data <- RaModel("multi_classification", data.ind, n = n, p = p, p0 = rep(1/4,4))
test.data <- RaModel("multi_classification", data.ind, n = n, p = p, p0 = rep(1/4,4))
xtrain <- train.data$x
ytrain <- train.data$y
xtest <- test.data$x
ytest <- test.data$y

ts_tab <- Expand(list(base = base,criterion = criterion))

fit_0 <- RaSE(xtrain, ytrain, B1 = B1, B2 = B2,
              iteration = 0,base = base)
class(fit_0)
predict(fit_0,xtest)
mean(predict(fit_0,xtest) != ytest)
fit_0$ranking.base
fit_0$ranking.feature


fit_1 <- RaSE(xtrain, ytrain, B1 = B1, B2 = B2,
              iteration = 1,base = base)
class(fit_1)
predict(fit_1,xtest)
mean(predict(fit_1,xtest) != ytest)
fit_1$ranking.base
fit_1$ranking.feature


fit_2 <- RaSE(xtrain, ytrain, B1 = B1, B2 = B2,
              iteration = 2,base = base)
class(fit_2)
predict(fit_2,xtest)
mean(predict(fit_2,xtest) != ytest)
fit_2$ranking.base
fit_2$ranking.feature
