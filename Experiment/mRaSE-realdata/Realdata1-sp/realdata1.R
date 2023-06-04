B1 = 200
B2 = 500
cores = 8
maxit = 80

.libPaths("/scratch/fb2234/rpackages")
load("sp_dt.RData")

setwd("/scratch/fb2234/RSource")
files.sources = list.files()
sapply(files.sources, source)

pkgs <- c("MASS","class","caret","doParallel","foreach","parallel","iterators","lattice",
          "rpart","nnet","randomForest","e1071","ggplot2","gridExtra",
          "formatR","FNN","ranger","KernelKnn","utils","glmnet","ModelMetrics",
          "lava","keras")

lapply(pkgs, require, character.only = TRUE)

seed <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
set.seed(seed, kind = "L'Ecuyer-CMRG")
set_id <- floor((seed-1)/100)+1

base_seq <- c("lda", "knn", "logistic", "svm", "tree")

base <- base_seq[set_id]
if(set_id == 6){base = base_seq}

nmulti <- 6
ntrain <- nmulti*100
train_ind <- as.vector(sapply(1:nmulti, function(j){
  sample(which(spy == j), 100)
}))
nall <- nrow(spx)
xtrain <- spx[train_ind, ]
test_ind <- setdiff(1:nall,train_ind)
xtest <- spx[test_ind, ]
ytrain <- spy[train_ind]
ytest <- spy[test_ind]

fit1 <- RaSE(xtrain, ytrain, B1 = B1 , B2 = B2,
             iteration = 0,base = base, cores = cores)
test.error <- mean(predict(fit1, xtest) != ytest)
train.error <- mean(predict(fit1, xtrain) != ytrain)

fit1_it <- RaSE(xtrain, ytrain, B1 = B1, B2 = B2,
                iteration = 1, base = base, cores = cores)
test.error.it <- mean(predict(fit1_it, xtest) != ytest)
train.error.it <- mean(predict(fit1_it, xtrain) != ytrain)

flag_lda <- try(mod_lda <- lda(xtrain, ytrain))
if(attr(flag_lda,"class") == "try-error"){
  mod_lda <- NA
  train.error.lda <- NA
  test.error.lda <- NA
} else {
  train.error.lda <- mean(predict(mod_lda, xtrain)$class != ytrain)
  test.error.lda <- mean(predict(mod_lda,xtest)$class != ytest)
}

##
flag_knn <- try(mod_knn <-  knn3(xtrain, factor(ytrain), k = 7))
if(attr(flag_knn,"class") == "try-error"){
  mod_knn <- NA
  train.error.knn <- NA
  test.error.knn <- NA
} else {
  train.error.knn <- mean(predict(mod_knn,xtrain,type = "class") != ytrain)
  test.error.knn <- mean(predict(mod_knn,xtest,type = "class") != ytest)
}

##
flag_logistic <- try(mod_logistic <- multinom(y ~ .,
                                              data = data.frame(x = xtrain,
                                                                y = ytrain),
                                              maxit = maxit))
if(attr(flag_logistic,"class") == "try-error"){
  mod_logistic <- NA
  train.error.logistic <- NA
  test.error.logistic <- NA
} else {
  train.error.logistic <- mean(predict(mod_logistic,data.frame(x = xtrain)) != ytrain)
  test.error.logistic <- mean(predict(mod_logistic,data.frame(x = xtest)) != ytest)
}

##
flag_svm <- try(mod_svm <- svm(x = xtrain, y = ytrain, type = "C-classification"))
if(attr(flag_svm,"class") == "try-error"){
  mod_svm <- NA
  train.error.svm <- NA
  test.error.svm <- NA
} else {
  train.error.svm <- mean(predict(mod_svm,xtrain) != ytrain)
  test.error.svm <- mean(predict(mod_svm,xtest) != ytest)
}


##
flag_tree <- try(mod_tree <- rpart(y ~ ., data = data.frame(x = xtrain, y = ytrain),
                                   method = "class"))
if(attr(flag_tree,"class") == "try-error"){
  mod_tree <-NA
  train.error.tree <- NA
  test.error.tree <- NA
} else {
  train.error.tree <- mean(predict(mod_tree,data.frame(x = xtrain),
                                   type = "class") != ytrain)
  test.error.tree <- mean(predict(mod_tree,data.frame(x = xtest),
                                  type = "class") != ytest)
}

mod_rf <- randomForest(y ~ ., data = data.frame(x = xtrain, y = as.factor(ytrain)))
test.error.rf <- mean(predict(mod_rf, data.frame(x = xtest)) != ytest)
train.error.rf <- mean(predict(mod_rf, data.frame(x = xtrain)) != ytrain)

dl_model <- dnn(xtrain, ytrain, levels = 3)
y_pred_label <- predict(dl_model, xtest)
test.error.dnn <- mean(y_pred_label != ytest)

y_pred_label <- predict(dl_model, xtrain)
train.error.dnn <- mean(y_pred_label != ytrain)

p1 <- list(D = fit1$D,
           cutoff = fit1$cutoff,
           subspace = fit1$subspace,
           rk = fit1$ranking)

p1_it <- list(D = fit1_it$D,
              cutoff = fit1_it$cutoff,
              subspace = fit1_it$subspace,
              rk  = fit1_it$ranking)

base_name <- ifelse(length(base) == 1,base,"super")
file_name <- paste0(base_name,ntrain,"_",seed-(set_id-1)*100,".RData")

setwd("/scratch/fb2234/Realdata1-result1")
save(p1,test.error,train.error,
     p1_it,test.error.it,train.error.it,
     mod_lda,test.error.lda,train.error.lda,
     mod_knn,test.error.knn,train.error.knn,
     mod_logistic,test.error.logistic,train.error.logistic,
     mod_svm,test.error.svm,train.error.svm,
     mod_tree,test.error.tree,train.error.tree,
     mod_rf,test.error.rf,train.error.rf,
     test.error.dnn,train.error.dnn,file = file_name)

