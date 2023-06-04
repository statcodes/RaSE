B1 = 200
B2 = 500
cores = 8
ntest = 3000
p = 400
p_seq <- rep(1/4,4)
maxit = 80


setwd("/scratch/fb2234/RSource")
files.sources = list.files()
sapply(files.sources, source)

setwd("/scratch/fb2234/rpackages")

pkgs <- c("MASS","class","caret","doParallel","foreach","parallel","iterators","lattice",
          "rpart","nnet","randomForest","e1071","ggplot2","gridExtra",
          "formatR","FNN","ranger","KernelKnn","utils","glmnet","ModelMetrics",
          "lava","keras")

# not use "keras","tensorflow","readr" here

lapply(pkgs, require, character.only = TRUE)
library(keras,lib = "/scratch/fb2234/rpackages")

# library(caret,lib = "/scratch/fb2234/rpackages")
#library(readxl,lib = "/scratch/fb2234/rpackages")
# library(keras,lib = "/scratch/fb2234/rpackages")
# library(lattice,lib = "/scratch/fb2234/rpackages")
# library(doParallel,lib = "/scratch/fb2234/rpackages")

seed <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
set.seed(seed, kind = "L'Ecuyer-CMRG")
set_id <- floor((seed-1)/100)+1

ntrain_seq <- c(200, 400, 1000)
base_seq <- c("lda", "knn", "logistic", "svm", "tree")
ex_seq <- 1:4
all_para <- expand.grid(ntrain_seq, 1:6, ex_seq)

para <- all_para[set_id, ]
ntrain <- as.numeric(para[1])
base <- base_seq[as.numeric(para[2])]
if(is.na(base)){base = base_seq}
ex <- as.numeric(para[3])

all.data3 <- RaModel("multi_classification", model.no = 3,
                     n = ntrain+ntest, p = p, p0 = p_seq)

xtrain1 <- all.data3$x[1:ntrain,]
colnames(xtrain1) <- paste0("V",1:dim(xtrain1)[2])
xtest1 <- all.data3$x[(ntrain+1):(ntrain+ntest),]
colnames(xtest1) <- paste0("V",1:dim(xtest1)[2])
ytrain1 <- all.data3$y[1:ntrain]
ytest1 <- all.data3$y[(ntrain+1):(ntrain+ntest)]

fit1 <- RaSE(xtrain1, ytrain1, B1 = B1 , B2 = B2,
                   iteration = 0,base = base, cores = cores)
test.error <- mean(predict(fit1, xtest1) != ytest1)
train.error <- mean(predict(fit1, xtrain1) != ytrain1)

fit1_it <- RaSE(xtrain1, ytrain1, B1 = B1, B2 = B2,
                      iteration = 1, base = base, cores = cores)
test.error.it <- mean(predict(fit1_it, xtest1) != ytest1)
train.error.it <- mean(predict(fit1_it, xtrain1) != ytrain1)

flag_lda <- try(mod_lda <- lda(xtrain1, ytrain1))
if(attr(flag_lda,"class") == "try-error"){
  mod_lda <- NA
  train.error.lda <- NA
  test.error.lda <- NA
} else {
  train.error.lda <- mean(predict(mod_lda, xtrain1)$class != ytrain1)
  test.error.lda <- mean(predict(mod_lda,xtest1)$class != ytest1)
}

##
flag_knn <- try(mod_knn <-  knn3(xtrain1, factor(ytrain1), k = 7))
if(attr(flag_knn,"class") == "try-error"){
  mod_knn <- NA
  train.error.knn <- NA
  test.error.knn <- NA
} else {
  train.error.knn <- mean(predict(mod_knn,xtrain1,type = "class") != ytrain1)
  test.error.knn <- mean(predict(mod_knn,xtest1,type = "class") != ytest1)
}

##
flag_logistic <- try(mod_logistic <- multinom(y ~ .,
                                              data = data.frame(x = xtrain1,
                                                                y = ytrain1),
                                              maxit = maxit))
if(attr(flag_logistic,"class") == "try-error"){
  mod_logistic <- NA
  train.error.logistic <- NA
  test.error.logistic <- NA
} else {
  train.error.logistic <- mean(predict(mod_logistic,data.frame(x = xtrain1)) != ytrain1)
  test.error.logistic <- mean(predict(mod_logistic,data.frame(x = xtest1)) != ytest1)
}


##
flag_svm <- try(mod_svm <- svm(x = xtrain1, y = ytrain1, type = "C-classification"))
if(attr(flag_svm,"class") == "try-error"){
  mod_svm <- NA
  train.error.svm <- NA
  test.error.svm <- NA
} else {
  train.error.svm <- mean(predict(mod_svm,xtrain1) != ytrain1)
  test.error.svm <- mean(predict(mod_svm,xtest1) != ytest1)
}


##
flag_tree <- try(mod_tree <- rpart(y ~ ., data = data.frame(x = xtrain1, y = ytrain1),
                                   method = "class"))
if(attr(flag_tree,"class") == "try-error"){
  mod_tree <-NA
  train.error.tree <- NA
  test.error.tree <- NA
} else {
  train.error.tree <- mean(predict(mod_tree,data.frame(x = xtrain1),
                                   type = "class") != ytrain1)
  test.error.tree <- mean(predict(mod_tree,data.frame(x = xtest1),
                                  type = "class") != ytest1)
  }

mod_rf <- randomForest(y ~ ., data = data.frame(x = xtrain1, y = as.factor(ytrain1)))
test.error.rf <- mean(predict(mod_rf, data.frame(x = xtest1)) != ytest1)
train.error.rf <- mean(predict(mod_rf, data.frame(x = xtrain1)) != ytrain1)

dl_model <- dnn(xtrain1, ytrain1, levels = 3)
y_pred_label <- predict(dl_model, xtest1)
test.error.dnn <- mean(y_pred_label != ytest1)

y_pred_label <- predict(dl_model, xtrain1)
train.error.dnn <- mean(y_pred_label != ytrain1)

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

setwd("/scratch/fb2234/Simulation3-result")
save(p1,test.error,train.error,
     p1_it,test.error.it,train.error.it,
     mod_lda,test.error.lda,train.error.lda,
     mod_knn,test.error.knn,train.error.knn,
     mod_logistic,test.error.logistic,train.error.logistic,
     mod_svm,test.error.svm,train.error.svm,
     mod_tree,test.error.tree,train.error.tree,
     mod_rf,test.error.rf,train.error.rf,
     test.error.dnn,train.error.dnn,file = file_name)
