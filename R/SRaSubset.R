#' @importFrom stats na.omit

SRaSubset <- function(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval,
                      B2 = B2, S = S, base = base, base.list = base.list,
                      k = k, criterion = criterion,cv = cv,
                      nmulti = nmulti, D =D,
                      t0.mle = NULL, t1.mle = NULL,
                      mu0.mle = NULL,mu1.mle = NULL, Sigma.mle = NULL, Sigma0.mle = NULL,
                      Sigma1.mle = NULL, gam = NULL, kl.k = NULL,
                      lower.limits = NULL, upper.limits = NULL,
                      weights = NULL,...) {
    list2env(list(...), environment())

    #maxit = 80

    if(length(base) == 1){##regular rase

    if(base == "logistic") {

      if (is.null(weights))
        weights <- rep(1, length(ytrain)) / length(ytrain)
      # nnet::multinom maxit = 10
      if (!exists("maxit"))
        maxit = 100
      
      folds <- createFolds(ytrain, k = cv)
      subspace.list <- sapply(1:B2, function(i) {
        
        Si <- S[, i][!is.na(S[, i])]  # current subspace
        mean(sapply(1:cv, function(j){
          fit <- multinom(y ~ ., data = data.frame(x = xtrain[-folds[[j]], Si, drop = F], y = ytrain[-folds[[j]]]),maxit = maxit)
          if(criterion == "error rate"){
            mean(as.numeric(predict(fit, data.frame(x = xtrain[folds[[j]], Si, drop = F]))) != ytrain[folds[[j]]], na.rm = TRUE)
          } else if(criterion == "training"){
            mean(as.numeric(predict(fit, data.frame(x = xtrain[-folds[[j]], Si, drop = F])))  != ytrain[-folds[[j]]], na.rm = TRUE)
            } else if(criterion == "likelihood"){
            ptab <- predict(fit, data.frame(x = xtrain[folds[[j]], Si, drop = F]),
                            type = "prob")
            pvec <- sapply(folds[[j]],function(index){ptab[match(index,folds[[j]]),
                                                           ytrain[index]]})
            pvec[which(pvec == 0)] <- 1e-10
            sum(log(1/pvec))}
          
        }))
      })

      i0 <- which.min(subspace.list)
      S <- S[!is.na(S[, i0]), i0]  # final optimal subspace

      xtrain.r <- xtrain[, S, drop = F]

      fit <- multinom(y~.,data = data.frame(x = xtrain.r,y = ytrain), weights = weights, maxit = maxit)
      ytrain.pred <- as.numeric(predict(fit,data = data.frame(xtrain.r)))}

    if (base == "svm") {
      
      if(!is.character(kernel))
        kernel <- "linear"

      folds <- createFolds(ytrain, k = cv)
      subspace.list <- sapply(1:B2, function(i){
        
        Si <- S[, i][!is.na(S[, i])]  # current subspace
        mean(sapply(1:cv, function(j) {
          fit <- svm(x = xtrain[-folds[[j]], Si, drop = F], y = ytrain[-folds[[j]]],
                     kernel = kernel, type = "C-classification",probability = T)
          if(criterion == "error rate"){
            mean(predict(fit, xtrain[folds[[j]], Si, drop = F]) != ytrain[folds[[j]]], na.rm = TRUE)
          } else if(criterion == "training"){
            mean(predict(fit, xtrain[-folds[[j]], Si, drop = F]) != ytrain[-folds[[j]]], na.rm = TRUE)
          } else if(criterion == "likelihood"){
            ptab <-  attr(predict(fit, xtrain[folds[[j]], Si, drop = F],
                                  probability = T), "probabilities")
            pvec <- sapply(folds[[j]],function(index){ptab[match(index,folds[[j]]),
                                                           ytrain[index]]})
            pvec[which(pvec == 0)] <- 1e-10
            sum(log(1/pvec))}

        }))
      })
    

        i0 <- which.min(subspace.list)
        S <- S[!is.na(S[, i0]), i0]  # final optimal subspace

        xtrain.r <- xtrain[, S, drop = F]
        fit <- svm(x = xtrain.r, y = ytrain, kernel = kernel, type = "C-classification",probability = T)
        ytrain.pred <- predict(fit, xtrain.r)
    }

    if (base == "knn") {

        folds <- createFolds(ytrain, k = cv)
        subspace.list <- sapply(1:B2, function(i){
          
            Si <- S[, i][!is.na(S[, i])]  # current subspace
            knn.test <- sapply(k, function(l){
                mean(sapply(1:cv, function(j){
        
                  if(criterion == "error rate"){
                    mean(predict(knn3(x = xtrain[-folds[[j]], Si, drop = F], y = factor(ytrain[-folds[[j]]]),
                                      k = l, use.all = FALSE), xtrain[folds[[j]], Si, drop = F], type = "class") !=
                           ytrain[folds[[j]]], na.rm = TRUE)
                  } else if(criterion == "training"){
                    mean(predict(knn3(x = xtrain[-folds[[j]], Si, drop = F], y = factor(ytrain[-folds[[j]]]),
                                      k = l, use.all = FALSE), xtrain[-folds[[j]], Si, drop = F], type = "class") !=
                           ytrain[-folds[[j]]], na.rm = TRUE)
                  } else if(criterion == "likelihood"){
                    ptab <- 
                    predict(knn3(x = xtrain[-folds[[j]], Si, drop = F], y = factor(ytrain[-folds[[j]]]),
                                 k = l, use.all = FALSE), xtrain[folds[[j]], Si, drop = F], type = "prob")
                    pvec <- sapply(folds[[j]],function(index){ptab[match(index,folds[[j]]),
                                                                   ytrain[index]]})
                    pvec[which(pvec == 0)] <- 1e-10
                    sum(log(1/pvec))}
                  
                  # k = 5 :  1,1,1,1,2 --- (4/5,1/5,0,0)
                  # k = 11 : 1,1,1,1,2,1,1,1,1,1,1,1,1 (10/11,1/11,0,0)
                
                  
                  }))
            })
            min(knn.test)
        })
      
        i0 <- which.min(subspace.list)
        S <- S[!is.na(S[, i0]), i0]  # final optimal subspace

        xtrain.r <- xtrain[, S, drop = F]
        knn.test <- sapply(k,function(l){
          mean(sapply(1:cv, function(j){
            if(criterion == "error rate"){
              mean(predict(knn3(x = xtrain.r[-folds[[j]], ,drop = F],
                                y = factor(ytrain[-folds[[j]]]), k = l, use.all = FALSE),
                           xtrain.r[folds[[j]], , drop = F], type = "class") != ytrain[folds[[j]]],
                   na.rm = TRUE)
            } else if(criterion == "training"){
              mean(predict(knn3(x = xtrain.r[-folds[[j]], ,drop = F],
                                y = factor(ytrain[-folds[[j]]]), k = l, use.all = FALSE),
                           xtrain.r[-folds[[j]], , drop = F], type = "class") != ytrain[-folds[[j]]],
                   na.rm = TRUE)
            } else if(criterion == "likelihood"){
              ptab <- 
                predict(knn3(x = xtrain.r[-folds[[j]], ,drop = F],
                             y = factor(ytrain[-folds[[j]]]), k = l, use.all = FALSE),
                        xtrain.r[folds[[j]], , drop = F], type = "prob")
              pvec <- sapply(folds[[j]],function(index){ptab[match(index,folds[[j]]),
                                                                     ytrain[index]]})
              pvec[which(pvec == 0)] <- 1e-10
              sum(log(1/pvec))
            }
            
          }))
        })

        k.op <- k[which.min(knn.test)]
        fit <- knn3(x = xtrain.r, y = factor(ytrain), k = k.op, use.all = FALSE)
        ytrain.pred <- predict(fit, xtrain.r, type = "class")

    }

    if (base == "tree") {

      ytrain <- factor(ytrain)

      folds <- createFolds(ytrain, k = cv)
      subspace.list <- sapply(1:B2, function(i) {
          # the last row is training error for each i in 1:B2
          Si <- S[, i][!is.na(S[, i])]  # current subspace
          mean(sapply(1:cv, function(j){
            fit <-
              rpart(y ~ .,
                    data = data.frame(x = xtrain[-folds[[j]], Si, drop = F], y = ytrain[-folds[[j]]]),
                    method = "class")
            if(criterion == "error rate"){
              mean((as.numeric(
                predict(fit, data.frame(x = xtrain[folds[[j]], Si, drop = F]), type = "class")
              )) != ytrain[folds[[j]]],
              na.rm = TRUE)
            } else if(criterion == "training"){
              mean((as.numeric(
                predict(fit, data.frame(x = xtrain[-folds[[j]], Si, drop = F]), type = "class")
              )) != ytrain[-folds[[j]]],
              na.rm = TRUE)
            } else if(criterion == "likelihood"){
              ptab <- 
              predict(fit, data.frame(x = xtrain[folds[[j]], Si, drop = F]), type = "prob")
              pvec <- sapply(folds[[j]],function(index){ptab[match(index,folds[[j]]),
                                                             ytrain[index]]})
              pvec[which(pvec == 0)] <- 1e-10
              sum(log(1/pvec))
            }

          }))
        })
      
        i0 <- which.min(subspace.list)
        S <- S[!is.na(S[, i0]), i0]  # final optimal subspace

        xtrain.r <- xtrain[, S, drop = F]
        fit <- rpart(y ~ ., data = data.frame(x = xtrain.r, y = ytrain), method = "class")
        ytrain.pred <- predict(fit, data.frame(x = xtrain.r), type = "class")
    }

    if (base == "lda") {

        folds <- createFolds(ytrain, k = cv)
        subspace.list <- sapply(1:B2, function(i) {

          Si <- S[, i][!is.na(S[, i])]  # current subspace
          mean(sapply(1:cv, function(j){

            aa <- tryCatch(
              expr = {
                lda(x = xtrain[-folds[[j]], Si, drop = F], grouping = ytrain[-folds[[j]]])
              },
              error = function(jj = 1){
                xx <- xtrain[-folds[[j]], Si, drop = F]
                lda(x = xx + rnorm(length(xx)) * 1e-10, grouping = ytrain[-folds[[j]]])
              }
            )
            if(criterion == "error rate"){
              mean(predict(aa, xtrain[folds[[j]], Si, drop = F])$class !=
                     ytrain[folds[[j]]], na.rm = TRUE)
            } else if (criterion == "training"){
              mean(predict(aa, xtrain[-folds[[j]], Si, drop = F])$class !=
                     ytrain[-folds[[j]]], na.rm = TRUE)
            } else if (criterion == "likelihood"){
              ptab <- predict(aa, xtrain[folds[[j]], Si, drop = F])$posterior
              pvec <- sapply(folds[[j]],function(index){ptab[match(index,folds[[j]]),
                                                             ytrain[index]]})
              pvec[which(pvec == 0)] <- 1e-10
              return(sum(log(1/pvec)))
            }
            }))
          })
      
        i0 <- which.min(subspace.list)
        S <- S[!is.na(S[, i0]), i0]  # final optimal subspace

        #cat("Length of S :",length(S),"\n")

        xtrain.r <- xtrain[, S, drop = F]
        fit <- lda(x = as.matrix(xtrain.r), grouping = ytrain)
        ytrain.pred <- predict(fit, as.matrix(xtrain.r))$class
    }

   
    return(list(fit = fit, ytrain.pred = as.numeric(ytrain.pred), subset = S))

    } else {
      
      # super RaSE
      # ---------------------------------------------------
      if (!is.character(kernel)) {
      kernel <- "linear"
    }

      folds <- createFolds(ytrain, k = cv)
      subspace.list <- sapply(1:B2, function(i) {

        Si <- S[, i][!is.na(S[, i])]   # current subspace
        xtrain.r <- xtrain[, Si, drop = F]
        xtrain.r.df <- data.frame(xtrain.r)
        
        if (base.list[i] == "lda"){
                
                mean(sapply(1:cv, function(j){
                
                aa <- tryCatch(
                  expr = {
                    lda(x = xtrain.r.df[-folds[[j]],,drop = F],
                        grouping = ytrain[-folds[[j]]])
                  },
                  error = function(jj = 1){
                    xx <- xtrain.r.df[-folds[[j]],, drop = F]
                    lda(x = xx + rnorm(length(xx)) * 1e-10, grouping = ytrain[-folds[[j]]])
                  }
                )
                
                if(criterion == "error rate"){
                  mean(predict(aa, xtrain.r.df[folds[[j]], , drop = F])$class != ytrain[folds[[j]]], na.rm = TRUE)
                } else if(criterion == "training"){
                  mean(predict(aa, xtrain.r.df[-folds[[j]], , drop = F])$class != ytrain[-folds[[j]]], na.rm = TRUE)
                } else if(criterion == "likelihood"){
                  ptab <- predict(aa, xtrain.r.df[folds[[j]], , drop = F])$posterior
                  pvec <- sapply(folds[[j]],function(index){ptab[match(index,folds[[j]]),
                                                                 ytrain[index]]})
                  pvec[which(pvec == 0)] <- 1e-10
                  sum(log(1/pvec))
                }
                
                }))


              }
              else if (base.list[i] == "svm"){
                mean(sapply(1:cv, function(j){
                  
                fit <- svm(x = xtrain.r[-folds[[j]], , drop = F], y = ytrain[-folds[[j]]], kernel = kernel, 
                           type = "C-classification",probability = T)

                if(criterion == "error rate"){
                  mean(predict(fit, xtrain.r[folds[[j]], , drop = F]) != ytrain[folds[[j]]], na.rm = TRUE)
                    } else if(criterion == "training"){
                      mean(predict(fit, xtrain.r[-folds[[j]], , drop = F]) != ytrain[-folds[[j]]], na.rm = TRUE)
                    } else if(criterion == "likelihood"){
                      ptab <-  attr(predict(fit, xtrain.r[folds[[j]], , drop = F],
                                            probability = T), "probabilities")
                      pvec <- sapply(folds[[j]],function(index){ptab[match(index,folds[[j]]),
                                                                     ytrain[index]]})
                      pvec[which(pvec == 0)] <- 1e-10
                      sum(log(1/pvec))
                    }
                }))
                }
              else if (base.list[i] == "tree"){
                  ytrain <- factor(ytrain)

                  mean(sapply(1:cv, function(j){
                    
                    fit <- rpart(y ~ ., data = data.frame(x = xtrain.r[-folds[[j]], , drop = F], y = ytrain[-folds[[j]]]), method = "class")
                    if(criterion == " error rate"){
                      mean((as.numeric(predict(fit, data.frame(x = xtrain.r[folds[[j]], , drop = F]), type = "class"))) != ytrain[folds[[j]]], na.rm = TRUE)
                    } else if(criterion == "training"){
                      mean((as.numeric(predict(fit, data.frame(x = xtrain.r[-folds[[j]], , drop = F]), type = "class"))) != ytrain[-folds[[j]]], na.rm = TRUE)
                    } else if(criterion == "likelihood"){
                      ptab <- 
                        predict(fit, data.frame(x = xtrain.r[folds[[j]], , drop = F]), type = "prob")
                      pvec <- sapply(folds[[j]],function(index){ptab[match(index,folds[[j]]),
                                                                     ytrain[index]]})
                      pvec[which(pvec == 0)] <- 1e-10
                      sum(log(1/pvec))
                    }
                  }))
              }
              else if (base.list[i] == "logistic"){
                  if (is.null(weights)) {
                      weights <- rep(1, length(ytrain))/length(ytrain)
                  }
                  # nnet::multinom maxit = 10
                  if(!exists("maxit")){maxit = 80}
                mean(sapply(1:cv, function(j){
                  fit <- multinom(y ~ ., data = data.frame(x = xtrain.r[-folds[[j]], , drop = F], y = ytrain[-folds[[j]]]),maxit = maxit)
                  
                  if(criterion == "error rate"){
                    mean(as.numeric(predict(fit, data.frame(x = xtrain.r[folds[[j]], , drop = F])))  != ytrain[folds[[j]]], na.rm = TRUE)
                  } else if(criterion  == "training"){
                    mean(as.numeric(predict(fit, data.frame(x = xtrain.r[-folds[[j]], , drop = F])))  != ytrain[-folds[[j]]], na.rm = TRUE)
                  } else if(criterion == "likelihood"){
                    ptab <- predict(fit, data.frame(x = xtrain.r[folds[[j]], , drop = F]),
                                    type = "prob")
                    pvec <- sapply(folds[[j]],function(index){ptab[match(index,folds[[j]]),
                                                                   ytrain[index]]})
                    pvec[which(pvec == 0)] <- 1e-10
                    sum(log(1/pvec))
                  }
                  }))
    

              }
              else if (base.list[i] == "knn") {
                
                knn.test <- sapply(k, function(l) {
                    mean(sapply(1:cv, function(j) {
                      fit <- knn3(x = xtrain.r[-folds[[j]], , drop = F], y = factor(ytrain[-folds[[j]]]), k = l, use.all = FALSE)
                      if(criterion == "cv"){
                        mean(predict(fit, xtrain.r[folds[[j]], , drop = F], type = "class")  != ytrain[folds[[j]]], na.rm = TRUE)
                      } else if(criterion == "training"){
                        mean(predict(fit, xtrain.r[-folds[[j]], , drop = F], type = "class")  != ytrain[-folds[[j]]], na.rm = TRUE)
                      } else if(criterion == "likelihood"){
                        ptab <- 
                          predict(fit, xtrain.r[folds[[j]], , drop = F], type = "prob")
                        pvec <- sapply(folds[[j]],function(index){ptab[match(index,folds[[j]]),
                                                                       ytrain[index]]})
                        pvec[which(pvec == 0)] <- 1e-10
                        sum(log(1/pvec))}
                      }))
                    })
                  min(knn.test)
              }
        })


      i0 <- which.min(subspace.list)
      S <- S[!is.na(S[, i0]), i0]  # final optimal subspace
      xtrain.r <- xtrain[, S, drop = F]
      if (base.list[i0] == "lda"){
          fit <- lda(x = as.matrix(xtrain.r), grouping = ytrain)
          ytrain.pred <- predict(fit, as.matrix(xtrain.r))$class
      }
      if (base.list[i0] == "svm"){
          fit <- svm(x = xtrain.r, y = ytrain, kernel = kernel, type = "C-classification")
          ytrain.pred <- predict(fit, xtrain.r)
      }
      if (base.list[i0] == "tree"){
          fit <- rpart(y ~ ., data = data.frame(x = xtrain.r, y = ytrain), method = "class")
          ytrain.pred <- predict(fit, data.frame(x = xtrain.r), type = "class")
      }
      if (base.list[i0] == "logistic"){
          # nnet::multinom maxit = 10
          if(!exists("maxit")){maxit = 80}
          fit <- multinom(y~.,data = data.frame(x = xtrain.r,y = ytrain), weights = weights, maxit = maxit)
          ytrain.pred <- as.numeric(predict(fit,data = data.frame(xtrain.r)))
      }
      if (base.list[i0] == "knn") {

          knn.test <- sapply(k, function(l) {
            mean(sapply(1:cv, function(j){
              if(criterion == "error rate"){
                mean(predict(knn3(x = xtrain.r[-folds[[j]], ,drop = F],
                                  y = factor(ytrain[-folds[[j]]]), k = l, use.all = FALSE),
                             xtrain.r[folds[[j]], , drop = F], type = "class") != ytrain[folds[[j]]], na.rm = TRUE)
              } else if(criterion == "training"){
                mean(predict(knn3(x = xtrain.r[-folds[[j]], ,drop = F],
                                  y = factor(ytrain[-folds[[j]]]), k = l, use.all = FALSE),
                             xtrain.r[-folds[[j]], , drop = F], type = "class") != ytrain[-folds[[j]]], na.rm = TRUE)
              } else if(criterion == "likelihood"){
                ptab <- 
                  predict(knn3(x = xtrain.r[-folds[[j]], ,drop = F],
                               y = factor(ytrain[-folds[[j]]]), k = l, use.all = FALSE), 
                          xtrain.r[folds[[j]], , drop = F], type = "prob")
                pvec <- sapply(folds[[j]],function(index){ptab[match(index,folds[[j]]),
                                                                       ytrain[index]]})
                pvec[which(pvec == 0)] <- 1e-10
                sum(log(1/pvec))
              }
              }))
            })

          k.op <- k[which.min(knn.test)[1]]
          fit <- knn3(x = xtrain.r, y = factor(ytrain), k = k.op, use.all = FALSE)
          ytrain.pred <- predict(fit, xtrain.r, type = "class")
          }

      return(list(fit = fit, ytrain.pred = ytrain.pred, subset = S, base.select = base.list[i0]))
  }

}
