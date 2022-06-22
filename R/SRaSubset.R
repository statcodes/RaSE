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

    if (base == "logistic") {

        if (is.null(weights)) {
            weights <- rep(1, length(ytrain))/length(ytrain)
        }
        # nnet::multinom maxit = 10
        if(!exists("maxit")){maxit = 100}

        folds <- createFolds(ytrain, k = cv)
        subspace.list <- sapply(1:B2, function(i) {
            # the last row is training error for each i in 1:B2
            Si <- S[, i][!is.na(S[, i])]  # current subspace
            mean(sapply(1:cv, function(j) {
                fit <- multinom(y ~ ., data = data.frame(x = xtrain[-folds[[j]], Si, drop = F], y = ytrain[-folds[[j]]]),maxit = maxit)
                mean(as.numeric(predict(fit, data.frame(x = xtrain[folds[[j]], Si, drop = F])))  != ytrain[folds[[j]]], na.rm = TRUE)
            }))
        })

        i0 <- which.min(subspace.list)
        S <- S[!is.na(S[, i0]), i0]  # final optimal subspace

        xtrain.r <- xtrain[, S, drop = F]


        fit <- multinom(y~.,data = data.frame(x = xtrain.r,y = ytrain), weights = weights, maxit = maxit)
        ytrain.pred <- as.numeric(predict(fit,data = data.frame(xtrain.r)))
        }

    if (base == "svm") {

        if (!is.character(kernel)) {
            kernel <- "linear"
        }

            folds <- createFolds(ytrain, k = cv)
            subspace.list <- sapply(1:B2, function(i) {
                # the last row is training error for each i in 1:B2
                Si <- S[, i][!is.na(S[, i])]  # current subspace
                mean(sapply(1:cv, function(j) {
                  fit <- svm(x = xtrain[-folds[[j]], Si, drop = F], y = ytrain[-folds[[j]]], kernel = kernel, type = "C-classification")
                  mean(predict(fit, xtrain[folds[[j]], Si, drop = F]) != ytrain[folds[[j]]], na.rm = TRUE)
                }))
            })


        i0 <- which.min(subspace.list)
        S <- S[!is.na(S[, i0]), i0]  # final optimal subspace

        xtrain.r <- xtrain[, S, drop = F]
        fit <- svm(x = xtrain.r, y = ytrain, kernel = kernel, type = "C-classification")
        ytrain.pred <- predict(fit, xtrain.r)
    }

    if (base == "knn") {

        folds <- createFolds(ytrain, k = cv)
        subspace.list <- sapply(1:B2, function(i) {
            # the last row is training error for each i in 1:B2
            Si <- S[, i][!is.na(S[, i])]  # current subspace
            knn.test <- sapply(k, function(l) {
                mean(sapply(1:cv, function(j) {
                    mean(predict(knn3(x = xtrain[-folds[[j]], Si, drop = F], y = factor(ytrain[-folds[[j]]]), k = l, use.all = FALSE), xtrain[folds[[j]], Si, drop = F], type = "class") != ytrain[folds[[j]]], na.rm = TRUE)
                }))
            })
            min(knn.test)
        })

        i0 <- which.min(subspace.list)
        S <- S[!is.na(S[, i0]), i0]  # final optimal subspace

        xtrain.r <- xtrain[, S, drop = F]

        knn.test <- sapply(k, function(l) {
            mean(sapply(1:cv, function(j) {
                mean(predict(knn3(x = xtrain.r[-folds[[j]], ,drop = F], y = factor(ytrain[-folds[[j]]]), k = l, use.all = FALSE), xtrain.r[folds[[j]], , drop = F], type = "class") != ytrain[folds[[j]]], na.rm = TRUE)
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
                mean(sapply(1:cv, function(j) {
                  fit <- rpart(y ~ ., data = data.frame(x = xtrain[-folds[[j]], Si, drop = F], y = ytrain[-folds[[j]]]), method = "class")
                  mean((as.numeric(predict(fit, data.frame(x = xtrain[folds[[j]], Si, drop = F]), type = "class"))) != ytrain[folds[[j]]],
                    na.rm = TRUE)
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
            #set.seed(i)
            #cat(i)
            # the last row is training error for each i in 1:B2
            Si <- S[, i][!is.na(S[, i])]  # current subspace
            mean(sapply(1:cv, function(j) {
                #cat(j)
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
            }
            )
            )
        })

        i0 <- which.min(subspace.list)
        S <- S[!is.na(S[, i0]), i0]  # final optimal subspace

        cat("Length of S :",length(S),"\n")

        xtrain.r <- xtrain[, S, drop = F]
        fit <- lda(x = as.matrix(xtrain.r), grouping = ytrain)
        ytrain.pred <- predict(fit, as.matrix(xtrain.r))$class
    }

    if (base == "qda") {

        folds <- createFolds(ytrain, k = cv)
        subspace.list <- sapply(1:B2, function(i) {
            Si <- S[, i][!is.na(S[, i])]  # current subspace
            mean(sapply(1:cv, function(j) {
              xtrain_j <- xtrain[-folds[[j]], Si, drop = F]
              ytrain_j <- ytrain[-folds[[j]]]
              n_start <- sapply(1:nmulti, function(i)sum(ytrain_j == i))

              #ind_to_remove <-  remove_ind(xtrain_j, ytrain_j)


              Sigma.mle.qda <- compute_Sigma(xtrain_j, ytrain_j, nmulti, n_start)
              mu.mle.qda <- compute_mu(xtrain_j, ytrain_j, nmulti)
              ind_to_keep <- remove_linear_combo(1:length(Si), Sigma.mle.qda, mu.mle.qda, length(Si), nmulti)
              #Sigma.mle_j <- compute_Sigma(xtrain_j, ytrain_j, nmulti, n_start)

              #mu.mle_j <- compute_mu(xtrain_j, ytrain_j, nmulti)
              #ind_to_remove_combo_j <- remove_linear_combo(Si, Sigma.mle_j, mu.mle_j, D)
              #ind_to_remove_all <- unique(c(ind_to_remove,
               #                             ind_to_remove_combo_j))
              #ind_to_keep <- setdiff(1:length(Si), remove_ind_j)
              ind_to_remove_constant <- na.omit(as.numeric(unlist(lapply(unique(ytrain[-folds[[j]]]), function(group){
                  inds <- which(ytrain[-folds[[j]]] == group)
                  newx <- xtrain[-folds[[j]], Si[ind_to_keep], drop = F]
                  constant_inds <- which(apply(newx[inds, , drop = F], 2, var) == 0)
                  constant_inds
              }))))
              ind_to_keep <- as.numeric(ind_to_keep)

              if(length(ind_to_remove_constant) > 0 && ind_to_keep == ind_to_remove_constant){
                  ind_to_keep <- ind_to_keep[1]
              }else{
                  ind_to_keep <- setdiff(ind_to_keep, ind_to_remove_constant)
              }

              if(length(ind_to_keep) == 1){
                  llabel <- sapply(1:nmulti,function(r){length(which(ytrain[-folds[[j]]] == r))})
                  llabel <- c(1,cumsum(llabel)[1:(nmulti-1)])
                  dis <- rep(0,length(xtrain[-folds[[j]], Si[ind_to_keep], drop = F]))
                  dis[llabel] <- 1e-20

                  fit <- qda(x = xtrain[-folds[[j]], Si[ind_to_keep], drop = F]+as.matrix(dis), grouping = ytrain[-folds[[j]]])
              } else{
                  fit <- qda(x = xtrain[-folds[[j]], Si[ind_to_keep], drop = F], grouping = ytrain[-folds[[j]]])
              }
              mean(predict(fit, xtrain[folds[[j]], Si[ind_to_keep], drop = F])$class != ytrain[folds[[j]]], na.rm = TRUE)
            }))
        })
        i0 <- which.min(subspace.list)
        S <- S[!is.na(S[, i0]), i0]  # final optimal subspace
        xtrain.r <- xtrain[, S, drop = F]
        fit <- qda(x = as.matrix(xtrain.r), grouping = ytrain)
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
                cat(i,"\n")
                #set.seed(i)
                Si <- S[, i][!is.na(S[, i])]   # current subspace
                xtrain.r <- xtrain[, Si, drop = F]
                xtrain.r.df <- data.frame(xtrain.r)
                if (base.list[i] == "qda"){
                    mean(sapply(1:cv, function(j) {
                        cat(j,"\n")
                        xtrain_j <- xtrain[-folds[[j]], Si, drop = F]
                        ytrain_j <- ytrain[-folds[[j]]]
                        n_start <- sapply(1:nmulti, function(i)sum(ytrain_j == i))


                        Sigma.mle.qda <- compute_Sigma(xtrain_j, ytrain_j, nmulti, n_start)
                        mu.mle.qda <- compute_mu(xtrain_j, ytrain_j, nmulti)
                        ind_to_keep <- remove_linear_combo(1:length(Si), Sigma.mle.qda, mu.mle.qda, length(Si), nmulti)
                        ind_to_keep <- na.omit(ind_to_keep)
                        #Sigma.mle_j <- compute_Sigma(xtrain_j, ytrain_j, nmulti, n_start)

                        #mu.mle_j <- compute_mu(xtrain_j, ytrain_j, nmulti)
                        #ind_to_remove_combo_j <- remove_linear_combo(Si, Sigma.mle_j, mu.mle_j, D)
                        #ind_to_remove_all <- unique(c(ind_to_remove,
                        #                             ind_to_remove_combo_j))
                        #ind_to_keep <- setdiff(1:length(Si), remove_ind_j)

                        ind_to_remove_constant <- na.omit(as.numeric(unlist(lapply(unique(ytrain[-folds[[j]]]), function(group){
                            inds <- which(ytrain[-folds[[j]]] == group)
                            newx <- xtrain[-folds[[j]], Si[ind_to_keep], drop = F]
                            constant_inds <- which(apply(newx[inds, , drop = F], 2, var) == 0)
                            constant_inds
                        }))))

                        ind_to_keep <- as.numeric(ind_to_keep)

                        if(length(ind_to_remove_constant) > 0 && ind_to_keep == ind_to_remove_constant){
                            ind_to_keep <- ind_to_keep[1]
                        }else{
                            ind_to_keep <- setdiff(ind_to_keep, ind_to_remove_constant)
                        }

                       if(length(ind_to_keep) == 1){
                           llabel <- sapply(1:nmulti,function(r){length(which(ytrain[-folds[[j]]] == r))})
                           llabel <- c(1,cumsum(llabel)[1:(nmulti-1)])
                           dis <- rep(0,length(xtrain[-folds[[j]], Si[ind_to_keep], drop = F]))
                           dis[llabel] <- 1e-20

                        fit <- qda(x = xtrain[-folds[[j]], Si[ind_to_keep], drop = F]+as.matrix(dis), grouping = ytrain[-folds[[j]]])
                        } else{
                            fit <- qda(x = xtrain[-folds[[j]], Si[ind_to_keep], drop = F], grouping = ytrain[-folds[[j]]])
                        }
                        mean(predict(fit, xtrain[folds[[j]], Si[ind_to_keep], drop = F])$class != ytrain[folds[[j]]], na.rm = TRUE)
                    }))
                } else if (base.list[i] == "lda"){

                    mean(sapply(1:cv, function(j) {
                        cat(j,"\n")
                        fit <- lda(x = xtrain.r.df[-folds[[j]], , drop = F], grouping = ytrain[-folds[[j]]])
                        mean(predict(fit, xtrain.r.df[folds[[j]], , drop = F])$class != ytrain[folds[[j]]], na.rm = TRUE)
                    }))
                } else if (base.list[i] == "svm"){
                    mean(sapply(1:cv, function(j) {
                        fit <- svm(x = xtrain.r[-folds[[j]], , drop = F], y = ytrain[-folds[[j]]], kernel = kernel, type = "C-classification")
                    mean(predict(fit, xtrain.r[folds[[j]], , drop = F]) != ytrain[folds[[j]]], na.rm = TRUE)
                    }))
                } else if (base.list[i] == "tree"){
                    ytrain <- factor(ytrain)
                    mean(sapply(1:cv, function(j) {
                        fit <- rpart(y ~ ., data = data.frame(x = xtrain.r[-folds[[j]], , drop = F], y = ytrain[-folds[[j]]]), method = "class")
                        mean((as.numeric(predict(fit, data.frame(x = xtrain.r[folds[[j]], , drop = F]), type = "class"))) != ytrain[folds[[j]]], na.rm = TRUE)
                    }))
                } else if (base.list[i] == "logistic"){
                    if (is.null(weights)) {
                        weights <- rep(1, length(ytrain))/length(ytrain)
                    }
                    # nnet::multinom maxit = 10
                    if(!exists("maxit")){maxit = 80}
                    mean(sapply(1:cv, function(j) {
                        fit <- multinom(y ~ ., data = data.frame(x = xtrain.r[-folds[[j]], , drop = F], y = ytrain[-folds[[j]]]),maxit = maxit)
                        mean(as.numeric(predict(fit, data.frame(x = xtrain.r[folds[[j]], , drop = F])))  != ytrain[folds[[j]]], na.rm = TRUE)
                    }))
                } else if (base.list[i] == "knn") {
                    knn.test <- sapply(k, function(l) {
                        mean(sapply(1:cv, function(j) {
                            fit <- knn3(x = xtrain.r[-folds[[j]], , drop = F], y = factor(ytrain[-folds[[j]]]), k = l, use.all = FALSE)
                            mean(predict(fit, xtrain.r[folds[[j]], , drop = F], type = "class")  != ytrain[folds[[j]]], na.rm = TRUE)
                        }))
                    })
                    min(knn.test)
                }

                }
        )

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
                mean(sapply(1:cv, function(j) {
                    mean(predict(knn3(x = xtrain.r[-folds[[j]], ,drop = F], y = factor(ytrain[-folds[[j]]]), k = l, use.all = FALSE), xtrain.r[folds[[j]], , drop = F], type = "class") != ytrain[folds[[j]]], na.rm = TRUE)
                }))
            })
            k.op <- k[which.min(knn.test)]
            fit <- knn3(x = xtrain.r, y = factor(ytrain), k = k.op, use.all = FALSE)
            ytrain.pred <- predict(fit, xtrain.r, type = "class")
            }

        return(list(fit = fit, ytrain.pred = ytrain.pred, subset = S, base.select = base.list[i0]))
    }

}
