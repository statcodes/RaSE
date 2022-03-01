
RaSubsetnew <- function(xtrain, ytrain, xval, yval, B2, S, base, k, criterion, cv, t0.mle = NULL, t1.mle = NULL, mu0.mle = NULL,  mu1.mle = NULL, Sigma.mle = NULL, Sigma0.mle = NULL, Sigma1.mle = NULL, gam = NULL, kl.k = NULL, lower.limits = NULL, upper.limits = NULL, weights = NULL, ...) {
    list2env(list(...), environment())



    if (all(base == "logistic")) {

        if (is.null(weights)) {
            weights <- rep(1, length(ytrain))/length(ytrain)
        }

        folds <- createFolds(ytrain, k = cv)
        subspace.list <- sapply(1:B2, function(i) {
            # the last row is training error for each i in 1:B2
            Si <- S[, i][!is.na(S[, i])]  # current subspace
            mean(sapply(1:cv, function(j) {
                fit <- multinom(y ~ ., data = data.frame(x = xtrain[-folds[[j]], Si, drop = F], y = ytrain[-folds[[j]]]))
                mean(as.numeric(predict(fit, data.frame(x = xtrain[folds[[j]], Si, drop = F])))  != ytrain[folds[[j]]], na.rm = TRUE)
            }))
        })

        i0 <- which.min(subspace.list)
        S <- S[!is.na(S[, i0]), i0]  # final optimal subspace

        xtrain.r <- xtrain[, S, drop = F]

        # nnet::multinom maxit = 10
        if(!exists("maxit")){maxit = 80}

        fit <- multinom(y~.,data = data.frame(x = xtrain.r,y = ytrain), weights = weights,maxit = maxit)

        ytrain.pred <- as.numeric(predict(fit,data = data.frame(xtrain.r)))
        }

    if (all(base == "svm")) {

        if (!is.character(kernel)) {
            kernel <- "linear"
        }

            folds <- createFolds(ytrain, k = cv)
            subspace.list <- sapply(1:B2, function(i) {
                # the last row is training error for each i in 1:B2
                Si <- S[, i][!is.na(S[, i])]  # current subspace
                mean(sapply(1:cv, function(j) {
                  fit <- svm(x = xtrain[-folds[[j]], Si, drop = F], y = ytrain[-folds[[j]]], kernel = kernel, type = "C-classification")
                  mean(as.numeric(predict(fit, xtrain[folds[[j]], Si, drop = F])) != ytrain[folds[[j]]], na.rm = TRUE)
                }))
            })


        i0 <- which.min(subspace.list)
        S <- S[!is.na(S[, i0]), i0]  # final optimal subspace

        xtrain.r <- xtrain[, S, drop = F]
        fit <- svm(x = xtrain.r, y = ytrain, kernel = kernel, type = "C-classification")
        ytrain.pred <- as.numeric(predict(fit, xtrain.r))
    }

    if (all(base == "knn")) {

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

    if (all(base == "tree")) {

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

    if (all(base == "lda")) {

        folds <- createFolds(ytrain, k = cv)
        subspace.list <- sapply(1:B2, function(i) {
            # the last row is training error for each i in 1:B2
            Si <- S[, i][!is.na(S[, i])]  # current subspace
            mean(sapply(1:cv, function(j) {
                mean(predict(lda(x = xtrain[-folds[[j]], Si, drop = F], grouping = ytrain[-folds[[j]]]), xtrain[folds[[j]], Si, drop = F])$class !=
                         ytrain[folds[[j]]], na.rm = TRUE)
            }))
        })

        i0 <- which.min(subspace.list)
        S <- S[!is.na(S[, i0]), i0]  # final optimal subspace

        xtrain.r <- xtrain[, S, drop = F]
        fit <- lda(x = as.matrix(xtrain.r), grouping = ytrain)
        ytrain.pred <- predict(fit, as.matrix(xtrain.r))$class
    }

    if (all(base == "qda")) {

        folds <- createFolds(ytrain, k = cv)
        subspace.list <- sapply(1:B2, function(i) {
            Si <- S[, i][!is.na(S[, i])]  # current subspace
            mean(sapply(1:cv, function(j) {
                mean(predict(qda(x = xtrain[-folds[[j]], Si, drop = F], grouping = ytrain[-folds[[j]]]), xtrain[folds[[j]], Si, drop = F])$class !=
                         ytrain[folds[[j]]], na.rm = TRUE)
            }))
        })

        i0 <- which.min(subspace.list)
        S <- S[!is.na(S[, i0]), i0]  # final optimal subspace

        xtrain.r <- xtrain[, S, drop = F]
        fit <- qda(x = as.matrix(xtrain.r), grouping = ytrain)
        ytrain.pred <- predict(fit, as.matrix(xtrain.r))$class
    }

    if (length(unique(base)) == 1) {
        return(list(fit = fit, ytrain.pred = as.numeric(ytrain.pred), subset = S))
    }

    # super RaSE
    # ---------------------------------------------------

    if (length(unique(base)) > 1) {
        if (criterion == "training") {
            subspace.list <- sapply(1:B2, function(i) {
                Si <- S[, i][!is.na(S[, i])]  # current subspace
                xtrain.r <- xtrain[, Si, drop = F]
                if (base[i] == "qda"){
                    mean(predict(qda(x = xtrain.r, grouping = ytrain), xtrain.r)$class != ytrain, na.rm = TRUE)
                } else if (base[i] == "lda"){
                    mean(predict(lda(x = xtrain.r, grouping = ytrain), xtrain.r)$class != ytrain, na.rm = TRUE)
                } else if (base[i] == "svm"){
                    mean(as.numeric(predict(svm(x = xtrain.r, y = ytrain, kernel = kernel, type = "C-classification", ...), xtrain.r)) - 1 != ytrain, na.rm = TRUE)
                } else if (base[i] == "tree"){
                    ytrain <- factor(ytrain)
                    fit <- rpart(y ~ ., data = data.frame(x = xtrain.r, y = ytrain), method = "class")
                    score <- as.numeric(predict(fit, data.frame(x = xtrain.r), type = "prob")[, 2])
                    -auc(ytrain, score)
                } else if (base[i] == "randomforest"){
                    mean(as.numeric(predict(randomForest(x = xtrain.r, y = factor(ytrain)), xtrain.r)) - 1 != factor(ytrain), na.rm = TRUE)
                } else if (base[i] == "logistic"){
                    mean(as.numeric(I(predict(glm(y ~ ., data = data.frame(x = xtrain.r, y = ytrain), family = "binomial", weights = weights), data.frame(x = xtrain.r)) > 0)) != ytrain, na.rm = TRUE)
                } else if (base[i] == "knn"){
                    stop("'criterion' cannot be 'training' when base classifiers include 'knn'! Please check your input.")
                }
            })
        } else if (criterion == "cv") {
            if (!is.character(kernel)) {
                kernel <- "linear"
            }
            folds <- createFolds(ytrain, k = cv)
            subspace.list <- sapply(1:B2, function(i) {
                Si <- S[, i][!is.na(S[, i])]  # current subspace
                xtrain.r <- xtrain[, Si, drop = F]
                if (base[i] == "qda"){
                    mean(sapply(1:cv, function(j) {
                        mean(predict(qda(x = xtrain.r[-folds[[j]], , drop = F], grouping = ytrain[-folds[[j]]]), xtrain.r[folds[[j]], , drop = F])$class !=
                                 ytrain[folds[[j]]], na.rm = TRUE)
                    }))
                } else if (base[i] == "lda"){
                    mean(sapply(1:cv, function(j) {
                        mean(predict(lda(x = xtrain.r[-folds[[j]], , drop = F], grouping = ytrain[-folds[[j]]]), xtrain.r[folds[[j]], , drop = F])$class !=
                                 ytrain[folds[[j]]], na.rm = TRUE)
                    }))
                } else if (base[i] == "svm"){
                    mean(sapply(1:cv, function(j) {
                        mean(as.numeric(predict(svm(x = xtrain.r[-folds[[j]], , drop = F], y = ytrain[-folds[[j]]], kernel = kernel, type = "C-classification", ...), xtrain.r[folds[[j]], , drop = F])) - 1 != ytrain[folds[[j]]], na.rm = TRUE)
                    }))
                } else if (base[i] == "tree"){
                    ytrain <- factor(ytrain)
                    mean(sapply(1:cv, function(j) {
                        fit <- rpart(y ~ ., data = data.frame(x = xtrain.r[-folds[[j]], , drop = F], y = ytrain[-folds[[j]]]), method = "class")
                        mean((as.numeric(predict(fit, data.frame(x = xtrain.r[folds[[j]], , drop = F]), type = "class")) - 1) != ytrain[folds[[j]]], na.rm = TRUE)
                    }))
                } else if (base[i] == "randomforest"){
                    mean(sapply(1:cv, function(j) {
                        mean(as.numeric(predict(randomForest(x = xtrain.r[-folds[[j]], , drop = F], y = factor(ytrain)[-folds[[j]]]), xtrain.r[folds[[j]], , drop = F])) - 1 != factor(ytrain)[folds[[j]]], na.rm = TRUE)
                    }))
                } else if (base[i] == "logistic"){
                    mean(sapply(1:cv, function(j) {
                        mean(as.numeric(I(predict(glm(y ~ ., data = data.frame(x = xtrain.r[-folds[[j]], , drop = F], y = ytrain[-folds[[j]]]), family = "binomial", weights = weights), data.frame(x = xtrain.r[folds[[j]], , drop = F])) > 0)) != ytrain[folds[[j]]], na.rm = TRUE)
                    }))
                } else if (base[i] == "knn") {
                    knn.test <- sapply(k, function(l) {
                        mean(sapply(1:cv, function(j) {
                            mean(predict(knn3(x = xtrain.r[-folds[[j]], , drop = F], y = factor(ytrain[-folds[[j]]]), k = l, use.all = FALSE), xtrain.r[folds[[j]], , drop = F], type = "class") != ytrain[folds[[j]]], na.rm = TRUE)
                        }))
                    })
                    min(knn.test)
                }
            })
        } else if (criterion == "auc") {
            subspace.list <- sapply(1:B2, function(i) {
                Si <- S[, i][!is.na(S[, i])]  # current subspace
                xtrain.r <- xtrain[, Si, drop = F]
                if (base[i] == "qda"){
                    score <- as.numeric(predict(qda(x = xtrain.r, grouping = ytrain), xtrain.r)$x)
                    -auc(ytrain, score)
                } else if (base[i] == "lda"){
                    score <- as.numeric(predict(lda(x = xtrain.r, grouping = ytrain), xtrain.r)$x)
                    -auc(ytrain, score)
                } else if (base[i] == "svm"){
                    stop("'criterion' cannot be 'auc' when base classifiers include 'svm'! Please check your input.")
                } else if (base[i] == "tree"){
                    ytrain <- factor(ytrain)
                    fit <- rpart(y ~ ., data = data.frame(x = xtrain.r, y = ytrain), method = "class")
                    score <- as.numeric(predict(fit, data.frame(x = xtrain.r), type = "prob")[, 2])
                    -auc(ytrain, score)
                } else if (base[i] == "randomforest"){
                    score <- as.numeric(predict(randomForest(x = xtrain.r, y = factor(ytrain), ...), xtrain.r, type = "prob")[, 1])
                    -auc(ytrain, score)
                } else if (base[i] == "logistic"){
                    score <- predict(glm(y ~ ., data = data.frame(x = xtrain.r, y = ytrain), family = "binomial", weights = weights), data.frame(x = xtrain.r))
                    -auc(ytrain, score)
                } else if (base[i] == "knn") {
                    knn.test <- sapply(k, function(j) {
                        rs <- knn.cv(xtrain.r, ytrain, j, use.all = FALSE, prob = TRUE)
                        - auc(ytrain, attr(rs,"prob"))
                    })
                    min(knn.test)
                }
            })
        }

        i0 <- which.min(subspace.list)
        S <- S[!is.na(S[, i0]), i0]  # final optimal subspace
        xtrain.r <- xtrain[, S, drop = F]
        if (base[i0] == "qda"){
            fit <- qda(x = xtrain.r, grouping = ytrain, ...)
            ytrain.pred <- as.numeric(predict(fit, xtrain.r)$class) - 1
        }
        if (base[i0] == "lda"){
            fit <- lda(x = xtrain.r, grouping = ytrain, ...)
            ytrain.pred <- as.numeric(predict(fit, xtrain.r)$class) - 1
        }
        if (base[i0] == "svm"){
            fit <- svm(x = xtrain.r, y = ytrain, kernel = kernel, type = "C-classification", ...)
            ytrain.pred <- as.numeric(predict(fit, xtrain.r)) - 1
        }
        if (base[i0] == "tree"){
            fit <- rpart(y ~ ., data = data.frame(x = xtrain.r, y = factor(ytrain)), method = "class")
            ytrain.pred <- as.numeric(predict(fit, data.frame(x = xtrain.r), type = "class")) - 1
        }
        if (base[i0] == "randomforest"){
            fit <- randomForest(x = xtrain.r, y = factor(ytrain))
            ytrain.pred <- as.numeric(predict(fit, xtrain.r)) - 1
        }
        if (base[i0] == "logistic"){
            fit <- glm(y ~ ., data = data.frame(x = xtrain.r, y = ytrain), family = "binomial", weights = weights)
            ytrain.pred <- as.numeric(I(predict(fit, data.frame(x = xtrain.r)) > 0))
        }
        if (base[i0] == "knn") {
            if (criterion == "loo") {
                knn.test <- sapply(k, function(j) {
                    mean(knn.cv(xtrain.r, ytrain, j, use.all = FALSE) != ytrain, na.rm = TRUE)
                })
            } else if (criterion == "cv") {
                knn.test <- sapply(k, function(l) {
                    mean(sapply(1:cv, function(j) {
                        mean(predict(knn3(x = xtrain.r[-folds[[j]], , drop = F], y = factor(ytrain[-folds[[j]]]), k = l, use.all = FALSE), xtrain.r[folds[[j]], , drop = F], type = "class") != ytrain[folds[[j]]], na.rm = TRUE)
                    }))
                })
            } else if (criterion == "auc") {
                knn.test <- sapply(k, function(j) {
                    rs <- knn.cv(xtrain.r, ytrain, j, use.all = FALSE, prob = TRUE)
                    - auc(rs, attr(rs,"prob"))
                })
            } else if (criterion == "validation") {
                xval.r <- xval[, S, drop = F]
                knn.test <- sapply(k, function(j) {
                    fit <- knn3(x = xtrain.r, y = factor(ytrain), k = j, use.all = FALSE)
                    mean(as.numeric(predict(fit, xval.r, type = "class")) - 1 != yval)
                })
            }

            k.op <- k[which.min(knn.test)]
            fit <- knn3(x = xtrain.r, y = factor(ytrain), k = k.op, use.all = FALSE)
            ytrain.pred <- as.numeric(predict(fit, xtrain.r, type = "class")) - 1
        }



        return(list(fit = fit, ytrain.pred = ytrain.pred, subset = S))
    }





}
