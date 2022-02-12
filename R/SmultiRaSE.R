#' Construct the super multi-label random subspace ensemble classifier.
#'
#' \code{SmRaSE} is a general ensemble classification framework, adapted from RaSE algorithm, to solve the sparse multi class classification problem. Like RaSE algorithm, for each of the B1 weak learners, B2 random subspaces are generated and the optimal one is chosen to train the model on the basis of some criterion.
#' @export
#' @importFrom MASS lda
#' @importFrom MASS qda
#' @importFrom MASS mvrnorm
#' @importFrom class knn
#' @importFrom class knn.cv
#' @importFrom caret knn3
#' @importFrom caret createFolds
#' @importFrom caret findLinearCombos
#' @importFrom caret knnreg
#' @importFrom doParallel registerDoParallel
#' @importFrom doParallel stopImplicitCluster
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom parallel detectCores
#' @importFrom rpart rpart
#' @importFrom nnet multinom
#' @importFrom randomForest randomForest
#' @importFrom e1071 svm
#' @importFrom stats glm
#' @importFrom stats predict
#' @importFrom stats nlm
#' @importFrom stats rmultinom
#' @importFrom stats rgamma
#' @importFrom stats dgamma
#' @importFrom stats ecdf
#' @importFrom stats optimise
#' @importFrom stats cov
#' @importFrom stats cor
#' @importFrom stats var
#' @importFrom stats rnorm
#' @importFrom stats rt
#' @importFrom stats runif
#' @importFrom stats deviance
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 expr
#' @importFrom ggplot2 labs
#' @importFrom gridExtra grid.arrange
#' @importFrom formatR tidy_eval
#' @importFrom FNN KL.divergence
#' @importFrom ranger ranger
#' @importFrom KernelKnn KernelKnn
#' @importFrom utils data
#' @importFrom glmnet glmnet
#' @importFrom glmnet predict.glmnet
#' @importFrom ModelMetrics auc
#' @importFrom lava Expand
#'
#' @param xtrain n * p observation matrix. n observations, p features.
#' @param ytrain n observations with k classes.
#' @param B1 the number of weak learners. Default = 200.
#' @param B2 the number of subspace candidates generated for each weak learner. Default = 500.
#' @param D_max the maximal subspace size when generating random subspaces. Default = NULL, which is \eqn{floor(min(\sqrt n, p))}. For classical RaSE with a single classifier type, D_max is a positive integer. For super RaSE with multiple classifier types, D_max is a vector indicating different maximum D values used for each base classifier type (the corresponding classifier types should be noted in the names of the vector).
#' @seealso \code{\link{predict.SmultiRaSE}}.
#' @examples
#' set.seed(0, kind = "L'Ecuyer-CMRG")
#' train.data <- RaModel("multi_classification", model.no = 1, n = 100,
#' p = 50, p0 = rep(1/4,4))
#' test.data <- RaModel("multi_classification", model.no = 1, n = 100,
#' p = 50, p0 = rep(1/4,4))
#' xtrain <- train.data$x
#' colnames(xtrain) <- paste0("V",1:dim(xtrain)[2])
#' ytrain <- train.data$y
#' xtest <- test.data$x
#' colnames(xtest) <- paste0("V",1:dim(xtest)[2])
#' ytest <- test.data$y
#'
#' # test mRaSE classifier with LDA base classifier
#' fit <- SmultiRase(xtrain, ytrain, B1 = 20, B2 = 50, iteration = 0,
#' base = 'lda', cores = 1)
#' mean(predict(fit, xtest) != ytest)
#'
#' \dontrun{
#' # test mRaSE classifier with LDA base classifier and 1 iteration round
#' fit <- SmultiRase(xtrain, ytrain, B1 = 20, B2 = 50, iteration = 1,
#' base = 'lda', cores = 6)
#' mean(predict(fit, xtest) != ytest)
#'
#' # test mRaSE classifier with KNN base classifier
#' fit <- SmultiRase(xtrain, ytrain, B1 = 20, B2 = 50, iteration = 0,
#' base = 'knn', cores = 6)
#' mean(predict(fit, xtest) != ytest)
#'
#' # test mRaSE classifier with logistic regression base classifier
#' fit <- SmultiRase(xtrain, ytrain, B1 = 20, B2 = 50, iteration = 0,
#' base = 'logistic', cores = 6)
#' mean(predict(fit, xtest) != ytest)
#'
#' # test mRaSE classifier with tree base classifier
#' fit <- SmultiRase(xtrain, ytrain, B1 = 20, B2 = 50, iteration = 0,
#' base = 'svm', cores = 6)
#' mean(predict(fit, xtest) != ytest)
#'
#' # test mRaSE classifier with tree base classifier
#' fit <- SmultiRase(xtrain, ytrain, B1 = 20, B2 = 50, iteration = 0,
#' base = 'tree', cores = 6)
#' mean(predict(fit, xtest) != ytest)
#'
#' # fit a super RaSE classifier by sampling base learner from kNN, LDA and logistic
#' # regression in equal probability
#' fit <- SmultiRase(xtrain, ytrain, B1 = 20, B2 = 50,
#' base = c("knn", "lda", "logistic"), iteration = 1, cores = 6)
#' mean(predict(fit, xtest) != ytest)
#'
#' }
SmultiRase <- function(xtrain, ytrain,
                       xval = NULL, yval = NULL,
                       B1 = 50, B2 = 100,
                       D = NULL, dist = NULL,
                       base = "lda",
                       super = list(type = "seperate", base.update = TRUE),
                       criterion = "cv",
                       ranking = TRUE,
                       k = c(3, 5, 7, 9, 11),
                       cores = 1,
                       seed = NULL,
                       iteration = 1,
                       cutoff = TRUE,
                       cv = 5,
                       scale = FALSE,
                       C0 = 0.1,
                       kl.k = NULL,
                       lower.limits = NULL,
                       upper.limits = NULL,
                       weights = NULL, ...) {

  if(!is.matrix(xtrain) & !is.data.frame(xtrain)){
    stop("xtrain needs to be a matrix or data frame!")
  }

  if(!is.vector(ytrain)){
    stop("ytrain needs to be a vector")
  }
  if(any(is.na(colnames(xtrain))) ||
     any(is.null(colnames(xtrain)))){
   stop("column names of xtrain contains NA or NULL! Please make sure the column names of xtrain and xtest have no NAs or NULLs!")
  }
  #label from 1 to K
  trans_inf <- labeltrans(ytrain)
  ytrain <- trans_inf$vnew
  lab_table <- trans_inf$label

  if (!is.null(seed)) {
    set.seed(seed, kind = "L'Ecuyer-CMRG")
  }

  xtrain <- as.matrix(xtrain)

  if (is.character(base)) {
    if (!all(base %in%
             c("lda", "qda", "knn", "logistic",
               "tree", "svm"))){
      stop("'base' can only be chosen from 'lda',
           'qda', 'knn', 'logistic', 'tree' and 'svm'!")
    }
  }

  base.dist <- NULL
  if (length(base) > 1) {  #super RaSE has base.dist
      base.dist <- rep(1/length(base), length(base))
      names(base.dist) <- base
  }

  #single base doesn't has base.dist


  # features of input
  p <- ncol(xtrain)
  n <- length(ytrain)
  nmulti <- length(unique(ytrain))

  # prior class
  n_start <- sapply(1:nmulti, function(i)sum(ytrain == i))



  if(is.null(kl.k)) {
    kl.k <- floor(sqrt(n_start))
  } ######### delete


  # scale the xtrain
  if (scale) {
    L <- scale_Rase(xtrain)
    xtrain <- L$data
    scale.center <- L$center
    scale.scale <- L$scale
  }


  registerDoParallel(cores)

  if(length(base) == 1){ #regular RaSE

  # base classifier : lda

  if (base == "lda") {

    # clean data (delete variables with high collinearity and variables that have constant values)
    delete.ind <- remove_ind(xtrain, ytrain)
    sig.ind <- setdiff(1:p, delete.ind)


    # estimate parameters
    if (is.null(D)) {
      D <- floor(min(sqrt(n), length(sig.ind)))
    }

    Sigma.mle <- 0
    for(i in 1:nmulti){
      Sigma.mle <- Sigma.mle + (n_start[i] - 1)*cov(xtrain[ytrain == i ,, drop = F])/n
    }

    mu.mle <- matrix(rep(0,p*nmulti),nmulti,p)
    for(i in 1:nmulti){
      mu.mle[i,] <- colMeans(xtrain[ytrain == i,,drop = F])
    }

    # start loops
    dist <- rep(1, p)
    dist[delete.ind] <- 0
    Dmin <- 1
    for (t in 1:(iteration + 1)) {
      output <- foreach(i = 1:B1, .combine = "rbind", .packages = "MASS") %dopar% {
        cat("i=",i," starts","\n")
        S <- sapply(1:B2, function(j) {

          S.size <- sample(Dmin:D, 1)

          c(sample(1:p, size = S.size, prob = dist),
            rep(NA, D - min(S.size, length(dist[dist != 0]))))

        }) # matrix of selected features (nrow = D, ncol = B2)

        S <- sapply(1:B2, function(j) {
          flag <- TRUE
          while (flag) {
            snew <- S[!is.na(S[, j]), j]
            if (length(snew) > 2) {

              ind0 <- findLinearCombos(Sigma.mle[snew, snew, drop = F])$remove
              if (!is.null(ind0)) {
                snew <- snew[-ind0]
              }
            }
            snew1 <- c(snew, rep(NA, D - length(snew)))
            if (sum(apply(mu.mle,1,var)) > 1e-10) {
              flag <- FALSE
            }
          }
          snew1
        })

        SRaSubset(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base, k = k,
                    criterion = criterion, cv = cv, nmulti = nmulti, ...)

      }

      if (is.matrix(output)) {
        subspace <- output[, 3]
      } else {
        subspace <- output[3]
      }

      # Get the number of features in each model

      s_num <- sapply(1:B1, function(i){length(subspace[[i]])})

      # Get the frequency of each feature in B1 models served as the new dist

      s <- rep(0, p)
      for (i in 1:length(subspace)) {
        s[subspace[[i]]] <- s[subspace[[i]]] + 1
      }

      dist <- s/B1
      dist[dist < C0/log(p)] <- C0/p
      dist[delete.ind] <- 0

      max_size <- sum(dist != 0)
      Dmin <- round(max(1,quantile(s_num,.25)-1.5*IQR(s_num)))
      D <- round(min(quantile(s_num,.75) + 1.5*IQR(s_num),
                     max_size))
    }

    if (is.matrix(output)) {
      ytrain.pred <- data.frame(matrix(unlist(output[, 2]), ncol = B1))
      fit.list <- output[, 1]
    } else {
      ytrain.pred <- data.frame(matrix(unlist(output[2]), ncol = B1))
      fit.list <- output[1]
    }

    y_count <- matrix(nrow = n,ncol = nmulti)
    for(i in 1:n){
      for(j in 1:nmulti){
        y_count[i,j] = sum(ytrain.pred[i,] == j)/B1
      }
    }

    # cv
    if(nmulti <= 5){
      cb_list <- lapply(1:nmulti,function(i){seq(0,0.9,0.1)})
      cb <- Expand(cb_list)
    }else{
      n_thre <- round(10^5/nmulti)
      cb <- matrix(runif(n_thre*nmulti),n_thre,nmulti)
    }

    cb <- as.matrix(cb)
    error <- foreach(i = 1:dim(cb)[1],.combine = "rbind",.packages = "MASS") %dopar%{
      alpha = cb[i,]
      pre.ind <- sapply(1:n,function(k){as.numeric(which.max(y_count[k,]+alpha))})
      mis <- sum(ytrain!=pre.ind)/n
      return(list(alpha = alpha,pre.ind = pre.ind,mis = mis))
    }

    error <- as.matrix(error)
    indc <-  which.min(as.numeric(error[,3]))
    cutoff <- error[[indc,1]]
    pre <- error[[indc,2]]


  }

  # base classifier : qda

  if (base == "qda") {

    # clean data (delete variables with high collinearity and variables that have constant values)
    delete.ind <- remove_ind(xtrain, ytrain)
    sig.ind <- setdiff(1:p, delete.ind)

    # estimate parameters
    if (is.null(D)) {
      D <- floor(min(sqrt(n), length(sig.ind)))
    }


    Sigma.mle.qda <- compute_Sigma(xtrain, ytrain, nmulti, n_start)

    mu.mle.qda <- compute_mu(xtrain, ytrain, nmulti)


    # start loops
    dist <- rep(1, p)
    dist[delete.ind] <- 0
    Dmin <- 1
    for (t in 1:(iteration + 1)) {
      output <- foreach(i = 1:B1, .combine = "rbind", .packages = "MASS") %dopar% {
        cat("i=",i," starts","\n")
        S <- sapply(1:B2, function(j) {

          S.size <- sample(Dmin:D, 1)

          c(sample(1:p, size = S.size, prob = dist),
            rep(NA, D - min(S.size, length(dist[dist != 0]))))

        }) # matrix of selected features (nrow = D, ncol = B2)

        S <- sapply(1:B2, function(j)
          remove_linear_combo(S[, j], Sigma.mle.qda, mu.mle.qda, D, nmulti)

        )

        SRaSubset(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base, k = k,
                    criterion = criterion, cv = cv, nmulti = nmulti, D = D, ...)

      }

      if (is.matrix(output)) {
        subspace <- output[, 3]
      } else {
        subspace <- output[3]
      }

      # Get the number of features in each model

      s_num <- sapply(1:B1, function(i){length(subspace[[i]])})

      # Get the frequency of each feature in B1 models served as the new dist

      s <- rep(0, p)
      for (i in 1:length(subspace)) {
        s[subspace[[i]]] <- s[subspace[[i]]] + 1
      }

      dist <- s/B1
      dist[dist < C0/log(p)] <- C0/p
      dist[delete.ind] <- 0

      max_size <- sum(dist != 0)
      Dmin <- round(max(1,quantile(s_num,.25)-1.5*IQR(s_num)))
      D <- round(min(quantile(s_num,.75) + 1.5*IQR(s_num),
                     max_size))
    }

    if (is.matrix(output)) {
      ytrain.pred <- data.frame(matrix(unlist(output[, 2]), ncol = B1))
      fit.list <- output[, 1]
    } else {
      ytrain.pred <- data.frame(matrix(unlist(output[2]), ncol = B1))
      fit.list <- output[1]
    }

    y_count <- matrix(nrow = n,ncol = nmulti)
    for(i in 1:n){
      for(j in 1:nmulti){
        y_count[i,j] = sum(ytrain.pred[i,] == j)/B1
      }
    }

    # cv
    if(nmulti <= 5){
      cb_list <- lapply(1:nmulti,function(i){seq(0,0.9,0.1)})
      cb <- Expand(cb_list)
    }else{
      n_thre <- round(10^5/nmulti)
      cb <- matrix(runif(n_thre*nmulti),n_thre,nmulti)
    }

    cb <- as.matrix(cb)
    error <- foreach(i = 1:dim(cb)[1],.combine = "rbind",.packages = "MASS") %dopar%{
      alpha = cb[i,]
      pre.ind <- sapply(1:n,function(k){as.numeric(which.max(y_count[k,]+alpha))})
      mis <- sum(ytrain!=pre.ind)/n
      return(list(alpha = alpha,pre.ind = pre.ind,mis = mis))
    }

    error <- as.matrix(error)
    indc <-  which.min(as.numeric(error[,3]))
    cutoff <- error[[indc,1]]
    pre <- error[[indc,2]]

  }

  # base classifier : knn

  if (base == "knn"){

    # estimate parameters
    if (is.null(D)) {
      D <- floor(min(sqrt(n), p))
    }

    if (all(is.null(lower.limits)) && all(is.null(upper.limits)) && base == "logistic" || criterion == "nric") {
      use.glmnet <- FALSE
    } else {
      use.glmnet <- TRUE
    }

    # start loops
    dist <- rep(1, p)
    Dmin <- 1
    for (t in 1:(iteration + 1)) {
      output <- foreach(i = 1:B1, .combine = "rbind", .packages = "MASS") %dopar% {
        cat("i=",i," starts","\n")
        S <- sapply(1:B2, function(j) {
          if (use.glmnet) {
            S.size <- sample(2:D, 1) # glmnet cannot fit the model with a single variable
            if (length(dist[dist != 0]) == 1) {
              stop ("Only one feature has positive sampling weights! 'glmnet' cannot be applied in this case! ")
            }
          } else {
            S.size <- sample(Dmin:D, 1)
          }
          c(sample(1:p, size = min(S.size, length(dist[dist != 0])), prob = dist), rep(NA, D - min(S.size, length(dist[dist !=
                                                                                                                         0]))))
        })

        SRaSubset(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base, k = k,
                    criterion = criterion, cv = cv)

      }


      if (is.matrix(output)) {
        subspace <- output[, 3]
      } else {
        subspace <- output[3]
      }

      # Get the number of features in each model

      s_num <- sapply(1:B1, function(i){length(subspace[[i]])})

      s <- rep(0, p)
      for (i in 1:length(subspace)) {
        s[subspace[[i]]] <- s[subspace[[i]]] + 1
      }

      dist <- s/B1
      dist[dist < C0/log(p)] <- C0/p

      max_size <- sum(dist != 0)
      Dmin <- round(max(1,quantile(s_num,.25)-1.5*IQR(s_num)))
      D <- round(min(quantile(s_num,.75) + 1.5*IQR(s_num),
                     max_size))
    }

    if (is.matrix(output)) {
      ytrain.pred <- data.frame(matrix(unlist(output[, 2]), ncol = B1))
      fit.list <- output[, 1]
    } else {
      ytrain.pred <- data.frame(matrix(unlist(output[2]), ncol = B1))
      fit.list <- output[1]
    }

    y_count <- matrix(nrow = n,ncol = nmulti)
    for(i in 1:n){
      for(j in 1:nmulti){
        y_count[i,j] = sum(ytrain.pred[i,] == j)/B1
      }
    }

    # cv
    if(nmulti <= 5){
      cb_list <- lapply(1:nmulti,function(i){seq(0,0.9,0.1)})
      cb <- Expand(cb_list)
    }else{
      n_thre <- round(10^5/nmulti)
      cb <- matrix(runif(n_thre*nmulti),n_thre,nmulti)
    }

    cb <- as.matrix(cb)
    error <- foreach(i = 1:dim(cb)[1],.combine = "rbind",.packages = "MASS") %dopar%{
      alpha = cb[i,]
      pre.ind <- sapply(1:n,function(k){as.numeric(which.max(y_count[k,]+alpha))})
      mis <- sum(ytrain!=pre.ind)/n
      return(list(alpha = alpha,pre.ind = pre.ind,mis = mis))
    }

    error <- as.matrix(error)
    indc <-  which.min(as.numeric(error[,3]))
    cutoff <- error[[indc,1]]
    pre <- error[[indc,2]]

  }

  # base classifier : logistic

  if (base == "logistic"){

    # estimate parameters
    if (is.null(D)) {
      D <- floor(min(sqrt(n), p))
    }

    if (all(is.null(lower.limits)) && all(is.null(upper.limits)) && base == "logistic" || criterion == "nric") {
      use.glmnet <- FALSE
    } else {
      use.glmnet <- TRUE
    }

    # start loops
    dist <- rep(1, p)
    Dmin <- 1
    for (t in 1:(iteration + 1)) {
      output <- foreach(i = 1:B1, .combine = "rbind", .packages = "MASS") %dopar% {
        cat("i=",i," starts","\n")
        S <- sapply(1:B2, function(j) {
          if (use.glmnet) {
            S.size <- sample(2:D, 1) # glmnet cannot fit the model with a single variable
            if (length(dist[dist != 0]) == 1) {
              stop ("Only one feature has positive sampling weights! 'glmnet' cannot be applied in this case! ")
            }
          } else {
            S.size <- sample(Dmin:D, 1)
          }
          c(sample(1:p, size = min(S.size, length(dist[dist != 0])), prob = dist), rep(NA, D - min(S.size, length(dist[dist !=
                                                                                                                         0]))))
        })

        SRaSubset(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base, k = k, criterion = criterion, cv = cv, nmulti = nmulti, ...)

      }


      if (is.matrix(output)) {
        subspace <- output[, 3]
      } else {
        subspace <- output[3]
      }

      # Get the number of features in each model

      s_num <- sapply(1:B1, function(i){length(subspace[[i]])})

      s <- rep(0, p)
      for (i in 1:length(subspace)) {
        s[subspace[[i]]] <- s[subspace[[i]]] + 1
      }

      dist <- s/B1
      dist[dist < C0/log(p)] <- C0/p

      max_size <- sum(dist != 0)
      Dmin <- round(max(1,quantile(s_num,.25)-1.5*IQR(s_num)))
      D <- round(min(quantile(s_num,.75) + 1.5*IQR(s_num),
                     max_size))
    }

    if (is.matrix(output)) {
      ytrain.pred <- data.frame(matrix(unlist(output[, 2]), ncol = B1))
      fit.list <- output[, 1]
    } else {
      ytrain.pred <- data.frame(matrix(unlist(output[2]), ncol = B1))
      fit.list <- output[1]
    }

    y_count <- matrix(nrow = n,ncol = nmulti)
    for(i in 1:n){
      for(j in 1:nmulti){
        y_count[i,j] = sum(ytrain.pred[i,] == j)/B1
      }
    }

    # cv
    if(nmulti <= 5){
      cb_list <- lapply(1:nmulti,function(i){seq(0,0.9,0.1)})
      cb <- Expand(cb_list)
    }else{
      n_thre <- round(10^5/nmulti)
      cb <- matrix(runif(n_thre*nmulti),n_thre,nmulti)
    }

    cb <- as.matrix(cb)
    error <- foreach(i = 1:dim(cb)[1],.combine = "rbind",.packages = "MASS") %dopar%{
      alpha = cb[i,]
      pre.ind <- sapply(1:n,function(k){as.numeric(which.max(y_count[k,]+alpha))})
      mis <- sum(ytrain!=pre.ind)/n
      return(list(alpha = alpha,pre.ind = pre.ind,mis = mis))
    }

    error <- as.matrix(error)
    indc <-  which.min(as.numeric(error[,3]))
    cutoff <- error[[indc,1]]
    pre <- error[[indc,2]]

  }

  # base classifier : svm

  if (base == "svm"){

    if (is.null(D)) {
      D <- floor(min(sqrt(n), p))
    }

    dist <- rep(1, p)
    Dmin <- 1
    for (t in 1:(iteration + 1)) {
      output <- foreach(i = 1:B1, .combine = "rbind", .packages = "MASS") %dopar% {
        cat("i=",i,"\n")
        S <- sapply(1:B2, function(j) {
          S.size <- sample(Dmin:D, 1)
          c(sample(1:p, size = min(S.size, length(dist[dist != 0])), prob = dist),
            rep(NA, D - min(S.size, length(dist[dist !=0]))))
        })

        SRaSubset(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base, k = k, kl.k = kl.k,
                    criterion = criterion, cv = cv)
      }



      if (is.matrix(output)) {
        subspace <- output[, 3]
      } else {
        subspace <- output[3]
      }

      # Get the number of features in each model

      s_num <- sapply(1:B1, function(i){length(subspace[[i]])})

      s <- rep(0, p)
      for (i in 1:length(subspace)) {
        s[subspace[[i]]] <- s[subspace[[i]]] + 1
      }

      dist <- s/B1
      dist[dist < C0/log(p)] <- C0/p

      max_size <- sum(dist != 0)
      Dmin <- round(max(1,quantile(s_num,.25)-1.5*IQR(s_num)))
      D <- round(min(quantile(s_num,.75) + 1.5*IQR(s_num),
                     max_size))
    }

    if (is.matrix(output)) {
      ytrain.pred <- data.frame(matrix(unlist(output[, 2]), ncol = B1))
      fit.list <- output[, 1]
    } else {
      ytrain.pred <- data.frame(matrix(unlist(output[2]), ncol = B1))
      fit.list <- output[1]
    }

    y_count <- matrix(nrow = n,ncol = nmulti)
    for(i in 1:n){
      for(j in 1:nmulti){
        y_count[i,j] = sum(ytrain.pred[i,] == j)/B1
      }
    }

    # cv
    if(nmulti <= 5){
      cb_list <- lapply(1:nmulti,function(i){seq(0,0.9,0.1)})
      cb <- Expand(cb_list)
    }else{
      n_thre <- round(10^5/nmulti)
      cb <- matrix(runif(n_thre*nmulti),n_thre,nmulti)
    }

    cb <- as.matrix(cb)
    error <- foreach(i = 1:dim(cb)[1],.combine = "rbind",.packages = "MASS") %dopar%{
      alpha = cb[i,]
      pre.ind <- sapply(1:n,function(k){as.numeric(which.max(y_count[k,]+alpha))})
      mis <- sum(ytrain!=pre.ind)/n
      return(list(alpha = alpha,pre.ind = pre.ind,mis = mis))
    }

    error <- as.matrix(error)
    indc <-  which.min(as.numeric(error[,3]))
    cutoff <- error[[indc,1]]
    pre <- error[[indc,2]]
  }

  # base classifier : tree

  if (base == "tree"){

    if (is.null(D)) {
      D <- floor(min(sqrt(n), p))
    }

    dist <- rep(1, p)
    Dmin <- 1
    for (t in 1:(iteration + 1)) {
      output <- foreach(i = 1:B1, .combine = "rbind", .packages = "MASS") %dopar% {
        cat("i=",i," starts","\n")
        S <- sapply(1:B2, function(j) {
          S.size <- sample(Dmin:D, 1)
          c(sample(1:p, size = min(S.size, length(dist[dist != 0])), prob = dist),
            rep(NA, D - min(S.size, length(dist[dist !=0]))))
        })

        SRaSubset(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base, k = k, kl.k = kl.k,
                    criterion = criterion, cv = cv)
      }



      if (is.matrix(output)) {
        subspace <- output[, 3]
      } else {
        subspace <- output[3]
      }

      # Get the number of features in each model

      s_num <- sapply(1:B1, function(i){length(subspace[[i]])})

      s <- rep(0, p)
      for (i in 1:length(subspace)) {
        s[subspace[[i]]] <- s[subspace[[i]]] + 1
      }

      dist <- s/B1
      dist[dist < C0/log(p)] <- C0/p

      max_size <- sum(dist != 0)
      Dmin <- round(max(1,quantile(s_num,.25)-1.5*IQR(s_num)))
      D <- round(min(quantile(s_num,.75) + 1.5*IQR(s_num),
                     max_size))
    }

    if (is.matrix(output)) {
      ytrain.pred <- data.frame(matrix(unlist(output[, 2]), ncol = B1))
      fit.list <- output[, 1]
    } else {
      ytrain.pred <- data.frame(matrix(unlist(output[2]), ncol = B1))
      fit.list <- output[1]
    }

    y_count <- matrix(nrow = n,ncol = nmulti)
    for(i in 1:n){
      for(j in 1:nmulti){
        y_count[i,j] = sum(ytrain.pred[i,] == j)/B1
      }
    }

    # cv
    if(nmulti <= 5){
      cb_list <- lapply(1:nmulti,function(i){seq(0,0.9,0.1)})
      cb <- Expand(cb_list)
    }else{
      n_thre <- round(10^5/nmulti)
      cb <- matrix(runif(n_thre*nmulti),n_thre,nmulti)
    }

    cb <- as.matrix(cb)
    error <- foreach(i = 1:dim(cb)[1],.combine = "rbind",.packages = "MASS") %dopar%{
      alpha = cb[i,]
      pre.ind <- sapply(1:n,function(k){
        as.numeric(which.max(y_count[k,]+alpha))})
      mis <- sum(ytrain!=pre.ind)/n
      return(list(alpha = alpha,pre.ind = pre.ind,mis = mis))
    }

    error <- as.matrix(error)
    indc <-  which.min(as.numeric(error[,3]))
    cutoff <- error[[indc,1]]
    pre <- error[[indc,2]]

  }
  }
  else{ #super RaSE
    if(!is.null(D)){
      if(length(D) == 1){
      D <- rep(D, length(base))
    }
      else if(length(D) != length(base)){
      stop("Length of D is not equal to the length of base!")
        }
      }

    dist <- matrix(1, nrow = length(base), ncol = p)
    rownames(dist) <- base
    is.null.D <- is.null(D)

    if (is.null.D) {
      D <- rep(floor(min(sqrt(n), p)), length(base))
    }
    names(D) <- base

    if ("lda" %in% names(base.dist)) {
      # clean data
      delete.ind.lda <- remove_ind(xtrain, ytrain)
      sig.ind.lda <- setdiff(1:p, delete.ind.lda)

      # estimate parameters

      D[base == "lda"] <- floor(min(sqrt(n), length(sig.ind.lda)))


      Sigma.mle.lda <- 0
      for(i in 1:nmulti){
        Sigma.mle.lda <- Sigma.mle.lda + (n_start[i] - 1)*cov(xtrain[ytrain == i ,, drop = F])/n
      }

      mu.mle.lda <- matrix(rep(0,p*nmulti),nmulti,p)
      for(i in 1:nmulti){
        mu.mle.lda[i,] <- colMeans(xtrain[ytrain == i,,drop = F])
      }

      dist["lda", ] <- rep(1, p)
      dist["lda", delete.ind.lda] <- 0
    }

    if ("qda" %in% names(base.dist)) {
      # clean data
      delete.ind.qda <- remove_ind(xtrain, ytrain)
      sig.ind.qda <- setdiff(1:p, delete.ind.qda)

      # estimate parameters

      D[base == "qda"] <- floor(min(sqrt(n), length(sig.ind.qda)))

      Sigma.mle.qda <- lapply(1:nmulti,function(i){
        (n_start[i] - 1)*cov(xtrain[ytrain == i ,, drop = F])/n_start[i]
      })

      mu.mle.qda <- matrix(rep(0,p*nmulti),nmulti,p)
      for(i in 1:nmulti){
        mu.mle.qda[i,] <- colMeans(xtrain[ytrain == i,,drop = F])
      }

      dist["qda",] <- rep(1, p)
      dist["qda", delete.ind.qda] <- 0
    }


    #start loops
    Dmin <- 1
    Dmax <- max(D)
    for (t in 1:(iteration + 1)) {
      output <- foreach(i = 1:B1, .combine = "rbind",
                        .packages = c("caret","MASS")) %dopar% {
        cat("B1 =",i," times starts","\n")
        set.seed(i)
        base.list <- sample(base, size = B2, prob = base.dist, replace = TRUE)
        S <- sapply(1:B2, function(j) {
          S.size <- sample(Dmin:D[base.list[i]], 1)
          snew <- sample(1:p, size = S.size, prob = dist[base.list[j], ])

          if (base.list[j] == "lda") {
            flag <- TRUE
            while (flag) {
              if (length(snew) > 2) {
                ind0 <- findLinearCombos(Sigma.mle.lda[snew, snew, drop = F])$remove
                if (!is.null(ind0)) {
                  snew <- snew[-ind0]
                }
              }
              snew1 <- c(snew, rep(NA, Dmax - length(snew)))
              if (sum(apply(mu.mle.lda,1,var)) > 1e-10){
                flag <- FALSE
              }
            }
            snew1

            } else if (base.list[j] == "qda") {
            flag <- TRUE
            while (flag) {
            if (length(snew) > 2) {
              ind0 <- lapply(1:nmulti,function(i){
                findLinearCombos(Sigma.mle.qda[[i]][snew, snew, drop = F])$remove})
              ind0 <- unique(unlist(ind0))
              if (!is.null(ind0)) {
                snew <- snew[-ind0]
              }
            }
            snew1 <- c(snew, rep(NA, Dmax - length(snew)))
            if (sum(apply(mu.mle.qda,1,var)) > 1e-10) {
              flag <- FALSE
            }
          }
          snew1

          } else {
            c(snew, rep(NA, Dmax - length(snew)))
          }
          })

        SRaSubset(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval,
                  B2 = B2, S = S, base = base, base.list = base.list,k = k,
                  criterion = criterion, cv = cv, nmulti = nmulti,D = D)
      }

      if (is.matrix(output)) {
        subspace <- output[, 3]
        base.list <- unlist(output[, 4])
      } else {
        subspace <- output[3]
        base.list <- unlist(output[4])
      }

      # Get the number of features in each model
      s_num <- sapply(1:B1, function(i){length(subspace[[i]])})
      D_seq <- sapply(1:length(base),
                      function(j){
                        s_q <- s_num[base.list == base[j]]
                        round(min(quantile(s_q,.75) + 1.5*IQR(s_q),
                                  sum(dist[j, ] != 0)))
                      }
      )



      s <- matrix(rep(0, p*length(base)), ncol = p)
      colnames(s) <- 1:p
      rownames(s) <- base
      for (i in 1:length(subspace)) {
        s[base.list[i], subspace[[i]]] <- s[base.list[[i]], subspace[[i]]] + 1
      }
      base.count <- sapply(1:length(base), function(i){
        sum(Reduce("c", base.list) == base[i])
      })

      if (super$base.update) { # update the base classifier distribution
        base.dist[1:length(base.dist)] <- base.count/B1
        base.dist[which(base.dist == 0)] <- C0*B1/2
      }


      dist <- s/base.count
      dist[dist < C0/log(p)] <- C0/p
      if (any(base.count == 0)) {
        dist[base.count == 0, ] <- 1/p
        warning("Some base classifiers have zero selecting frequency, and the feature sampling distribution cannot be calculated. Use uniform distribution instead in the next interation round.")
      }
      if ("lda" %in% base) {
        dist["lda", delete.ind.lda] <- 0
      } else if ("qda" %in% base) {
        dist["qda", delete.ind.qda] <- 0
      }


      D <- rep(round(mean(D_seq,na.rm = TRUE)), length(base))
      names(D) <- base
      Dmin <- 1
      Dmax <- max(D)

    }

    if (is.matrix(output)) {
      ytrain.pred <- data.frame(matrix(unlist(output[, 2]), ncol = B1))
      fit.list <- output[, 1]
    } else {
      ytrain.pred <- data.frame(matrix(unlist(output[2]), ncol = B1))
      fit.list <- output[1]
    }

    y_count <- matrix(nrow = n,ncol = nmulti)
    for(i in 1:n){
      for(j in 1:nmulti){
        y_count[i,j] = sum(ytrain.pred[i,] == j)/B1
      }
    }

  # cv
  if(nmulti <= 5){
    cb_list <- lapply(1:nmulti,function(i){seq(0,0.9,0.1)})
    cb <- Expand(cb_list)
  }else{
    n_thre <- round(10^5/nmulti)
    cb <- matrix(runif(n_thre*nmulti),n_thre,nmulti)
  }

  cb <- as.matrix(cb)
  error <- foreach(i = 1:dim(cb)[1],.combine = "rbind",.packages = "MASS") %dopar%{
    alpha = cb[i,]
    new_count <- y_count + matrix(alpha, nrow = n, ncol = ncol(cb), byrow = TRUE)
    pre.ind <- apply(new_count, 1, which.max)
    #  pre.ind <- sapply(1:n,function(k){as.numeric(which.max(y_count[k,]+alpha))})
    mis <- sum(ytrain!=pre.ind)/n
    return(list(alpha = alpha,pre.ind = pre.ind,mis = mis))
  }

  error <- as.matrix(error)
  indc <-  which.min(as.numeric(error[,3]))
  cutoff <- error[[indc,1]]
  pre <- error[[indc,2]]




  }

  # output
  # -------------------------------
  if (length(base) == 1) { # original RaSE
  if (ranking == TRUE) {
    rk <- s/B1
  } else {
    rk <- NULL
  }

  if (scale == TRUE) {
    scale.parameters <- list(center = scale.center, scale = scale.scale)
  } else {
    scale.parameters <- NULL
  }

  stopImplicitCluster()
  obj <- list(marginal = pre, base = base,
              criterion = criterion,
              B1 = B1, B2 = B2, D = D,
              nmulti = nmulti,table = lab_table,
              iteration = iteration, fit.list = fit.list, cutoff = cutoff, subspace = subspace, ranking = rk, scale = scale.parameters)
  class(obj) <- "multiRaSE"
  }
  else  { # super RaSE
    if (ranking == TRUE) {
      rk.feature <- s/base.count
    } else {
      rk.feature <- NULL
    }

    if (ranking == TRUE) {
      rk.base <- base.count/B1
      names(rk.base) <- base
    } else {
      rk.base <- NULL
    }

    if (scale == TRUE) {
      scale.parameters <- list(center = scale.center, scale = scale.scale)
    } else {
      scale.parameters <- NULL
    }

    stopImplicitCluster()

    obj <- list(marginal = pre,
                base = base,
                base.list = base.list,
                criterion = criterion, B1 = B1, B2 = B2,
                D = D,nmulti = nmulti,table = lab_table,
                iteration = iteration, fit.list = fit.list, cutoff = cutoff, subspace = subspace, ranking.feature = rk.feature, ranking.base = rk.base, scale = scale.parameters)
    class(obj) <- "SmultiRaSE"
  }

  return(obj)
}

