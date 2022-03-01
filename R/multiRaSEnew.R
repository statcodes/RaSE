 #' Construct the random subspace ensemble classifier.
#'
#' \code{RaSE} is a general ensemble classification framework to solve the sparse classification problem. In RaSE algorithm, for each of the B1 weak learners, B2 random subspaces are generated and the optimal one is chosen to train the model on the basis of some criterion.
#' @export
#' @param xtrain a
#' @param ytrain a
#' @param xval a
#' @param yval a
#' @param B1 a
#' @param B2 a
#' @param D a
#' @param dist a
#' @param base a
#' @param super a
#' @param criterion a
#' @param ranking a
#' @param k a
#' @param cores a
#' @param seed a
#' @param iteration a
#' @param cutoff a
#' @param cv a
#' @param scale a
#' @param C0 a
#' @param kl.k a
#' @param lower.limits a
#' @param upper.limits a
#' @param weights a
#' @param ... a
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
#' @importFrom stats quantile
#' @importFrom stats IQR


multiRasenew <- function(xtrain, ytrain,
                      xval = NULL, yval = NULL,
                      B1 = 50, B2 = 100,
                      D = NULL, dist = NULL,
                      base = "lda",
                      super = list(type = "seperate", base.update = TRUE),
                      criterion = cv,
                      ranking = TRUE,
                      k = c(3, 5, 7, 9, 11),
                      cores = 1,
                      seed = NULL,
                      iteration = 0,
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

  #label from 1 to K
  trans_inf <- labeltrans(ytrain)
  ytrain <- trans_inf$vnew
  lab_table <- trans_inf$label

  if (!is.null(seed)) {
    set.seed(seed, kind = "L'Ecuyer-CMRG")
  }


  xtrain <- as.matrix(xtrain)
  base.dist <- NULL

  if (length(base) > 1) {  #super RaSE has base.dist
    if (is.character(base)) {
      if (!all(base %in%
               c("lda", "qda", "knn", "logistic",
                 "tree", "svm"))){
      stop("'base' can only be chosen from 'lda',
           'qda', 'knn', 'logistic', 'tree' and 'svm'!")
        }
      base.dist <- rep(1/length(base), length(base))
      names(base.dist) <- base
    } else {
      base.dist <- base
      base <- names(base)
    }
  }
   #single base doesn't has base.dist


  # features of input
  p <- ncol(xtrain)
  n <- length(ytrain)
  nmulti <- length(unique(ytrain))

  # prior class
  n_start <- rep(0,nmulti)
  for (i in 1:nmulti){
    n_start[i] <- sum(ytrain == i)
  }


  if(is.null(kl.k)) {
    kl.k <- floor(sqrt(n_start))
  } ######### meaning of kl.k?


  # scale the xtrain
  if (scale) {
    L <- scale_Rase(xtrain)
    xtrain <- L$data
    scale.center <- L$center
    scale.scale <- L$scale
  }

  # # #

  registerDoParallel(cores)

  # base classifier : lda

  if (is.null(base.dist) && base == "lda") {

    # clean data (delete variables with high collinearity and variables that have constant values)
    a <- suppressWarnings(cor(xtrain))
    b <- a - diag(diag(a)) # delete the elements with value 1
    b0 <- which(abs(b) > 0.9999, arr.ind = T)
    b0 <- b0[b0[, 1] > b0[, 2], ,drop = FALSE]
    a <- diag(cov(xtrain))


    delete.ind <- unique(c(which(a == 0), b0[, 1]))
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

        RaSubsetnew(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base, k = k,
                    criterion = criterion, cv = cv,...)

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


      Dmin <- round(max(1,quantile(s_num,.25)-1.5*IQR(s_num)))
      D <- round(quantile(s_num,.75) + 1.5*IQR(s_num))
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

  # base classifier : qda

  if (is.null(base.dist) && base == "qda") {

    # clean data (delete variables with high collinearity and variables that have constant values)
    a <- suppressWarnings(cor(xtrain))
    b <- a - diag(diag(a)) # delete the elements with value 1
    b0 <- which(abs(b) > 0.9999, arr.ind = T)
    b0 <- b0[b0[, 1] > b0[, 2], ,drop = FALSE]
    a <- diag(cov(xtrain))


    delete.ind <- unique(c(which(a == 0), b0[, 1]))
    sig.ind <- setdiff(1:p, delete.ind)

    # estimate parameters
    if (is.null(D)) {
      D <- floor(min(sqrt(n), length(sig.ind)))
    }

    Sigma.mle <- lapply(1:nmulti,function(i){
      (n_start[i] - 1)*cov(xtrain[ytrain == i ,, drop = F])/n_start[i]
    })

    mu.mle <- matrix(rep(0,p*nmulti),nmulti,p)
    for(i in 1:nmulti){
      mu.mle[i,] <- colMeans(xtrain[ytrain == i,,drop = F])
    }

    # start loops
    dist <- rep(1, p)
    dist[delete.ind] <- 0

    for (t in 1:(iteration + 1)) {
      output <- foreach(i = 1:B1, .combine = "rbind", .packages = "MASS") %dopar% {
        S <- sapply(1:B2, function(j) {

          S.size <- sample(1:D, 1)

          c(sample(1:p, size = S.size, prob = dist),
            rep(NA, D - min(S.size, length(dist[dist != 0]))))

        }) # matrix of selected features (nrow = D, ncol = B2)

        S <- sapply(1:B2, function(j) {
          flag <- TRUE
          while (flag) {
            snew <- S[!is.na(S[, j]), j]
            if (length(snew) > 2) {
              ind0 <- lapply(1:nmulti,function(i){
                findLinearCombos(Sigma.mle[[i]][snew, snew, drop = F])$remove})
              ind0 <- unique(unlist(ind0))
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

        RaSubsetnew(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base, k = k,
                    criterion = criterion, cv = cv,...)

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

      Dmin <- round(max(1,quantile(s_num,.25))-1.5*IQR(s_num))
      D <- round(quantile(s_num,.75) + 1.5*IQR(s_num))
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

  # base classifier : knn

  if (is.null(base.dist) && base == "knn"){

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
    for (t in 1:(iteration + 1)) {
      output <- foreach(i = 1:B1, .combine = "rbind", .packages = "MASS") %dopar% {

        S <- sapply(1:B2, function(j) {
          if (use.glmnet) {
            S.size <- sample(2:D, 1) # glmnet cannot fit the model with a single variable
            if (length(dist[dist != 0]) == 1) {
              stop ("Only one feature has positive sampling weights! 'glmnet' cannot be applied in this case! ")
            }
          } else {
            S.size <- sample(1:D, 1)
          }
          c(sample(1:p, size = min(S.size, length(dist[dist != 0])), prob = dist), rep(NA, D - min(S.size, length(dist[dist !=
                                                                                                                         0]))))
        })

        RaSubsetnew(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base, k = k,
                    criterion = criterion, cv = cv,...)

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

      Dmin <- round(max(1,quantile(s_num,.25)-1.5*IQR(s_num)))
      D <- round(quantile(s_num,.75) + 1.5*IQR(s_num))
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

  # base classifier : logistic

  if (is.null(base.dist) && base == "logistic"){

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

        S <- sapply(1:B2, function(j) {
          if (use.glmnet) {
            S.size <- sample(2:D, 1) # glmnet cannot fit the model with a single variable
            if (length(dist[dist != 0]) == 1) {
              stop ("Only one feature has positive sampling weights! 'glmnet' cannot be applied in this case! ")
            }
          } else {
            S.size <- sample(Dmin:D, 1)
          }
          c(sample(1:p, size = min(S.size, length(dist[dist != 0])), prob = dist), rep(NA, D - min(S.size, length(dist[dist != 0]))))
        })

        RaSubsetnew(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base, k = k,
                    criterion = criterion, cv = cv,...)

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

      Dmin <- round(max(1,quantile(s_num,.25)-1.5*IQR(s_num)))
      D <- round(quantile(s_num,.75) + 1.5*IQR(s_num))
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

  # base classifier : svm

  if (is.null(base.dist) && base == "svm"){

    if (is.null(D)) {
      D <- floor(min(sqrt(n), p))
    }

    dist <- rep(1, p)
    for (t in 1:(iteration + 1)) {
      output <- foreach(i = 1:B1, .combine = "rbind", .packages = "MASS") %dopar% {

        S <- sapply(1:B2, function(j) {
            S.size <- sample(1:D, 1)
            c(sample(1:p, size = min(S.size, length(dist[dist != 0])), prob = dist),
              rep(NA, D - min(S.size, length(dist[dist !=0]))))
        })

        RaSubsetnew(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base, k = k, kl.k = kl.k,
                    criterion = criterion, cv = cv,...)
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

    Dmin <- round(max(1,quantile(s_num,.25)-1.5*IQR(s_num)))
    D <- round(quantile(s_num,.75) + 1.5*IQR(s_num))
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

  # base classifier : tree

  if (is.null(base.dist) && base == "tree"){

    if (is.null(D)) {
      D <- floor(min(sqrt(n), p))
    }

    dist <- rep(1, p)
    for (t in 1:(iteration + 1)) {
      output <- foreach(i = 1:B1, .combine = "rbind", .packages = "MASS") %dopar% {

        S <- sapply(1:B2, function(j) {
          S.size <- sample(1:D, 1)
          c(sample(1:p, size = min(S.size, length(dist[dist != 0])), prob = dist),
            rep(NA, D - min(S.size, length(dist[dist !=0]))))
        })

        RaSubsetnew(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base, k = k, kl.k = kl.k,
                    criterion = criterion, cv = cv,...)
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

    Dmin <- round(max(1,quantile(s_num,.25)-1.5*IQR(s_num)))
    D <- round(quantile(s_num,.75) + 1.5*IQR(s_num))
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

  if (ranking == TRUE) {
    rk <- s/B1*100
    names(rk) <- 1:length(rk)
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

  return(obj)
}

