RaSE <- function(xtrain, ytrain, xval = NULL, yval = NULL,
                 B1 = 200, B2 = 500, D = NULL, dist = NULL, 
                 base = NULL, super = list(type = c("separate"), base.update = TRUE), 
                 criterion = NULL, ranking = TRUE,
                 k = c(3, 5, 7, 9, 11), cores = 1,
                 seed = NULL, iteration = 0, cutoff = TRUE, cv = 5, 
                 scale = TRUE, C0 = 0.1, kl.k = NULL, lower.limits = NULL, 
                 upper.limits = NULL, weights = NULL, ...) {

  
  nmulti <- length(unique(ytrain))
  
  if(nmulti == 2){
    
    if (!is.null(seed)) {
      set.seed(seed, kind = "L'Ecuyer-CMRG")
    }
    
    if (is.null(base)) {
      base <- "lda"
    }
    
    if (!all(base %in% c("lda", "qda", "knn", "logistic", "tree", "svm", "randomforest", "gamma"))) {
      stop("'base' can only be chosen from 'lda', 'qda', 'knn', 'logistic', 'tree', 'svm', 'randomforest' and 'gamma'!")
    }
    
    xtrain <- as.matrix(xtrain)
    base.dist <- NULL
    super.flag <- FALSE
    
    if (length(base) > 1) { # super RaSE
      super.flag <- TRUE
      if (is.character(base)) { # e.g. base = c("lda","knn","tree")
        if (!all(base %in% c("lda", "qda", "knn", "logistic", "tree", "svm", "randomforest", "gamma"))) {
          stop("'base' can only be chosen from 'lda', 'qda', 'knn', 'logistic', 'tree', 'svm', 'randomforest' and 'gamma'!")}
        base.dist <- rep(1/length(base), length(base))
        names(base.dist) <- base
      } else { # e.g. base = c(lda = 0.3, knn = 0.3, tree = 0.4)
        base.dist <- base
        base <- names(base)
      }
    }
    
    p <- ncol(xtrain)
    n <- length(ytrain)
    
    if(is.null(criterion) & length(base) > 1){criterion <- "cv"}
    
    if (is.null(criterion)) {
      if (base == "lda" || base == "qda" || base == "gamma") {
        criterion <- "ric"
      } else if (base == "logistic") {
        criterion <- "ebic"
        gam <- 0
      } else if (base == "knn") {
        criterion <- "loo"
      } else {
        criterion <- "training"
      }
    }
    
    n0 <- sum(ytrain == 0)
    n1 <- sum(ytrain == 1)
    
    if(is.null(kl.k)) {
      kl.k <- floor(c(sqrt(n0), sqrt(n1)))
    }
    
    if (scale == TRUE) {
      L <- scale_RaSE(xtrain)
      xtrain <- L$data
      scale.center <- L$center
      scale.scale <- L$scale
    }
    
    registerDoParallel(cores)
    
    # RaSE & SRaSE
    if(!super.flag){
      
      if (all(is.null(lower.limits)) && all(is.null(upper.limits)) && base == "logistic" || criterion == "nric") {
        use.glmnet <- FALSE
      } else {
        use.glmnet <- TRUE
      }
      
      # determine D
      if(base == 'lda'| base == 'qda'){
        a <- ifelse(base == 'lda',
                    suppressWarnings(cor(xtrain)),
                    suppressWarnings(cor(xtrain[ytrain == 0, ])))
        b <- a - diag(diag(a))
        b0 <- which(abs(b) > 0.9999, arr.ind = T)
        b0 <- matrix(b0[b0[, 1] > b0[, 2], ], ncol = 2)
        
        if(base == 'lda'){ #lda
          a <- diag(cov(xtrain))
          
          delete.ind <- unique(c(which(a == 0), b0[, 1]))
          sig.ind <- setdiff(1:p, delete.ind)
          
          if (is.null(D)) {
            D <- floor(min(sqrt(n), length(sig.ind)))
          }
          Sigma.mle <- ((n0 - 1) * cov(xtrain[ytrain == 0, , drop = F]) + 
                          (n1 - 1) * cov(xtrain[ytrain == 1, , drop = F]))/n
          mu0.mle <- colMeans(xtrain[ytrain == 0, , drop = F])
          mu1.mle <- colMeans(xtrain[ytrain == 1, , drop = F])
        }
        else{ # qda
          a0 <- diag(cov(xtrain[ytrain == 0, ]))
          
          a <- suppressWarnings(cor(xtrain[ytrain == 1, ]))
          b <- a - diag(diag(a))
          
          b1 <- which(abs(b) > 0.9999, arr.ind = T)
          b1 <- matrix(b1[b1[, 1] > b1[, 2], ], ncol = 2)
          a1 <- diag(cov(xtrain[ytrain == 1, ]))
          
          delete.ind <- unique(c(b0[, 1], b1[, 1], which(a0 == 0), which(a1 == 0)))
          sig.ind <- setdiff(1:p, delete.ind)
          
          if (is.null(D)) {
            D <- floor(min(sqrt(n0), sqrt(n1), length(sig.ind)))
          }
          Sigma0.mle <- (n0 - 1)/n0 * cov(xtrain[ytrain == 0, , drop = F])
          Sigma1.mle <- (n1 - 1)/n1 * cov(xtrain[ytrain == 1, , drop = F])
          mu0.mle <- colMeans(xtrain[ytrain == 0, , drop = F])
          mu1.mle <- colMeans(xtrain[ytrain == 1, , drop = F])
        }
      }
      else{
        if (is.null(D)) {
          D <- floor(min(sqrt(n), p))
        }
      }
      
      if(base == 'gamma'){
        lfun <- function(t, v) {
          -sum(dgamma(v, shape = t[1], scale = t[2], log = TRUE))
        }
        
        t0.mle <- t(sapply(1:p, function(i) {
          ai <- mean(xtrain[ytrain == 0, i])^2/var(xtrain[ytrain == 0, i])
          bi <- var(xtrain[ytrain == 0, i])/mean(xtrain[ytrain == 0, i])
          suppressWarnings(nlm(lfun, p = c(ai, bi), v = xtrain[ytrain == 0, i])$estimate)
        }))
        
        t1.mle <- t(sapply(1:p, function(i) {
          ai <- mean(xtrain[ytrain == 1, i])^2/var(xtrain[ytrain == 1, i])
          bi <- var(xtrain[ytrain == 1, i])/mean(xtrain[ytrain == 1, i])
          suppressWarnings(nlm(lfun, p = c(ai, bi), v = xtrain[ytrain == 1, i])$estimate)
        }))
      }
      
      # start loops
      dist <- rep(1, p)
      dist[delete.ind] <- 0
      
      for (t in 1:(iteration + 1)) {
        output <- foreach(i = 1:B1, .combine = "rbind", .packages = c("MASS",
                                                                      "caret",
                                                                      "e1071",
                                                                      "rpart",
                                                                      "nnet")) %dopar% {
          if(base == 'lda' | base == 'qda'){
          S <- sapply(1:B2, function(j) {
            S.size <- sample(1:D, 1)
            c(sample(1:p, size = min(S.size, length(dist[dist != 0])), prob = dist), 
              rep(NA, D - min(S.size, length(dist[dist != 0]))))
          })}
          if(base == 'lda'){
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
                if (any(abs(mu1.mle[snew1] - mu0.mle[snew1]) > 1e-10)) {
                  flag <- FALSE
                }
              }
              snew1
            })
          }
          else if(base == 'qda'){
            S <- sapply(1:B2, function(j) {
              snew <- S[!is.na(S[, j]), j]
              if (length(snew) > 2) {
                ind0 <- findLinearCombos(Sigma0.mle[snew, snew, drop = F])$remove
                ind1 <- findLinearCombos(Sigma1.mle[snew, snew, drop = F])$remove
                if (!all(is.null(c(ind0, ind1)))) {
                  snew <- snew[-c(ind0, ind1)]
                }
              }
              c(snew, rep(NA, D - length(snew)))
            })
          } 
          else if(base == 'gamma'){
            S <- sapply(1:B2, function(j) {
              S.size <- sample(1:D, 1)
              c(sample(1:p, size = min(S.size, length(dist[dist != 0])), prob = dist), rep(NA, D - min(S.size, length(dist[dist !=
                                                                                                                             0]))))
            })
          }
          else{
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
          }
          
          
          RaSubset(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base, k = k,
                   criterion = criterion, cv = cv, mu0.mle = mu0.mle, mu1.mle = mu1.mle, Sigma.mle = Sigma.mle, kl.k = kl.k, ...)
        }
        
        if (is.matrix(output)) {
          subspace <- output[, 3]
        } else {
          subspace <- output[3]
        }
        
        s <- rep(0, p)
        for (i in 1:length(subspace)) {
          s[subspace[[i]]] <- s[subspace[[i]]] + 1
        }
        
        dist <- s/B1
        if(t != (iteration + 1)){
          # C0/p
        dist[dist < C0/log(p)] <- 1/B1
        if(base == 'lda' | base == 'qda'){dist[delete.ind] <- 0} }
        
      }
      
      if (is.matrix(output)) {
        ytrain.pred <- data.frame(matrix(unlist(output[, 2]), ncol = B1))
        fit.list <- output[, 1]
      }
      else {
        ytrain.pred <- data.frame(matrix(unlist(output[2]), ncol = B1))
        fit.list <- output[1]
      }
      
    }
    else{ # super RaSE
      dist <- matrix(1, nrow = length(base), ncol = p)
      rownames(dist) <- base
      is.null.D <- is.null(D)
      is.na.D <- is.na(D)
      if (is.null.D) {
        D <- rep(floor(min(sqrt(n), p)), length(base))
        names(D) <- base
      }
      
      if ("lda" %in% names(base.dist)) {
        # clean data
        a <- suppressWarnings(cor(xtrain))
        b <- a - diag(diag(a))
        b0 <- which(abs(b) > 0.9999, arr.ind = T)
        b0 <- matrix(b0[b0[, 1] > b0[, 2], ], ncol = 2)
        a <- diag(cov(xtrain))
        
        delete.ind.lda <- unique(c(which(a == 0), b0[, 1]))
        sig.ind <- setdiff(1:p, delete.ind.lda)
        
        # estimate parameters
        if (is.null.D || is.na.D["lda"]) {
          D["lda"] <- floor(min(sqrt(n), length(sig.ind)))
        }
        Sigma.mle <- ((n0 - 1) * cov(xtrain[ytrain == 0, , drop = F]) + (n1 - 1) * cov(xtrain[ytrain == 1, , drop = F]))/n
        mu0.mle <- colMeans(xtrain[ytrain == 0, , drop = F])
        mu1.mle <- colMeans(xtrain[ytrain == 1, , drop = F])
        
        # start loops
        dist["lda", ] <- rep(1, p)
        dist["lda", delete.ind.lda] <- 0
      }
      
      if ("qda" %in% names(base.dist)) {
        # clean data
        a <- suppressWarnings(cor(xtrain[ytrain == 0, ]))
        b <- a - diag(diag(a))
        
        b0 <- which(abs(b) > 0.9999, arr.ind = T)
        b0 <- matrix(b0[b0[, 1] > b0[, 2], ], ncol = 2)
        a0 <- diag(cov(xtrain[ytrain == 0, ]))
        
        a <- suppressWarnings(cor(xtrain[ytrain == 1, ]))
        b <- a - diag(diag(a))
        
        b1 <- which(abs(b) > 0.9999, arr.ind = T)
        b1 <- matrix(b1[b1[, 1] > b1[, 2], ], ncol = 2)
        a1 <- diag(cov(xtrain[ytrain == 1, ]))
        
        delete.ind.qda <- unique(c(b0[, 1], b1[, 1], which(a0 == 0), which(a1 == 0)))
        sig.ind <- setdiff(1:p, delete.ind.qda)
        
        # estimate parameters
        if (is.null.D || is.na.D["qda"]) {
          D["qda"] <- floor(min(sqrt(n0), sqrt(n1), length(sig.ind)))
        }
        
        Sigma0.mle <- (n0 - 1)/n0 * cov(xtrain[ytrain == 0, , drop = F])
        Sigma1.mle <- (n1 - 1)/n1 * cov(xtrain[ytrain == 1, , drop = F])
        mu0.mle <- colMeans(xtrain[ytrain == 0, , drop = F])
        mu1.mle <- colMeans(xtrain[ytrain == 1, , drop = F])
        
        # start loops
        dist["qda",] <- rep(1, p)
        dist["qda", delete.ind.qda] <- 0
      }
      
      for (t in 1:(iteration + 1)) {
        output <- foreach(i = 1:B1, .combine = "rbind", .packages = c("MASS",
                                                                      "caret",
                                                                      "e1071",
                                                                      "rpart",
                                                                      "nnet")) %dopar% {
          base.list <- sample(base, size = B2, prob = base.dist, replace = TRUE)
          S <- sapply(1:B2, function(j) {
            S.size <- sample(1:D[base.list[j]], 1)
            snew <- sample(1:p, size = min(S.size, sum(dist[base.list[j], ] != 0)), prob = dist[base.list[j], ])
            if (base.list[j] == "lda") {
              flag <- TRUE
              while (flag) {
                if (length(snew) > 2) {
                  ind0 <- findLinearCombos(Sigma.mle[snew, snew, drop = F])$remove
                  if (!is.null(ind0)) {
                    snew <- snew[-ind0]
                  }
                }
                snew1 <- c(snew, rep(NA, max(D) - length(snew)))
                if (any(abs(mu1.mle[snew1] - mu0.mle[snew1]) > 1e-10)) {
                  flag <- FALSE
                }
              }
              snew1
            } else if (base.list[j] == "qda") {
              if (length(snew) > 2) {
                ind0 <- findLinearCombos(Sigma0.mle[snew, snew, drop = F])$remove
                ind1 <- findLinearCombos(Sigma1.mle[snew, snew, drop = F])$remove
                if (!all(is.null(c(ind0, ind1)))) {
                  snew <- snew[-c(ind0, ind1)]
                }
              }
              c(snew, rep(NA, max(D) - length(snew)))
            } else {
              c(snew, rep(NA, max(D) - length(snew)))
            }
          })
          
          RaSubset(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base.list, k = k,
                   criterion = criterion, cv = cv, mu0.mle = mu0.mle, mu1.mle = mu1.mle, Sigma0.mle = Sigma0.mle,
                   Sigma1.mle = Sigma1.mle,  Sigma.mle = Sigma.mle, kl.k = kl.k, gam = gam, ...)
        }
        
        if (is.matrix(output)) {
          subspace <- output[, 3]
          base.list <- output[, 4]
        } else {
          subspace <- output[3]
          base.list <- output[, 4]
        }
        
        
        s <- matrix(rep(0, p*length(base)), ncol = p)
        colnames(s) <- 1:p
        rownames(s) <- base
        for (i in 1:length(subspace)) {
          s[base.list[[i]], subspace[[i]]] <- s[base.list[[i]], subspace[[i]]] + 1
        }
        base.count <- sapply(1:length(base), function(i){
          sum(Reduce("c", base.list) == base[i])
        })
        
        if (super$base.update) { # update the base classifier distribution
          base.dist <- base.count/B1
        }
        
        dist <- s/base.count
        if(t != iteration + 1){
        dist[dist < C0/log(p)] <- 1/B1
        if (any(base.count == 0) && (!super$base.update) && t != (iteration + 1)) {
          dist[base.count == 0, ] <- 1/p
          warning("Some base classifiers have zero selecting frequency, and the feature sampling distribution cannot be calculated. Use uniform distribution instead in the next interation round.")
        }
        if ("lda" %in% base) {
          dist["lda", delete.ind.lda] <- 0
        } else if ("qda" %in% base) {
          dist["qda", delete.ind.qda] <- 0
        }
        }
      }
      
      if (is.matrix(output)) {
        ytrain.pred <- data.frame(matrix(unlist(output[, 2]), ncol = B1))
        fit.list <- output[, 1]
      }
      else {
        ytrain.pred <- data.frame(matrix(unlist(output[2]), ncol = B1))
        fit.list <- output[1]
      }
    }
    
    # output for RaSE & SRaSE
    if (!super.flag) {
      p0 <- sum(ytrain == 0)/nrow(xtrain)
      if (cutoff == TRUE) {
        cutoff <- RaCutoff(ytrain.pred, ytrain, p0)
      } else {
        cutoff <- 0.5
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
      obj <- list(marginal = c(`class 0` = p0, `class 1` = 1 - p0), base = base, criterion = criterion, B1 = B1, B2 = B2, D = D,
                  iteration = iteration, fit.list = fit.list, cutoff = cutoff, subspace = subspace, ranking = rk, scale = scale.parameters)
      class(obj) <- "RaSE"
    } 
    else { # super RaSE
      p0 <- sum(ytrain == 0)/nrow(xtrain)
      if (cutoff == TRUE) {
        cutoff <- RaCutoff(ytrain.pred, ytrain, p0)
      } else {
        cutoff <- 0.5
      }
      
      if (ranking == TRUE) {
        rk.feature <- s/ifelse(base.count > 0,base.count,1)*100
        
        rk.base <- base.count/B1*100
        names(rk.base) <- base
      } else {
        rk.feature <- NULL
        rk.base <- NULL
      }
      
      if (scale == TRUE) {
        scale.parameters <- list(center = scale.center, scale = scale.scale)
      } else {
        scale.parameters <- NULL
      }
      
      stopImplicitCluster()
      obj <- list(marginal = c(`class 0` = p0, `class 1` = 1 - p0), base = Reduce("c", base.list), criterion = criterion, B1 = B1, B2 = B2, D = D,
                  iteration = iteration, fit.list = fit.list, cutoff = cutoff, subspace = subspace,ranking = list(ranking.feature = rk.feature,ranking.base = rk.base), scale = scale.parameters)
      class(obj) <- "SRaSE"
    }
  }
  else{
    
    if(I(!is.null(criterion) & !I(criterion %in% c("error rate","likelihood","training")))
       | is.null(criterion)) {criterion = "error rate"}
  
    if(is.null(base)){base = "lda"}
  
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
    if (length(base) > 1 & is.character(base)) {  #super mRaSE has base.dist
      base.dist <- rep(1/length(base), length(base))
      names(base.dist) <- base
    }
    else if (length(base) > 1 & !is.character(base)){
      base.dist <- base
      base <- names(base)
    }
    
    #single base doesn't has base.dist
    p <- ncol(xtrain)
    n <- length(ytrain)
    #nmulti <- length(unique(ytrain))
    
    # prior class
    n_start <- sapply(1:nmulti, function(i)
      sum(ytrain == i))
    
    
    if(is.null(kl.k)) {
      kl.k <- floor(sqrt(n_start))
    } ######### delete
    
    # scale the xtrain
    if (scale) {
      L <- scale_RaSE(xtrain)
      xtrain <- L$data
      scale.center <- L$center
      scale.scale <- L$scale
    }
    

    registerDoParallel(cores)
    
    if(length(base) == 1){ # mRaSE
      
      if(base == 'lda' | base == 'qda'){
        # clean data (delete variables with high collinearity and variables that have constant values)
        delete.ind <- remove_ind(xtrain, ytrain)
        sig.ind <- setdiff(1:p, delete.ind)
        
        Sigma.mle <- 0
        for(i in 1:nmulti){
          Sigma.mle <- Sigma.mle + (n_start[i] - 1)*cov(xtrain[ytrain == i ,, drop = F])/n
        }
        
        mu.mle <- matrix(rep(0,p*nmulti),nmulti,p)
        for(i in 1:nmulti){
          mu.mle[i,] <- colMeans(xtrain[ytrain == i,,drop = F])
        }
      }
      
      if (is.null(D)) {
        D <- floor(min(sqrt(n), p))
      }
      
      if (all(is.null(lower.limits)) && all(is.null(upper.limits)) && base == "logistic" || criterion == "nric") {
        use.glmnet <- FALSE
      } else {
        use.glmnet <- TRUE
      }

      # start loop
      dist <- rep(1, p)
      if (base == 'lda' | base == 'qda') {dist[delete.ind] <- 0}
      Dmin <- 1
      output <- NULL
      for (t in 1:(iteration + 1)) {
        output <- foreach(
          i = 1:B1,
          .combine = "rbind",
          .packages = c("MASS",
                        "caret",
                        "e1071",
                        "rpart",
                        "nnet")
        ) %dopar% {
          
          cat("B1 =",i," times starts","\n")
          
          if(base == 'lda' | base == 'qda'){
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
          }
          if(base == 'knn' | base == 'logistic'){
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
          }
          if(base == 'svm' | base == 'tree'){
            S <- sapply(1:B2, function(j) {
              S.size <- sample(Dmin:D, 1)
              if(base == "tree") S.size <- max(2,S.size)
              c(sample(1:p, size = min(S.size, length(dist[dist != 0])), prob = dist),
                rep(NA, D - min(S.size, length(dist[dist !=0]))))
            })
            cat(min(sapply(1:B2,function(d) sum(!is.na(S[,d])))),"\n")
          }
          
          SRaSubset(xtrain = xtrain, ytrain = ytrain, xval = xval, yval = yval, B2 = B2, S = S, base = base, k = k,
                    criterion = criterion, cv = cv, nmulti = nmulti, ...)
          
          
        }
        
        if (is.matrix(output)) {subspace <- output[, 3]}
        else {subspace <- output[3]}
        
        # Get the number of features in each model
        
        s_num <- sapply(1:B1, function(i){length(subspace[[i]])})
        
        # Get the frequency of each feature in B1 models served as the new dist
        
        s <- rep(0, p)
        for (i in 1:length(subspace)) {
          s[subspace[[i]]] <- s[subspace[[i]]] + 1
        }
        
        dist <- s/B1
        if(t != (iteration + 1)){

        dist[dist < C0/log(p)] <- 1/B1
        if(base == 'lda' | base == 'qda'){dist[delete.ind] <- 0} 
        
        max_size <- sum(dist != 0)
        Dmin <- round(max(1,quantile(s_num,.25)-1.5*IQR(s_num)))
        D <- round(min(quantile(s_num,.75) + 1.5*IQR(s_num),
                       max_size)) 
        cat(Dmin,D,"\n")}
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
      
      # alpha selection
      if(nmulti <= 5){
        cb_list <- lapply(1:nmulti,function(i){seq(0,0.9,0.1)})
        cb <- Expand(cb_list)
      }else{
        n_thre <- round(10^5/nmulti)
        cb <- matrix(runif(n_thre*nmulti),n_thre,nmulti)
      }
      
      cb <- as.matrix(cb)
      error <- foreach(i = 1:dim(cb)[1],.combine = "rbind",.packages = c("MASS",
                                                                         "caret",
                                                                         "e1071",
                                                                         "rpart",
                                                                         "nnet")) %dopar%{
        alpha = cb[i,]
        pre.ind <- sapply(1:n,function(kk){as.numeric(which.max(y_count[kk,]+alpha))})
        
        # pre.ind[,ytrain] : label as column index for this n*nmulti table
        
        if(criterion == "error rate"){
          mis <- sum(ytrain!=pre.ind)/n
        } else if(criterion == "likelihood"){
          
          temp_tab <- table(ytrain,pre.ind)
  
          mis <- sum(log(1/(diag(temp_tab)/rowSums(temp_tab))))
          
          #pvec <-  sapply(1:n,function(index) y_count[index,ytrain[index]] + alpha[ytrain[index]])
          #pvec[which(pvec == 0)] <- 1e-3
          
          #mis <- sum(log(1/pvec))
          }
          
        return(list(alpha = alpha,pre.ind = pre.ind,mis = mis))
      }
      
      error <- as.matrix(error)
      indc <-  which.min(as.numeric(error[,3]))
      cutoff <- error[[indc,1]]
      pre <- error[[indc,2]]
      
    }
    else{ #super mRaSE
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
      
      Dmin <- 1
      Dmax <- max(D)
      for (t in 1:(iteration + 1)) {
        output <- foreach(i = 1:B1, .combine = "rbind",
                          .packages = c("MASS",
                                        "caret",
                                        "e1071",
                                        "rpart",
                                        "nnet")) %dopar% {
                            cat("B1 =",i," times starts","\n")
                            #set.seed(i)
                            base.list <- sample(base, size = B2, prob = base.dist, replace = TRUE)
                            S <- sapply(1:B2, function(j) {
                              S.size <- sample(Dmin:D[base.list[j]], 1)
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
        
        if(t != (iteration + 1)){
        if (super$base.update) { # update the base classifier distribution
          base.dist[1:length(base.dist)] <- base.count/B1
          base.dist[which(base.dist == 0)] <- C0*B1/2
        } }
        
        
        dist <- s/base.count
        
        if(t != (iteration + 1)){
        dist[dist < C0/log(p)] <- 1/B1
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
        Dmax <- max(D) }
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
      
      # alpha selection
      if(nmulti <= 5){
        cb_list <- lapply(1:nmulti,function(i){seq(0,0.9,0.1)})
        cb <- Expand(cb_list)
      }else{
        n_thre <- round(10^5/nmulti)
        cb <- matrix(runif(n_thre*nmulti),n_thre,nmulti)
      }
      
      cb <- as.matrix(cb)
      error <- foreach(i = 1:dim(cb)[1],.combine = "rbind",.packages = c("MASS",
                                                                         "caret",
                                                                         "e1071",
                                                                         "rpart",
                                                                         "nnet")) %dopar%{
        alpha = cb[i,]
        new_count <- y_count + matrix(alpha, nrow = n, ncol = ncol(cb), byrow = TRUE)
        pre.ind <- apply(new_count, 1, which.max)
        
        # pvec <- sapply(1:n,function(index) new_count[index,ytrain[index]])
        # pvec[which(pvec == 0)] <- 1e-3 
        
        if(criterion == "error rate"){
          mis <- sum(ytrain!=pre.ind)/n
          } else if(criterion == "likelihood"){
            temp_tab <- table(ytrain,pre.ind)
            
            mis <- sum(log(1/(diag(temp_tab)/rowSums(temp_tab))))
            #mis <- sum(log(1/pvec))
            }
        
        return(list(alpha = alpha,pre.ind = pre.ind,mis = mis))
      }
      
      error <- as.matrix(error)
      indc <-  which.min(as.numeric(error[,3]))
      cutoff <- error[[indc,1]]
      pre <- error[[indc,2]]
    
    }
    
    # output
    # -------------------------------
    if (length(base) == 1) { # original mRaSE
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
      class(obj) <- "mRaSE"
    }
    else  { # super mRaSE
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
                  iteration = iteration, fit.list = fit.list, cutoff = cutoff, subspace = subspace, ranking = list(ranking.feature = rk.feature,ranking.base = rk.base), scale = scale.parameters)
      class(obj) <- "SmRaSE"
    }
  }
  return(obj)
}
    

    