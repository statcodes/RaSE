#' Predict the outcome of new observations based on the estimated mRaSE classifier (Bi, F., Zhu, J. and Feng, Y., 2022).
#'
#' @export
#' @param object a
#' @param newx a
#' @param type a
#' @param ... a
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
#' fit <- SmultiRase(xtrain, ytrain, B1 = 20, B2 = 50, iteration = 0,
#'base = 'lda', cores = 1)
#' ypred <- predict(fit, xtest)
#' mean(ypred != ytest)


predict.mRaSE <- function(object, newx, type = c("vote", "prob", "raw-vote", "raw-prob"), ...) {
  type <- match.arg(type)

  alpha = as.numeric(object$cutoff)
  if (!is.null(object$scale)) {
    newx <- scale(newx, center = object$scale$center, scale = object$scale$scale)
  }

  if (object$base == "lda") {
    if (type == "vote" || type == "raw-vote") {
      ytest.pred <- sapply(1:object$B1, function(i) {
        as.numeric(predict(object$fit.list[[i]], newx[, object$subspace[[i]], drop = F])$class)
      })
    } else if (type == "prob" || type == "raw-prob") {
      ytest.pred <- lapply(1:object$B1, function(i) {
        as.matrix(predict(object$fit.list[[i]], newx[, object$subspace[[i]], drop = F])$posterior)
      })
    }
  }

  if (object$base == "knn"){
    if (type == "vote" || type == "raw-vote") {
      ytest.pred <- sapply(1:object$B1, function(i) {
        as.numeric(predict(object$fit.list[[i]], newx[, object$subspace[[i]], drop = F], type = "class"))       })
    } else if (type == "prob" || type == "raw-prob") {
      ytest.pred <- sapply(1:object$B1, function(i) {
        as.matrix(predict(object$fit.list[[i]], newx[, object$subspace[[i]], drop = F],type = "prob"))
      })
    }
  }

  if (object$base == "svm"){
      ytest.pred <- sapply(1:object$B1, function(i) {
        predict(object$fit.list[[i]], newx[, object$subspace[[i]], drop = F], type = "class")       })
    }

  if (object$base == "tree") {
    if (type == "vote" || type == "raw-vote") {
      ytest.pred <- sapply(1:object$B1, function(i) {
        as.numeric(predict(object$fit.list[[i]], data.frame(x = newx[, object$subspace[[i]], drop = F]),type = "class"))
      })
    } else if (type == "prob" || type == "raw-prob") {
      ytest.pred <- sapply(1:object$B1, function(i) {
        as.numeric(predict(object$fit.list[[i]], data.frame(x = newx[, object$subspace[[i]], drop = F]), type = "prob")[, 2])
      })
    }

  }



    if (object$base == "logistic") {
      if (type == "vote" || type == "raw-vote") {
          ytest.pred <- sapply(1:object$B1, function(i) {
            as.numeric(predict(object$fit.list[[i]],data.frame(x = newx[, object$subspace[[i]], drop = F])))
          })
        }
       else if (type == "prob" || type == "raw-prob") {
         ytest.pred <- sapply(1:object$B1, function(i) {
           predict(object$fit.list[[i]], data = data.frame(x = newx[, object$subspace[[i]], drop = F]),type = "prob")
         })
        }
      }


  # final output
  if (type == "vote") {
    if (nrow(newx) == 1) {
      vote <- sapply(1:object$nmulti,function(x){
        sum(ytest.pred == x)/object$B1
      })
    }
    if (nrow(newx) > 1) {
      vote <- apply(ytest.pred,1,function(x){
        sapply(1:object$nmulti,function(y){
          sum(x == y)/object$B1
        })})
    }
    Class <- numeric()
    for(i in 1:nrow(newx)){
      Class[i] <- as.numeric(which.max(vote[,i] + alpha))
    }
   Class <- object$table[Class,1]
    return(Class)
  } else if (type == "prob") {
    if (nrow(newx) == 1) {
      vote <- sapply(1:object$nmulti,function(x){
        mean(sapply(1:object$B1,function(y){
          ytest.pred[[y]][x]
        }))
      })
    }
    if (nrow(newx) > 1) {
      vote <- sapply(1:nrow(newx),function(i){
        sapply(1:object$nmulti,function(x){
          mean(sapply(1:object$B1,function(y){
            ytest.pred[[y]][i,x]
          }))
        })
      })
    }
    vote <- as.data.frame(t(vote))
    names(vote) <- object$label[1:object$nmulti,1]
    return(vote)
  } else if (type == "raw-vote" || type == "raw-prob") {
    return(ytest.pred)
  }
}
