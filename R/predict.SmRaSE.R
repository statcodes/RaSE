#' Predict the outcome of new observations based on the estimated Super mRaSE classifier (Bi, F., Zhu, J. and Feng, Y., 2022).
#'
#' @export
#' @param object object fitted \code{'SmRaSE'} object using \code{RaSE}.
#' @param newx a set of new observations. Each row of \code{newx} is a new observation.
#' @param ... additional arguments.
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
#'\dontrun{
#' fit <- SmultiRaSE(xtrain, ytrain, B1 = 20, B2 = 50, iteration = 0,
#'base = c('lda','knn'), cores = 1)
#' ypred <- predict(fit, xtest)
#' mean(ypred != ytest)}


predict.SmRaSE <- function(object, newx, ...) {

  alpha = as.numeric(object$cutoff)
  if (!is.null(object$scale)) {
    newx <- scale(newx, center = object$scale$center, scale = object$scale$scale)
  }

  ytest.pred <- sapply(1:object$B1, function(i) {
  if (object$base.list[i] == "lda") {
      rs <- as.numeric(predict(object$fit.list[[i]], newx[, object$subspace[[i]], drop = F])$class)
      }


  if (object$base.list[i] == "knn"){
      rs <- as.numeric(predict(object$fit.list[[i]], newx[, object$subspace[[i]], drop = F], type = "class"))
    }


  if (object$base.list[i] == "svm"){
      rs <- predict(object$fit.list[[i]], newx[, object$subspace[[i]], drop = F], type = "class")
    }

  if (object$base.list[i] == "tree") {
      rs <- as.numeric(predict(object$fit.list[[i]], data.frame(x = newx[, object$subspace[[i]], drop = F]),type = "class"))
    }



    if (object$base.list[i] == "logistic") {
          rs <- as.numeric(predict(object$fit.list[[i]],data.frame(x = newx[, object$subspace[[i]], drop = F])))
    }

   rs
})

  nmulti <- object$nmulti

  # final output
    if (nrow(newx) == 1) {
      vote <- sapply(1:nmulti,function(x){
        sum(ytest.pred == x)/object$B1
      })
    }
    if (nrow(newx) > 1) {
      vote <- apply(ytest.pred,1,function(x){
        sapply(1:nmulti,function(y){
          sum(x == y)/object$B1
        })})
    }
    Class <- numeric()
    for(i in 1:nrow(newx)){
      Class[i] <- as.numeric(which.max(vote[,i] + alpha))
    }
    Class <- object$table[Class,1]
    return(Class)


}
