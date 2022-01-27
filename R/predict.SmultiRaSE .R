#' @export
predict.SmultiRaSE <- function(object, newx, type = c("vote", "prob", "raw-vote", "raw-prob"), ...) {
  type <- match.arg(type)

  alpha = as.numeric(object$cutoff)
  if (!is.null(object$scale)) {
    newx <- scale(newx, center = object$scale$center, scale = object$scale$scale)
  }

  ytest.pred <- sapply(1:object$B1, function(i) {
  if (object$base.list[i] == "lda" || object$base.list[i] == "qda") {
    if (type == "vote" || type == "raw-vote") {
      rs <- as.numeric(predict(object$fit.list[[i]], newx[, object$subspace[[i]], drop = F])$class)
      }else if (type == "prob" || type == "raw-prob") {
      rs <- as.matrix(predict(object$fit.list[[i]], newx[, object$subspace[[i]], drop = F])$posterior)
      }}


  if (object$base.list[i] == "knn"){
    if (type == "vote" || type == "raw-vote") {
      rs <- as.numeric(predict(object$fit.list[[i]], newx[, object$subspace[[i]], drop = F], type = "class"))
    } else if (type == "prob" || type == "raw-prob") {
      rs <- as.matrix(predict(object$fit.list[[i]], newx[, object$subspace[[i]], drop = F],type = "prob"))
      }}


  if (object$base.list[i] == "svm"){
      rs <- predict(object$fit.list[[i]], newx[, object$subspace[[i]], drop = F], type = "class")
    }

  if (object$base.list[i] == "tree") {
    if (type == "vote" || type == "raw-vote") {
      rs <- as.numeric(predict(object$fit.list[[i]], data.frame(x = newx[, object$subspace[[i]], drop = F]),type = "class"))
    } else if (type == "prob" || type == "raw-prob") {
      rs <- as.numeric(predict(object$fit.list[[i]], data.frame(x = newx[, object$subspace[[i]], drop = F]), type = "prob")[, 2])
      }}



    if (object$base.list[i] == "logistic") {
      if (type == "vote" || type == "raw-vote") {
          rs <- as.numeric(predict(object$fit.list[[i]],data.frame(x = newx[, object$subspace[[i]], drop = F])))
        }
       else if (type == "prob" || type == "raw-prob") {
         rs <-  predict(object$fit.list[[i]], data = data.frame(x = newx[, object$subspace[[i]], drop = F]),type = "prob")
        }
    }

   rs
})

  nmulti <- object$nmulti

  # final output
  if (type == "vote") {
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
  } else if (type == "prob") {
    if (nrow(newx) == 1) {
      vote <- sapply(1:nmulti,function(x){
        mean(sapply(1:object$B1,function(y){
          ytest.pred[[y]][x]
        }))
      })
    }
    if (nrow(newx) > 1) {
      vote <- sapply(1:nrow(newx),function(i){
        sapply(1:nmulti,function(x){
          mean(sapply(1:object$B1,function(y){
            ytest.pred[[y]][i,x]
          }))
        })
      })
    }
    vote <- as.data.frame(t(vote))
    names(vote) <- 1:nmulti
    return(vote)
  } else if (type == "raw-vote" || type == "raw-prob") {
    return(ytest.pred)
  }
}
