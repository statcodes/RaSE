#' Label Transformation.
#'
#' Label Transformation
#' @export
#' @param v a
#' @param start a
labeltrans <- function(v, start = 1){
  num <- length(unique(v))
  ori_lab <- unique(v)
  new_lab <- 1:num
  new_lab <- new_lab + start - 1
  label <- data.frame(pre = ori_lab,
                      post = new_lab)
  vnew <- match(v,ori_lab) + start - 1

  return(list(label = label,vnew = vnew))
}



