#' Print a fitted mRaSE object.
#'
#' Similar to the usual print methods, this function summarizes results.
#' from a fitted \code{'mRaSE'} object.
#' @export
#' @param x fitted \code{'mRaSE'} model object.
#' @param ... additional arguments.
#' @return No value is returned.
#' @seealso \code{\link{RaSE}}.
#' @examples
#' set.seed(0, kind = "L'Ecuyer-CMRG")
#' train.data <- RaModel("classification", 1, n = 100, p = 50)
#' xtrain <- train.data$x
#' ytrain <- train.data$y
#' \dontrun{
#' # test RaSE classifier with LDA base classifier
#' fit <- SmultiRase(xtrain, ytrain, B1 = 50, B2 = 50, iteration = 0, cutoff = TRUE,
#' base = 'lda', cores = 2, ranking = TRUE)
#'
#' # print the summarized results
#' print(fit)
#' }

print.mRaSE <- function(x, ...) {
  cat("Type of base classifiers:", x$base, "\n")
  cat("Criterion: 5-folds CV", "\n")
  cat("number of class:",x$nmulti,"\n")
  cat("B1:", x$B1, "\n")
  cat("B2:", x$B2, "\n")
  cat("D:", x$D, "\n")
  cat("Cutoff:", x$cutoff, "\n")
  if (!is.null(x$ranking)) {
    cat("Selected percentage of each feature appearing in B1 subspaces:", "\n")
    print(x$ranking)
  }
}
