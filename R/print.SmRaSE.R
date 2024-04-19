#' Print a fitted SmRaSE object.
#'
#' Similar to the usual print methods, this function summarizes results.
#' from a fitted \code{'SmRaSE'} object.
#' @export
#' @param x fitted \code{'SmRaSE'} model object.
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
#' fit <- RaSE(xtrain, ytrain, B1 = 50, B2 = 50, iteration = 0, cutoff = TRUE,
#' base = 'lda', cores = 2, ranking = TRUE)
#'
#' # print the summarized results
#' print(fit)
#' }

print.SmRaSE <- function(x, ...) {
  cat("Count of base classifier types among", x$B1, "classifiers: \n")
  print(x$ranking$ranking.base * B1)
  cat("Criterion: ",x$criterion, "\n")
  cat("B1:", x$B1, "\n")
  cat("B2:", x$B2, "\n")
  cat("number of class:",x$nmulti,"\n")
  cat("D: \n")
  print(x$D)
  cat("Cutoff:", x$cutoff, "\n")
  if (!is.null(x$ranking)) {
    cat("Selected percentage of each feature appearing in B1 subspaces under different base classifier types:", "\n")
    print(x$ranking)
  }
}
