#' Predict a deep learning model via keras.
#'
#' Predict a deep learning model.
#' @export
#' @importFrom keras to_categorical
#' @importFrom keras keras_model_sequential
#' @importFrom keras layer_dense
#' @importFrom keras compile
#' @param fit dnn fitted object.
#' @param xtrain n * p observation matrix. n observations, p features.

#' @return Predicted response.
#' \item{ypred}{The predicted response.}

#' @references
#' Tian, Y. and Feng, Y., 2021(a). RaSE: A variable screening framework via random subspace ensembles. Journal of the American Statistical Association, (just-accepted), pp.1-30.
#'
#' Tian, Y. and Feng, Y., 2021(b). RaSE: Random subspace ensemble classification. Journal of Machine Learning Research, 22(45), pp.1-93.
#'
#' Zhu, J. and Feng, Y., 2021. Super RaSE: Super Random Subspace Ensemble Classification. https://www.preprints.org/manuscript/202110.0042
#'
#' Chen, J. and Chen, Z., 2008. Extended Bayesian information criteria for model selection with large model spaces. Biometrika, 95(3), pp.759-771.
#'
#' Chen, J. and Chen, Z., 2012. Extended BIC for small-n-large-P sparse GLM. Statistica Sinica, pp.555-574.
#'
#' Akaike, H., 1973. Information theory and an extension of the maximum likelihood principle. In 2nd International Symposium on Information Theory, 1973 (pp. 267-281). Akademiai Kaido.
#'
#' Schwarz, G., 1978. Estimating the dimension of a model. The annals of statistics, 6(2), pp.461-464.
#'
#'


predict.dnn <- function(fit = dnn_fit, newdata = x_test){

    label <- fit$label
    newdata <- sapply(1:ncol(newdata),
                      function(j){
                        (newdata[,j]-fit$min_seq[j])/ (fit$max_seq[j]-fit$min_seq[j])
                      })
    y_pred <- fit$model %>% predict(newdata)
    y_pred_label <- apply(y_pred, 1, which.max)
    return(label[y_pred_label, 1])
}


