#' Fit a deep learning model via keras.
#'
#' Fit a deep learning model.
#' @export
#' @importFrom keras to_categorical
#' @importFrom keras keras_model_sequential
#' @importFrom keras layer_dense
#' @importFrom keras compile
#' @importFrom keras layer_dropout
#' @importFrom keras optimizer_rmsprop
#' @importFrom generics fit
#' @param x_train a
#' @param y_train a
#' @param levels a
#' @param max_units a
#' @param dropout_rate a
#' @param start a
#'

#' @return An Keras fitted model.
#' \item{model}{The fitted Keras fitted model.}

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


dnn <- function(x_train, y_train, levels = 4, max_units = 256,
                dropout_rate = 0.1, start = 0){

    ntrain <- nrow(x_train)
    ptrain <- ncol(x_train)
    min_seq <- apply(x_train, 2, min)
    max_seq <- apply(x_train, 2, max)
    x_train <- sapply(1:ncol(x_train),
                      function(j){
                        (x_train[,j]-min_seq[j])/ (max_seq[j]-min_seq[j])
                      })

    y_trans <- labeltrans(y_train, start = 0)
    y_train <- y_trans$vnew
    nclass <- length(unique(y_train))
    y_train <- to_categorical(y_train)
    model <- keras_model_sequential()
    model %>%
      layer_dense(units = max_units, activation = 'relu', input_shape = c(ptrain))
    for(j in 1:(levels-1)){
      model %>%
        layer_dropout(rate = dropout_rate) %>%
        layer_dense(units = max(max_units/(2^j),16), activation = "relu")
    }
    model %>%
      layer_dropout(rate = dropout_rate) %>%
      layer_dense(units = nclass, activation = 'softmax')
    model %>% compile(
      loss = 'categorical_crossentropy',
      optimizer = optimizer_rmsprop(),
      metrics = c('accuracy')
    )
    history <- model %>% fit(
      x_train, y_train,
      epochs = 100, batch_size = 64,
      validation_split = 0.1
    )
    obj <- list(model = model,
                label = y_trans$label,
                min_seq = min_seq,
                max_seq = max_seq)
    class(obj) <- "dnn"
    return(obj)
}


