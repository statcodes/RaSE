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
#' @importFrom dplyr %>%
#' @param xtrain n * p observation matrix. n observations, p features.
#' @param ytrain n 0/1 observations.
#' @param levels a

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


dl <- function(xtrain, ytrain, levels = 4){
    ntrain <- nrow(xtrain)
    #one_hot_train_labels <- to_categorical(ytrain)
 #   one_hot_test_labels <- to_categorical(ytest)
    model <- keras_model_sequential()

    model %>%
        layer_dense(units = 64, activation = "relu",
                    input_shape = ncol(xtrain))
    for(j in 1:(levels-1)){
        model %>%
            layer_dense(units = 64, activation = "relu") %>%
            layer_dropout(rate = 0.4)
    }
   model %>%
        layer_dense(units = length(unique(ytrain)), activation = "softmax")
   summary(model)
   model %>% compile(
       loss = 'categorical_crossentropy',
       optimizer = optimizer_rmsprop(),
       metrics = c('accuracy')
   )

    history <- model %>% fit(
        xtrain,
        ytrain,
        epochs = 30,
        batch_size = 128,
        validation_split = 0.2
    )
    return(model)
}
