pkgname <- "RaSEn"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "RaSEn-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('RaSEn')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("RaModel")
### * RaModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RaModel
### Title: Generate data (x, y) from various models in two papers.
### Aliases: RaModel

### ** Examples

train.data <- RaModel("classification", 1, n = 100, p = 50)
xtrain <- train.data$x
ytrain <- train.data$y

## Not run: 
##D train.data <- RaModel("screening", 2, n = 100, p = 50)
##D xtrain <- train.data$x
##D ytrain <- train.data$y
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RaModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RaPlot")
### * RaPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RaPlot
### Title: Visualize the feature ranking results of a fitted RaSE object.
### Aliases: RaPlot

### ** Examples

set.seed(0, kind = "L'Ecuyer-CMRG")
train.data <- RaModel("classification", 1, n = 100, p = 50)
xtrain <- train.data$x
ytrain <- train.data$y

# fit RaSE classifier with QDA base classifier
fit <- Rase(xtrain, ytrain, B1 = 50, B2 = 50, iteration = 1, base = 'qda',
cores = 2, criterion = 'ric')

# plot the selected percentage of each feature appearing in B1 subspaces
RaPlot(fit)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RaPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RaRank")
### * RaRank

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RaRank
### Title: Rank the features by selected percentages provided by the output
###   from 'RaScreen'.
### Aliases: RaRank

### ** Examples

## Not run: 
##D set.seed(0, kind = "L'Ecuyer-CMRG")
##D train.data <- RaModel("screening", 1, n = 100, p = 100)
##D xtrain <- train.data$x
##D ytrain <- train.data$y
##D 
##D # test RaSE screening with linear regression model and BIC
##D fit <- RaScreen(xtrain, ytrain, B1 = 100, B2 = 50, iteration = 0, model = 'lm',
##D cores = 2, criterion = 'bic')
##D 
##D # Select floor(n/logn) variables
##D RaRank(fit, selected.num = "n/logn")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RaRank", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RaScreen")
### * RaScreen

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RaScreen
### Title: Variable screening via RaSE.
### Aliases: RaScreen

### ** Examples

set.seed(0, kind = "L'Ecuyer-CMRG")
train.data <- RaModel("screening", 1, n = 100, p = 100)
xtrain <- train.data$x
ytrain <- train.data$y

# test RaSE screening with linear regression model and BIC
fit <- RaScreen(xtrain, ytrain, B1 = 100, B2 = 50, iteration = 0, model = 'lm',
cores = 2, criterion = 'bic')

# Select D variables
RaRank(fit, selected.num = "D")


## Not run: 
##D # test RaSE screening with knn model and 5-fold cross-validation MSE
##D fit <- RaScreen(xtrain, ytrain, B1 = 100, B2 = 50, iteration = 0, model = 'knn',
##D cores = 2, criterion = 'cv', cv = 5)
##D 
##D # Select n/logn variables
##D RaRank(fit, selected.num = "n/logn")
##D 
##D 
##D # test RaSE screening with SVM and 5-fold cross-validation MSE
##D fit <- RaScreen(xtrain, ytrain, B1 = 100, B2 = 50, iteration = 0, model = 'svm',
##D cores = 2, criterion = 'cv', cv = 5)
##D 
##D # Select n/logn variables
##D RaRank(fit, selected.num = "n/logn")
##D 
##D 
##D # test RaSE screening with logistic regression model and eBIC (gam = 0.5). Set iteration number = 1
##D train.data <- RaModel("screening", 6, n = 100, p = 100)
##D xtrain <- train.data$x
##D ytrain <- train.data$y
##D 
##D fit <- RaScreen(xtrain, ytrain, B1 = 100, B2 = 100, iteration = 1, model = 'logistic',
##D cores = 2, criterion = 'ebic', gam = 0.5)
##D 
##D # Select n/logn variables from the selected percentage after one iteration round
##D RaRank(fit, selected.num = "n/logn", iteration = 1)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RaScreen", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Rase")
### * Rase

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Rase
### Title: Construct the random subspace ensemble classifier.
### Aliases: Rase

### ** Examples

set.seed(0, kind = "L'Ecuyer-CMRG")
train.data <- RaModel("classification", 1, n = 100, p = 50)
test.data <- RaModel("classification", 1, n = 100, p = 50)
xtrain <- train.data$x
ytrain <- train.data$y
xtest <- test.data$x
ytest <- test.data$y

# test RaSE classifier with LDA base classifier
fit <- Rase(xtrain, ytrain, B1 = 100, B2 = 50, iteration = 0, base = 'lda',
cores = 2, criterion = 'ric')
mean(predict(fit, xtest) != ytest)

## Not run: 
##D # test RaSE classifier with LDA base classifier and 1 iteration round
##D fit <- Rase(xtrain, ytrain, B1 = 100, B2 = 50, iteration = 1, base = 'lda',
##D cores = 2, criterion = 'ric')
##D mean(predict(fit, xtest) != ytest)
##D 
##D # test RaSE classifier with QDA base classifier and 1 iteration round
##D fit <- Rase(xtrain, ytrain, B1 = 100, B2 = 50, iteration = 1, base = 'qda',
##D cores = 2, criterion = 'ric')
##D mean(predict(fit, xtest) != ytest)
##D 
##D # test RaSE classifier with kNN base classifier
##D fit <- Rase(xtrain, ytrain, B1 = 100, B2 = 50, iteration = 0, base = 'knn',
##D cores = 2, criterion = 'loo')
##D mean(predict(fit, xtest) != ytest)
##D 
##D # test RaSE classifier with logistic regression base classifier
##D fit <- Rase(xtrain, ytrain, B1 = 100, B2 = 50, iteration = 0, base = 'logistic',
##D cores = 2, criterion = 'bic')
##D mean(predict(fit, xtest) != ytest)
##D 
##D # test RaSE classifier with SVM base classifier
##D fit <- Rase(xtrain, ytrain, B1 = 100, B2 = 50, iteration = 0, base = 'svm',
##D cores = 2, criterion = 'training')
##D mean(predict(fit, xtest) != ytest)
##D 
##D # test RaSE classifier with random forest base classifier
##D fit <- Rase(xtrain, ytrain, B1 = 20, B2 = 10, iteration = 0, base = 'randomforest',
##D cores = 2, criterion = 'cv', cv = 3)
##D mean(predict(fit, xtest) != ytest)
##D 
##D # fit a super RaSE classifier by sampling base learner from kNN, LDA and logistic
##D # regression in equal probability
##D fit <- Rase(xtrain = xtrain, ytrain = ytrain, B1 = 100, B2 = 100,
##D base = c("knn", "lda", "logistic"), super = list(type = "separate", base.update = T),
##D criterion = "cv", cv = 5, iteration = 1, cores = 2)
##D mean(predict(fit, xtest) != ytest)
##D 
##D # fit a super RaSE classifier by sampling base learner from random forest, LDA and
##D # SVM with probability 0.2, 0.5 and 0.3
##D fit <- Rase(xtrain = xtrain, ytrain = ytrain, B1 = 100, B2 = 100,
##D base = c(randomforest = 0.2, lda = 0.5, svm = 0.3),
##D super = list(type = "separate", base.update = F),
##D criterion = "cv", cv = 5, iteration = 0, cores = 2)
##D mean(predict(fit, xtest) != ytest)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Rase", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("predict.RaSE")
### * predict.RaSE

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: predict.RaSE
### Title: Predict the outcome of new observations based on the estimated
###   RaSE classifier (Tian, Y. and Feng, Y., 2021).
### Aliases: predict.RaSE

### ** Examples

## Not run: 
##D set.seed(0, kind = "L'Ecuyer-CMRG")
##D train.data <- RaModel("classification", 1, n = 100, p = 50)
##D test.data <- RaModel("classification", 1, n = 100, p = 50)
##D xtrain <- train.data$x
##D ytrain <- train.data$y
##D xtest <- test.data$x
##D ytest <- test.data$y
##D 
##D model.fit <- Rase(xtrain, ytrain, B1 = 100, B2 = 100, iteration = 0, base = 'lda',
##D cores = 2, criterion = 'ric', ranking = TRUE)
##D ypred <- predict(model.fit, xtest)
##D mean(ypred != ytest)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("predict.RaSE", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("predict.super_RaSE")
### * predict.super_RaSE

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: predict.super_RaSE
### Title: Predict the outcome of new observations based on the estimated
###   super RaSE classifier (Zhu, J. and Feng, Y., 2021).
### Aliases: predict.super_RaSE

### ** Examples

## Not run: 
##D set.seed(0, kind = "L'Ecuyer-CMRG")
##D train.data <- RaModel("classification", 1, n = 100, p = 50)
##D test.data <- RaModel("classification", 1, n = 100, p = 50)
##D xtrain <- train.data$x
##D ytrain <- train.data$y
##D xtest <- test.data$x
##D ytest <- test.data$y
##D 
##D # fit a super RaSE classifier by sampling base learner from kNN, LDA and
##D # logistic regression in equal probability
##D fit <- Rase(xtrain = xtrain, ytrain = ytrain, B1 = 100, B2 = 100,
##D base = c("knn", "lda", "logistic"), super = list(type = "separate", base.update = T),
##D criterion = "cv", cv = 5, iteration = 1, cores = 2)
##D ypred <- predict(fit, xtest)
##D mean(ypred != ytest)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("predict.super_RaSE", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print.RaSE")
### * print.RaSE

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print.RaSE
### Title: Print a fitted RaSE object.
### Aliases: print.RaSE

### ** Examples

set.seed(0, kind = "L'Ecuyer-CMRG")
train.data <- RaModel("classification", 1, n = 100, p = 50)
xtrain <- train.data$x
ytrain <- train.data$y

# test RaSE classifier with LDA base classifier
fit <- Rase(xtrain, ytrain, B1 = 50, B2 = 50, iteration = 0, cutoff = TRUE,
base = 'lda', cores = 2, criterion = 'ric', ranking = TRUE)

# print the summarized results
print(fit)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print.RaSE", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print.super_RaSE")
### * print.super_RaSE

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print.super_RaSE
### Title: Print a fitted super_RaSE object.
### Aliases: print.super_RaSE

### ** Examples

set.seed(0, kind = "L'Ecuyer-CMRG")
train.data <- RaModel("classification", 1, n = 100, p = 50)
xtrain <- train.data$x
ytrain <- train.data$y

# test RaSE classifier with LDA base classifier
fit <- Rase(xtrain, ytrain, B1 = 50, B2 = 50, iteration = 0, cutoff = TRUE,
base = 'lda', cores = 2, criterion = 'ric', ranking = TRUE)

# print the summarized results
print(fit)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print.super_RaSE", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
