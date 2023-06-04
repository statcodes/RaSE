library(r02pro)
library(ggplot2)
library(gridExtra)
library(tidyverse)
load("summary4.RData")

#ntrain = 200
test.err <- c(result_200$error_test,result_200$error_basic_test)
train.err <- c(result_200$error_train,result_200$error_basic_train)
var.test <- c(result_200$var_test,result_200$var_basic_test)
var.train <- c(result_200$var_test,result_200$var_basic_test)


md_name <- c("lda","knn","logistic","svm","tree","super",
             "lda_it","knn_it","logistic_it","svm_it","tree_it","super_it")
md_name <- paste("mRaSE",md_name,sep = "_")
md_name_basic <- c("lda","knn","logistic","svm","tree","rf","dnn")

d200 <- 
data.frame(model = c(md_name,md_name_basic),
           test.error = test.err,
           train.error = train.err,
           var.test = var.test,
           var.train = var.train)

d200 <- d200[,c(1,2,4)]
d200[,2] <- d200[,2]*100
d200[,3] <- d200[,3]*1000

rk_200 <- feature[,,1]/(100*200)
rownames(rk_200) <- md_name
colnames(rk_200) <- 1:400
#rk_200[,c(1,2,5)]
#rk_200[,c(1,2,10,30,50)]

# barplot(rk_200[,1],ylim = c(0,1))
# barplot(rk_200[,2],ylim = c(0,1))
# barplot(rk_200[,5],ylim = c(0,1))

#ntrain = 400
test.err <- c(result_400$error_test,result_400$error_basic_test)
train.err <- c(result_400$error_train,result_400$error_basic_train)
var.test <- c(result_400$var_test,result_400$var_basic_test)
var.train <- c(result_400$var_test,result_400$var_basic_test)

md_name <- c("lda","knn","logistic","svm","tree","super",
             "lda_it","knn_it","logistic_it","svm_it","tree_it","super_it")
md_name <- paste("mRaSE",md_name,sep = "_")
md_name_basic <- c("lda","knn","logistic","svm","tree","rf","dnn")

d400 <-
data.frame(model = c(md_name,md_name_basic),
           test.error = test.err,
           train.error = train.err,
           var.test = var.test,
           var.train = var.train)

d400 <- d400[,c(1,2,4)]
d400[,2] <- d400[,2]*100
d400[,3] <- d400[,3]*1000

rk_400 <- feature[,,2]/(100*200)
rownames(rk_400) <- md_name
colnames(rk_400) <- 1:400
# rk_400[,c(1,2,5)]
#rk_400[,c(1,2,10,30,50)]


# barplot(rk_400[,1],ylim = c(0,1))
# barplot(rk_400[,2],ylim = c(0,1))
# barplot(rk_400[,5],ylim = c(0,1))

#ntrain = 1000
test.err <- c(result_1000$error_test,result_1000$error_basic_test)
train.err <- c(result_1000$error_train,result_1000$error_basic_train)
var.test <- c(result_1000$var_test,result_1000$var_basic_test)
var.train <- c(result_1000$var_test,result_1000$var_basic_test)

md_name <- c("lda","knn","logistic","svm","tree","super",
             "lda_it","knn_it","logistic_it","svm_it","tree_it","super_it")
md_name <- paste("mRaSE",md_name,sep = "_")
md_name_basic <- c("lda","knn","logistic","svm","tree","rf","dnn")

d1000 <-
data.frame(model = c(md_name,md_name_basic),
           test.error = test.err,
           train.error = train.err,
           var.test = var.test,
           var.train = var.train)

d1000 <- d1000[,c(1,2,4)]
d1000[,2] <- d1000[,2]*100
d1000[,3] <- d1000[,3]*1000

rk_1000 <- feature[,,3]/(100*200)
rownames(rk_1000) <- md_name
colnames(rk_1000) <- 1:400
#rk_1000[,c(1,2,5)]
#rk_1000[,c(1,2,10,30,50)]

# barplot(rk_1000[,1],ylim = c(0,1))
# barplot(rk_1000[,2],ylim = c(0,1))
# barplot(rk_1000[,5],ylim = c(0,1))

d200$model <- c(paste("mRaSE",c("LDA","KNN","Logi","SVM","Tree"),
                      sep = "-"),"SmRaSE",
                paste("mRaSE$_1$",c("LDA","KNN","Logi","SVM","Tree"),
                      sep = "-"),"SmRaSE$_1$",
                c("LDA","KNN","Logi","SVM","Tree","RF","DNN"))
d400$model <- c(paste("mRaSE",c("LDA","KNN","Logi","SVM","Tree"),
                      sep = "-"),"SmRaSE",
                paste("mRaSE$_1$",c("LDA","KNN","Logi","SVM","Tree"),
                      sep = "-"),"SmRaSE$_1$",
                c("LDA","KNN","Logi","SVM","Tree","RF","DNN"))
d1000$model <- c(paste("mRaSE",c("LDA","KNN","Logi","SVM","Tree"),
                       sep = "-"),"SmRaSE",
                 paste("mRaSE$_1$",c("LDA","KNN","Logi","SVM","Tree"),
                       sep = "-"),"SmRaSE$_1$",
                 c("LDA","KNN","Logi","SVM","Tree","RF","DNN"))

source("xtab.R")
xtab(x = cbind(d200[,c(1,2)],d400[,2],d1000[,2]),
     x2 = cbind(d200[,c(1,3)],d400[,3],d1000[,3]),digits = 2)

## feature plot

rk_200 <- as_tibble(rk_200) 

tb_200 <-
  rk_200 %>%
  mutate(model = rep(md_name[1:6],2),
         iter = rep(c(0,1),c(6,6))) %>%
  pivot_longer(!iter & !model,names_to = "feature",values_to = "percentage")

tb_200[["feature"]][tb_200[["feature"]] != 1 &
                      tb_200[["feature"]] != 2 &
                      tb_200[["feature"]] != 3] <- "N" 
tb_200[["iter"]] <- as.factor(tb_200[["iter"]])

rk_400 <- as_tibble(rk_400) 

tb_400 <-
  rk_400 %>%
  mutate(model = rep(md_name[1:6],2),
         iter = rep(c(0,1),c(6,6))) %>%
  pivot_longer(!iter & !model,names_to = "feature",values_to = "percentage")

tb_400[["feature"]][tb_400[["feature"]] != 1 &
                      tb_400[["feature"]] != 2 &
                      tb_400[["feature"]] != 3] <- "N" 
tb_400[["iter"]] <- as.factor(tb_400[["iter"]])

rk_1000 <- as_tibble(rk_1000) 

tb_1000 <-
  rk_1000 %>%
  mutate(model = rep(md_name[1:6],2),
         iter = rep(c(0,1),c(6,6))) %>%
  pivot_longer(!iter & !model,names_to = "feature",values_to = "percentage")

tb_1000[["feature"]][tb_1000[["feature"]] != 1 &
                       tb_1000[["feature"]] != 2 &
                       tb_1000[["feature"]] != 3] <- "N" 
tb_1000[["iter"]] <- as.factor(tb_1000[["iter"]])

tl <- factor(c("mRaSE-LDA","mRaSE-KNN","mRaSE-Logistic","mRaSE-SVM","mRaSe-tree","SmRaSE"),
             levels = c("mRaSE-LDA","mRaSE-KNN","mRaSE-Logistic","mRaSE-SVM","mRaSe-tree","SmRaSE"))

tb_200 <- mutate(tb_200,n = 200,model = factor(model) %>% fct_recode("KNN" = "mRaSE_knn",
                                                                     "LDA" = "mRaSE_lda",
                                                                     "Logi" = "mRaSE_logistic",
                                                                     "SVM" = "mRaSE_svm",
                                                                     "Tree" = "mRaSE_tree",
                                                                     "Super" = "mRaSE_super") %>%
                   fct_relevel(c("LDA","KNN","Logi","SVM","Tree","Super")))
tb_400 <- mutate(tb_400,n = 400,model = factor(model) %>% fct_recode("KNN" = "mRaSE_knn",
                                                                     "LDA" = "mRaSE_lda",
                                                                     "Logi" = "mRaSE_logistic",
                                                                     "SVM" = "mRaSE_svm",
                                                                     "Tree" = "mRaSE_tree",
                                                                     "Super" = "mRaSE_super") %>%
                   fct_relevel(c("LDA","KNN","Logi","SVM","Tree","Super")))
tb_1000 <- mutate(tb_1000,n = 1000,model = factor(model) %>% fct_recode("KNN" = "mRaSE_knn",
                                                                        "LDA" = "mRaSE_lda",
                                                                        "Logi" = "mRaSE_logistic",
                                                                        "SVM" = "mRaSE_svm",
                                                                        "Tree" = "mRaSE_tree",
                                                                        "Super" = "mRaSE_super") %>%
                    fct_relevel(c("LDA","KNN","Logi","SVM","Tree","Super")))
tb <- rbind(tb_200,tb_400,tb_1000)
tb[[4]] <- tb[[4]] * 100


ggplot(data = tb) +
  geom_bar(mapping = aes(fill = iter,
                         x = feature,
                         y = percentage),
           position = "dodge",
           stat="identity",data = tb %>% filter(feature != "N")) +
  geom_boxplot(aes(fill = iter,x = feature,y = percentage),data = tb %>% filter(feature == "N")) +
  facet_grid(cols =vars(n) ,rows = vars(model)) +
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
#5.04 * 6.17

## count plot
nm <- colnames(count)

tb <- data.frame(md = nm,
                 s1 = unname(count[1,]),
                 s2 = unname(count[2,]),
                 s3 = unname(count[3,]))
tb1 <- data.frame(md = nm,
                  s1 = unname(count_it[1,]),
                  s2 = unname(count_it[2,]),
                  s3 = unname(count_it[3,]))
nt <- rbind(tb,tb1)

nt[,1] <- as.factor(nt[,1])
nt[,1] <-   fct_recode(nt[,1],"LDA" = "lda","KNN" = "knn","Logi" = "logistic",
                       "SVM" = "svm","Tree" = "tree")
nt[,1] <- fct_relevel(nt[,1],c("LDA","KNN","Logi","SVM","Tree")) 
nt <- nt %>%mutate(iter = rep(c(0,1),c(5,5))) 

names(nt)[1:4] <- c("base","200","400","1000")

nt <- nt %>%
  pivot_longer(cols = ends_with("0"),
               names_to = "size",
               values_to = "percentage")

#588 590
ggplot(data = nt) +
  geom_bar(mapping = aes(fill = base,x = base,y = percentage),
           position = "dodge",
           stat="identity",data = nt) +
  facet_grid(cols = vars(iter) ,rows = vars(factor(size,levels = c("200",
                                                                   "400",
                                                                   "1000")))) +
  labs(fill = "base classifier") +
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

