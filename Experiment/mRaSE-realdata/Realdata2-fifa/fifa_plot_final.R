library(r02pro)
library(ggplot2)
library(gridExtra)
library(tidyverse)
load("fifa_summary.RData")

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

rk_400 <- feature[,,1]/(100*200)
rownames(rk_400) <- md_name
colnames(rk_400) <- 1:1876


#ntrain = 600
test.err <- c(result_600$error_test,result_600$error_basic_test)
train.err <- c(result_600$error_train,result_600$error_basic_train)
var.test <- c(result_600$var_test,result_600$var_basic_test)
var.train <- c(result_600$var_test,result_600$var_basic_test)

md_name <- c("lda","knn","logistic","svm","tree","super",
             "lda_it","knn_it","logistic_it","svm_it","tree_it","super_it")
md_name <- paste("mRaSE",md_name,sep = "_")
md_name_basic <- c("lda","knn","logistic","svm","tree","rf","dnn")

d600 <-
  data.frame(model = c(md_name,md_name_basic),
             test.error = test.err,
             train.error = train.err,
             var.test = var.test,
             var.train = var.train)
d600 <- d600[,c(1,2,4)]
d600[,2] <- d600[,2]*100
d600[,3] <- d600[,3]*1000

rk_600 <- feature[,,2]/(100*200)
rownames(rk_600) <- md_name
colnames(rk_600) <- 1:1876


#ntrain = 800
test.err <- c(result_800$error_test,result_800$error_basic_test)
train.err <- c(result_800$error_train,result_800$error_basic_train)
var.test <- c(result_800$var_test,result_800$var_basic_test)
var.train <- c(result_800$var_test,result_800$var_basic_test)

md_name <- c("lda","knn","logistic","svm","tree","super",
             "lda_it","knn_it","logistic_it","svm_it","tree_it","super_it")
md_name <- paste("mRaSE",md_name,sep = "_")
md_name_basic <- c("lda","knn","logistic","svm","tree","rf","dnn")

d800 <-
  data.frame(model = c(md_name,md_name_basic),
             test.error = test.err,
             train.error = train.err,
             var.test = var.test,
             var.train = var.train)
d800 <- d800[,c(1,2,4)]
d800[,2] <- d800[,2]*100
d800[,3] <- d800[,3]*1000

rk_800 <- feature[,,3]/(100*200)
rownames(rk_800) <- md_name
colnames(rk_800) <- 1:1876

d400$model <- c(paste("mRaSE",c("LDA","KNN","Logi","SVM","Tree"),
                      sep = "-"),"SmRaSE",
                paste("mRaSE$_1$",c("LDA","KNN","Logi","SVM","Tree"),
                      sep = "-"),"SmRaSE$_1$",
                c("LDA","KNN","Logi","SVM","Tree","RF","DNN"))
d600$model <- c(paste("mRaSE",c("LDA","KNN","Logi","SVM","Tree"),
                      sep = "-"),"SmRaSE",
                paste("mRaSE$_1$",c("LDA","KNN","Logi","SVM","Tree"),
                      sep = "-"),"SmRaSE$_1$",
                c("LDA","KNN","Logi","SVM","Tree","RF","DNN"))
d800$model <- c(paste("mRaSE",c("LDA","KNN","Logi","SVM","Tree"),
                       sep = "-"),"SmRaSE",
                 paste("mRaSE$_1$",c("LDA","KNN","Logi","SVM","Tree"),
                       sep = "-"),"SmRaSE$_1$",
                 c("LDA","KNN","Logi","SVM","Tree","RF","DNN"))

xtab(x = cbind(d400[,c(1,2)],d600[,2],d800[,2]),
     x2 = cbind(d400[,c(1,3)],d600[,3],d800[,3]),digits = 2)

##count plot
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

names(nt)[1:4] <- c("base","400","600","800")

nt <- nt %>%
  pivot_longer(cols = ends_with("0"),
               names_to = "size",
               values_to = "percentage")

#588 590
ggplot(data = nt) +
  geom_bar(mapping = aes(fill = base,x = base,y = percentage),
           position = "dodge",
           stat="identity",data = nt) +
  facet_grid(cols = vars(iter) ,rows = vars(factor(size,levels = c("400",
                                                                   "600",
                                                                   "800")))) +
  labs(fill = "base classifier") +
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

## feature plot

stat_tab <- matrix(0,nrow = 36,ncol = 20)

for(i in 1:36){
  pos <- 1 + ((1:36)-1)%/%12
  idrow <- rep(1:12,3)
  stat_tab[i,] <- order(feature[idrow[i],,pos[i]],decreasing = T)[1:5]
}

mst_ft <- as.numeric(names(sort(table(stat_tab),decreasing = T)[1:5]))

rk_400 <- as_tibble(rk_400)
tb_400 <-
  rk_400 %>%
  mutate(model = rep(md_name[1:6],2),
         iter = as.factor(rep(c(0,1),c(6,6)))) %>%
  pivot_longer(!iter & !model,names_to = "feature",values_to = "percentage") %>%
  group_by(model,iter) %>%
  summarise(feature = c(mst_ft,rep("N",1876 - length(mst_ft))),
            percentage = c(percentage[mst_ft],
                           percentage[-mst_ft])) %>%
  ungroup()

rk_600 <- as_tibble(rk_600)
tb_600 <-
  rk_600 %>%
  mutate(model = rep(md_name[1:6],2),
         iter = as.factor(rep(c(0,1),c(6,6)))) %>%
  pivot_longer(!iter & !model,names_to = "feature",values_to = "percentage") %>%
  group_by(model,iter) %>%
  summarise(feature = c(mst_ft,rep("N",1876 - length(mst_ft))),
            percentage = c(percentage[mst_ft],
                           percentage[-mst_ft])) %>%
  ungroup()

rk_800 <- as_tibble(rk_800)
tb_800 <-
  rk_800 %>%
  mutate(model = rep(md_name[1:6],2),
         iter = as.factor(rep(c(0,1),c(6,6)))) %>%
  pivot_longer(!iter & !model,names_to = "feature",values_to = "percentage") %>%
  group_by(model,iter) %>%
  summarise(feature = c(mst_ft,rep("N",1876 - length(mst_ft))),
            percentage = c(percentage[mst_ft],
                           percentage[-mst_ft])) %>%
  ungroup()


pl <- list()
flag = 1

tl <- factor(c("mRaSE-LDA","mRaSE-KNN","mRaSE-Logistic","mRaSE-SVM","mRaSe-tree","SmRaSE"),
             levels = c("mRaSE-LDA","mRaSE-KNN","mRaSE-Logistic","mRaSE-SVM","mRaSe-tree","SmRaSE"))

tb_400 <- mutate(tb_400,n = 400,model = factor(model) %>% fct_recode("KNN" = "mRaSE_knn",
                                                                     "LDA" = "mRaSE_lda",
                                                                     "Logi" = "mRaSE_logistic",
                                                                     "SVM" = "mRaSE_svm",
                                                                     "Tree" = "mRaSE_tree",
                                                                     "Super" = "mRaSE_super") %>%
                   fct_relevel(c("LDA","KNN","Logi","SVM","Tree","Super")))
tb_600 <- mutate(tb_600,n = 600,model = factor(model) %>% fct_recode("KNN" = "mRaSE_knn",
                                                                     "LDA" = "mRaSE_lda",
                                                                     "Logi" = "mRaSE_logistic",
                                                                     "SVM" = "mRaSE_svm",
                                                                     "Tree" = "mRaSE_tree",
                                                                     "Super" = "mRaSE_super") %>%
                   fct_relevel(c("LDA","KNN","Logi","SVM","Tree","Super")))
tb_800 <- mutate(tb_800,n = 800,model = factor(model) %>% fct_recode("KNN" = "mRaSE_knn",
                                                                        "LDA" = "mRaSE_lda",
                                                                        "Logi" = "mRaSE_logistic",
                                                                        "SVM" = "mRaSE_svm",
                                                                        "Tree" = "mRaSE_tree",
                                                                        "Super" = "mRaSE_super") %>%
                    fct_relevel(c("LDA","KNN","Logi","SVM","Tree","Super")))
tb <- rbind(tb_400,tb_600,tb_800)


ggplot() +
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

load("fifa22.RData")
names(spx)[mst_ft]

## table
