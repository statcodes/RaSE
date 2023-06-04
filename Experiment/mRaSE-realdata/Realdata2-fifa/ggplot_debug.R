library(r02pro)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(forcats)
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



# 400: iter = 1 SVM (10)
# 600: iter = 1 Logi (9)
# 800: iter = 1 Logi (9)

# mst_ft <- stat_tab[c(10,21,33),]

mst_ft <- matrix(0,ncol = 20,nrow = 3)
mst_ft[1,] <- order(unlist(rk_400[10,]),decreasing = T)[1:20]
mst_ft[2,] <- order(unlist(rk_600[9,]),decreasing = T)[1:20]
mst_ft[3,] <- order(unlist(rk_800[9,]),decreasing = T)[1:20]

rk_400 <- as_tibble(rk_400)
tb_400 <-
  rk_400 %>%
  mutate(model = rep(md_name[1:6],2),
         iter = as.factor(rep(c(0,1),c(6,6)))) %>%
  pivot_longer(!iter & !model,names_to = "feature",values_to = "percentage") %>%
  group_by(model,iter) %>%
  summarise(feature = c(mst_ft[1,],rep("N",1876 - length(mst_ft[1,]))),
            percentage = c(percentage[mst_ft[1,]],
                           percentage[-mst_ft[1,]])) %>%
  ungroup()

rk_600 <- as_tibble(rk_600)
tb_600 <-
  rk_600 %>%
  mutate(model = rep(md_name[1:6],2),
         iter = as.factor(rep(c(0,1),c(6,6)))) %>%
  pivot_longer(!iter & !model,names_to = "feature",values_to = "percentage") %>%
  group_by(model,iter) %>%
  summarise(feature = c(mst_ft[2,],rep("N",1876 - length(mst_ft[2,]))),
            percentage = c(percentage[mst_ft[2,]],
                           percentage[-mst_ft[2,]])) %>%
  ungroup()

rk_800 <- as_tibble(rk_800)
tb_800 <-
  rk_800 %>%
  mutate(model = rep(md_name[1:6],2),
         iter = as.factor(rep(c(0,1),c(6,6)))) %>%
  pivot_longer(!iter & !model,names_to = "feature",values_to = "percentage") %>%
  group_by(model,iter) %>%
  summarise(feature = c(mst_ft[3,],rep("N",1876 - length(mst_ft[3,]))),
            percentage = c(percentage[mst_ft[3,]],
                           percentage[-mst_ft[3,]])) %>%
  ungroup()


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

# pdf(file = "C:/Users/74714/OneDrive/Desktop/realdata_fifa/realdata_fifa_feature_plot.pdf",
#     width = 11.54,
#     height = 6.20)

load("fifa22.RData")
names(spx)[mst_ft]
feature_name <- c(names(spx)[mst_ft],"N")


# ggplot() +
#   geom_bar(mapping = aes(fill = iter,
#                          x = reorder(feature,-percentage),
#                          y = percentage),
#            position = "dodge",
#            stat="identity",data = tb %>% filter(feature != "N")) +
#   geom_boxplot(aes(fill = iter,x = feature,y = percentage),data = tb %>% filter(feature == "N")) +
#   facet_grid(cols =vars(n) ,rows = vars(model)) +
#   theme(strip.text.x = element_text(size = 10),
#         strip.text.y = element_text(size = 10),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 6)) 
# # +
# #   scale_x_discrete(labels= str_wrap(feature_name,width = 2))


# dev.off()




# try on tb_400

tb_400$feature <- fct_relevel(tb_400$feature,as.character(c(mst_ft[1,],"N")))

ggplot() +
  geom_bar(mapping = aes(fill = iter,
                         x = feature,
                         y = percentage),
           position = "dodge",
           stat="identity",data = tb_400 %>% filter(feature != "N")) + 
  geom_boxplot(aes(fill = iter,
                   x = length(mst_ft[1,]) + 1,
                   y = percentage),
               data = tb_400 %>% filter(feature == "N")) +
  scale_x_discrete(limits = c(mst_ft[1,],"N")) +
  facet_grid(rows = vars(model)) 



