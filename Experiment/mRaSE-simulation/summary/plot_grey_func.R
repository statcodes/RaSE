library(ggplot2)
library(gridExtra)
library(tidyverse)
library(foreach)

nrep <- 100
p <- 400
B1 <- 200
B2 <- 500

# index <- 1: LDA scenario
# index <- 3: KNN scenario
# index <- 4: Logistics scenario

index <- 3
saved_data  <- paste0("C:/Users/74714/OneDrive/Desktop/review/summary/summary",index,".RData")

load(saved_data)
rm(saved_data)

if(index == 1){
  signal_index <- c(1,2,5)
} else if (index == 3){
  signal_index <- 1:5
} else if(index == 4){
  signal_index <- 1:3
}

result_list <- list(result_200,result_400,result_1000)

result_tabs <- 
foreach(i = 1:3,
        .packages = c("dplyr",
                      "tidyr")) %do% {
  
  test.err <- c(result_list[[i]]$error_test,result_list[[i]]$error_basic_test)
  train.err <- c(result_list[[i]]$error_train,result_list[[i]]$error_basic_train)
  var.test <- c(result_list[[i]]$var_test,result_list[[i]]$var_basic_test)
  var.train <- c(result_list[[i]]$var_test,result_list[[i]]$var_basic_test)
  
  
  md_name <- c("lda","knn","logistic","svm","tree","super",
               "lda_it","knn_it","logistic_it","svm_it","tree_it","super_it")
  md_name <- paste("mRaSE",md_name,sep = "_")
  md_name_basic <- c("lda","knn","logistic","svm","tree","rf","dnn")
  
  d <- 
    data.frame(model = c(md_name,md_name_basic),
               test.error = test.err,
               train.error = train.err,
               var.test = var.test,
               var.train = var.train)
  
  d <- d[,c(1,2,4)]
  d[,2] <- d[,2]*100
  d[,3] <- d[,3]*1000
  d$model <- c(paste("mRaSE",c("LDA","KNN","Logi","SVM","Tree"),
                        sep = "-"),"SmRaSE",
                  paste("mRaSE$_1$",c("LDA","KNN","Logi","SVM","Tree"),
                        sep = "-"),"SmRaSE$_1$",
                  c("LDA","KNN","Logi","SVM","Tree","RF","DNN"))
  
  rk <- feature[,,i]/(nrep*B1)
  rownames(rk) <- md_name
  colnames(rk) <- 1:p
  
  rk <- as_tibble(rk)
  tb <-
    rk %>%
    mutate(model = rep(md_name[1:6],2),
           iter = rep(c(0,1),c(6,6))) %>%
    pivot_longer(!iter & !model,names_to = "feature",values_to = "percentage")
  
  cond <- paste(paste("tb[['feature']] != ",signal_index),collapse = " & ")
  
  tb[["feature"]][eval(parse(text = cond))] <- "N"
  tb[["iter"]] <- as.factor(tb[["iter"]])
  
  lst <- list(error_tab = d,ranking = tb)
  rm(test.err,
     train.err,
     var.test,
     var.train,
     d,rk,cond,tb)
  
  return(lst)
                      }

cbind(result_tabs[[1]]$error_tab,
      result_tabs[[2]]$error_tab[,-1],
      result_tabs[[3]]$error_tab[,-1])


tl <- factor(c("mRaSE-LDA","mRaSE-KNN","mRaSE-Logistic","mRaSE-SVM","mRaSe-tree","SmRaSE"),
             levels = c("mRaSE-LDA","mRaSE-KNN","mRaSE-Logistic","mRaSE-SVM","mRaSe-tree","SmRaSE"))

tb <- foreach(i = 1:3, .combine = "rbind") %do% {
  
  result_tabs[[i]]$ranking %>%
    mutate(n = c(200,400,1000)[i],model = factor(model) %>% fct_recode("KNN" = "mRaSE_knn",
                                                               "LDA" = "mRaSE_lda",
                                                               "Logi" = "mRaSE_logistic",
                                                               "SVM" = "mRaSE_svm",
                                                               "Tree" = "mRaSE_tree",
                                                               "Super" = "mRaSE_super") %>%
             fct_relevel(c("LDA","KNN","Logi","SVM","Tree","Super")))

}
tb[[4]] <- tb[[4]] * 100

ggplot(data = tb) +
  geom_bar(mapping = aes(fill = iter,
                         x = feature,
                         y = percentage),
           position = "dodge",
           stat="identity",data = tb %>% filter(feature != "N")) +
  geom_boxplot(aes(fill = iter,x = feature,y = percentage),data = tb %>% filter(feature == "N")) +
  scale_fill_grey(start = 0.4, end = 0.8) + 
  facet_grid(cols =vars(n) ,rows = vars(model)) +
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

ggsave(
  "plot1.png",
  width = 5.04,
  height = 6.17,
  dpi = 1200
)



nm <- colnames(count)

tb0 <- data.frame(md = nm,
                 s1 = unname(count[1,]),
                 s2 = unname(count[2,]),
                 s3 = unname(count[3,]))
tb1 <- data.frame(md = nm,
                  s1 = unname(count_it[1,]),
                  s2 = unname(count_it[2,]),
                  s3 = unname(count_it[3,]))
nt <- rbind(tb0,tb1)

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
        axis.title.y = element_text(size = 12)) + 
  scale_fill_grey()

ggsave(
  "plot2.png",
  width = 5.88,
  height = 6.90,
  dpi = 1200
)