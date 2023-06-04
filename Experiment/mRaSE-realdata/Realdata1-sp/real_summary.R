ntrain_seq <- c(600, 900, 1200)
base_seq <- c("lda", "knn", "logistic", "svm", "tree")
all_para <- expand.grid(ntrain_seq,1:6)

md_name <- c("lda","knn","logistic","svm","tree","super",
             "lda_it","knn_it","logistic_it","svm_it","tree_it","super_it")
md_name_basic <- c("lda","knn","logistic","svm","tree","rf","dnn")

error_mat_test <- matrix(0, 12, 300)
rownames(error_mat_test) <- md_name

error_mat_train <- matrix(0, 12, 300)
rownames(error_mat_train) <- md_name

error_mat_basic_test <- matrix(0,7,1800)
rownames(error_mat_basic_test) <- md_name_basic

error_mat_basic_train <- matrix(0,7,1800)
rownames(error_mat_basic_train) <- md_name_basic

feature <- array(0,c(12,561,3))

for(i in 1:1800){
  
  set_id <- floor((i-1)/100)+1
  para <- all_para[set_id, ]
  ntrain <- as.numeric(para[1])
  if(ntrain == 600){
    pos <- 1
  }else if(ntrain == 900){pos <- 2}else{pos <- 3}
  
  setwd(paste0("/scratch/fb2234/Realdata1-result",pos))
  cat("(counting error)starts = ",i,'\n')
  
  base <- base_seq[as.numeric(para[2])]
  if(is.na(base)){base = base_seq}
  # ex <- as.numeric(para[3])
  base_name <- ifelse(length(base) == 1,base,"super")
  
  file_name <- paste0(base_name,ntrain,"_",i-(set_id-1)*100,".RData")
  load(file_name)
  idx <- grep(base_name,md_name)
  
  error_mat_test[idx[1],i-(set_id-1)*100 + (pos-1)*100] = test.error
  error_mat_test[idx[2],i-(set_id-1)*100 + (pos-1)*100] = test.error.it
  
  error_mat_train[idx[1],i-(set_id-1)*100 + (pos-1)*100] = train.error
  error_mat_train[idx[2],i-(set_id-1)*100 + (pos-1)*100] = train.error.it
  
  error_mat_basic_test[1,i] = test.error.lda
  error_mat_basic_test[2,i] = test.error.knn
  error_mat_basic_test[3,i] = test.error.logistic
  error_mat_basic_test[4,i] = test.error.svm
  error_mat_basic_test[5,i] = test.error.tree
  error_mat_basic_test[6,i] = test.error.rf
  error_mat_basic_test[7,i] = test.error.dnn
  
  error_mat_basic_train[1,i] = train.error.lda
  error_mat_basic_train[2,i] = train.error.knn
  error_mat_basic_train[3,i] = train.error.logistic
  error_mat_basic_train[4,i] = train.error.svm
  error_mat_basic_train[5,i] = train.error.tree
  error_mat_basic_train[6,i] = train.error.rf
  error_mat_basic_train[7,i] = train.error.dnn
  
  for(j in 1:200){
    s <- as.numeric(unlist(p1$subspace[j]))
    feature[idx[1],s,pos] <- feature[idx[1],s,pos] + 1
    s1 <- as.numeric(unlist(p1_it$subspace[j]))
    feature[idx[2],s1,pos] <- feature[idx[2],s1,pos] + 1
  }
}

count <- matrix(0,3,5)
colnames(count) <- base_seq
count_it <- matrix(0,3,5)
colnames(count_it) <- base_seq

for(i in 1:300){
  cat("(counting base)starts = ",i,'\n')
  set_id <- floor((i-1)/100)+1
  num <- i-(set_id-1)*100
  setwd(paste0("/scratch/fb2234/Realdata1-result",set_id))
  file_name <- paste0("super",ntrain_seq[set_id],"_",num,".RData")
  load(file_name)
  count[set_id,1] <- count[set_id,1] + ifelse(is.na(p1$rk$ranking.base["lda"]),0,
                                              p1$rk$ranking.base["lda"])
  count[set_id,2] <- count[set_id,2] + ifelse(is.na(p1$rk$ranking.base["knn"]),0,
                                              p1$rk$ranking.base["knn"])
  count[set_id,3] <- count[set_id,3] + ifelse(is.na(p1$rk$ranking.base["logistic"]),0,
                                              p1$rk$ranking.base["logistic"])
  count[set_id,4] <- count[set_id,4] + ifelse(is.na(p1$rk$ranking.base["svm"]),0,
                                              p1$rk$ranking.base["svm"])
  count[set_id,5] <- count[set_id,5] + ifelse(is.na(p1$rk$ranking.base["tree"]),0,
                                              p1$rk$ranking.base["tree"])
  
  count_it[set_id,1] <- count_it[set_id,1] + ifelse(is.na(p1_it$rk$ranking.base["lda"]),0,
                                                    p1_it$rk$ranking.base["lda"])
  count_it[set_id,2] <- count_it[set_id,2] + ifelse(is.na(p1_it$rk$ranking.base["knn"]),0,
                                                    p1_it$rk$ranking.base["knn"])
  count_it[set_id,3] <- count_it[set_id,3] + ifelse(is.na(p1_it$rk$ranking.base["logistic"]),0,
                                                    p1_it$rk$ranking.base["logistic"])
  count_it[set_id,4] <- count_it[set_id,4] + ifelse(is.na(p1_it$rk$ranking.base["svm"]),0,
                                                    p1_it$rk$ranking.base["svm"])
  count_it[set_id,5] <- count_it[set_id,5] + ifelse(is.na(p1_it$rk$ranking.base["tree"]),0,
                                                    p1_it$rk$ranking.base["tree"])
}

count <- count/100
count_it <- count_it/100

error_test_600 <- rowMeans(error_mat_test[,1:100])
var_test_600 <- apply(error_mat_test[,1:100],1,var)

error_test_900 <- rowMeans(error_mat_test[,101:200])
var_test_900 <- apply(error_mat_test[,101:200],1,var)

error_test_1200 <- rowMeans(error_mat_test[,201:300])
var_test_1200 <- apply(error_mat_test[,201:300],1,var)

error_train_600 <- rowMeans(error_mat_train[,1:100])
var_train_600 <- apply(error_mat_train[,1:100],1,var)

error_train_900 <- rowMeans(error_mat_train[,101:200])
var_train_900 <- apply(error_mat_train[,101:200],1,var)

error_train_1200 <- rowMeans(error_mat_train[,201:300])
var_train_1200 <- apply(error_mat_train[,201:300],1,var)

id_1 <- rep(seq(1:100),6) + sort(rep(seq(0,1500,300),100))
id_2 <- id_1 + 100
id_3 <- id_2 + 100

error_basic_test_600 <- rowMeans(error_mat_basic_test[,id_1],na.rm =T)
var_basic_test_600 <- apply(error_mat_basic_test[,id_1],1,var)

error_basic_test_900 <- rowMeans(error_mat_basic_test[,id_2],na.rm =T)
var_basic_test_900 <- apply(error_mat_basic_test[,id_2],1,var)

error_basic_test_1200 <- rowMeans(error_mat_basic_test[,id_3],na.rm =T)
var_basic_test_1200 <- apply(error_mat_basic_test[,id_3],1,var)

error_basic_train_600 <- rowMeans(error_mat_basic_train[,id_1],na.rm = T)
var_basic_train_600 <- apply(error_mat_basic_train[,id_1],1,var)

error_basic_train_900 <- rowMeans(error_mat_basic_train[,id_2],na.rm = T)
var_basic_train_900 <- apply(error_mat_basic_train[,id_2],1,var)

error_basic_train_1200 <- rowMeans(error_mat_basic_train[,id_3],na.rm = T)
var_basic_train_1200 <- apply(error_mat_basic_train[,id_3],1,var)


result_600 <- list(error_test = error_test_600,error_train = error_train_600,
                   error_basic_test = error_basic_test_600,
                   error_basic_train = error_basic_train_600,
                   var_test = var_test_600, var_train = var_train_600,
                   var_basic_test = var_basic_test_600,
                   var_basic_train = var_basic_train_600)
result_900 <- list(error_test = error_test_900,error_train = error_train_900,
                   error_basic_test = error_basic_test_900,
                   error_basic_train = error_basic_train_900,
                   var_test = var_test_900, var_train = var_train_900,
                   var_basic_test = var_basic_test_900,
                   var_basic_train = var_basic_train_900)
result_1200 <- list(error_test = error_test_1200,error_train = error_train_1200,
                    error_basic_test = error_basic_test_1200,
                    error_basic_train = error_basic_train_1200,
                    var_test = var_test_1200, var_train = var_train_1200,
                    var_basic_test = var_basic_test_1200,
                    var_basic_train = var_basic_train_1200)


setwd("/scratch/fb2234/Realdata1-summary")
save(result_600,result_900,result_1200,feature,
     count,count_it,file = paste0("sp_summary.RData"))

