# 药物相似性计算
# author: Li Pidong
# date: 2015-10-21



setwd('./tcm_xx/')
molSym <- read_csv("./result_2015_10_13/mol_sym.csv") %>% as.data.frame()
# View(molSym)
# View(drugSymptomCount)
# drug_symptom_count_mat<-matrix(as.numeric(drugSymptomCount[,1:3]),ncol = 3)

mat <- molSym[, c(2, 1, 3)] # 分子-症状-文献数目
mat <- na.omit(mat)
mat <- unique(mat)
mat[, 1] <- as.character(mat[, 1])
mat[, 2] <- as.character(mat[, 2])
mat[, 4] <- as.factor(mat[, 1])
mat[, 5] <- as.factor(mat[, 2])
mat[, 4] <- as.numeric(mat[, 4])
mat[, 5] <- as.numeric(mat[, 5])
mat[, 3] <- as.numeric(mat[, 3])
length(unique(mat[, 5]))
View(mat)

library(Matrix)
sparseMat <- sparseMatrix(mat[, 4], mat[, 5], x = mat[, 3])
sparseMat1 <- as.matrix(sparseMat)

disease_allcount <- length(unique(mat[, 1]))
# aa<-matrix(1:100,10,10)

# aa 每一列分子对应的症状数目
aa <- lapply(X = 1:ncol(sparseMat1), function(x) {
  length(sparseMat1[sparseMat1[, x] != 0, x])
})

sym_colCount <- do.call("rbind", aa)
log <- log(disease_allcount/sym_colCount)
sparseMat2 <- sparseMat1  #sparseMat2 为sparseMat1 乘以 log
for (ii in 1:nrow(sparseMat1)) {
  sparseMat2[ii, ] <- sparseMat1[ii, ] * log
  
}
View(sparseMat2)

cos <- matrix(NA, nrow(sparseMat2) * nrow(sparseMat2), 5)
nn <- 0
for (ii in 1:(nrow(sparseMat2) - 1)) {
  for (jj in (ii + 1):nrow(sparseMat2)) {
    nn <- nn + 1
    
    sum_xy <- sum(sparseMat2[ii, ] * sparseMat2[jj, ])
    sum_x2y2 <- sqrt(sum(sparseMat2[ii, ]^2)) * sqrt(sum(sparseMat2[jj, ]^2))
    cos[nn, 1] <- ii
    cos[nn, 2] <- jj
    cos[nn, 3] <- sum_xy
    cos[nn, 4] <- sum_x2y2
    cos[nn, 5] <- sum_xy/sum_x2y2
  }
  print(ii)
}

# date 2014-10-14 8:34 match drug and symptom's id and name
drugSimpID <- mat
drugID2Name <- unique(drugSimpID[, c(1, 4)])
sympID2Name <- unique(drugSimpID[, c(2, 5)])

drugSymCos <- cos

drugSymCos <- as.data.frame(drugSymCos)
colnames(drugID2Name)
# match name with id in the table drugSymCos
library(sqldf)
drugSymSimilarAll <- left_join(drugSymCos, drugID2Name, by=c('V1'='V4')) %>% 
  left_join(drugID2Name, by = c('V2' = 'V4'))

View(drugSymSimilarAll)
drugSymSimilarAll <- na.omit(drugSymSimilarAll)
save(drugSymCos, sparseMat2, drugSimpID, drugSymSimilarAll, file = "drugsimilar.rdata")

nrow(drugSymSimilarAll)
drugSymSimilarAll_nor0 <- drugSymSimilarAll[drugSymSimilarAll[, 5] != 0, ]
View(drugSymSimilarAll_nor0)
write.csv(drugSymSimilarAll_nor0, file = "./result/final/drugSymSimilarAll_1112.csv") 



write_csv(drugSymSimilarAll, path = './result_2015_10_13/mol_similar_2.csv')
