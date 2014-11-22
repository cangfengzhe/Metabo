# 药物相似怐�<U+3E37>

drug_symptom_count <- na.omit(result)
nrow(drug_symptom_count)
save(drug_symptom_count, file = "drug_symptom_count.rdata")
load("drug_symptom_count.rdata")
drug_symptom_count <- as.data.frame(drug_symptom_count)
View(drug_symptom_count)
# 1021修改
drugSymptomCount <- sqldf("select drug_symptom_count.* from drug_symptom_count \n      inner join molName on drug_symptom_count.V1=molName.moleculeName")
# construct the sparseMatrix


# 11-12
molSym <- read.csv("./result/final//molSymAll.csv", stringsAsFactors = F)
View(molSym)
View(drugSymptomCount)
# drug_symptom_count_mat<-matrix(as.numeric(drugSymptomCount[,1:3]),ncol
# = 3)




mat <- molSym[, c(1, 2, 5)]
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

# aa 每一列（症状）对应的疾病数目
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

# 计算 cos
cos <- matrix(NA, nrow(sparseMat2) * nrow(sparseMat2), 5)
nn <- 0
for (ii in 1:(nrow(sparseMat2) - 1)) {
    for (jj in (ii + 1):nrow(sparseMat2)) {
        nn <- nn + 1
        sum_xy <- sum(sparseMat2[ii, ] * sparseMat2[jj, ])
        sum_x2y2 <- sqrt(sum(sparseMat2[ii, ]^2)) * sqrt(sum(sparseMat2[jj, 
            ]^2))
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
drugSymSimilarAll <- sqldf("select * from drugSymCos \n                         left join drugID2Name on\n                         drugSymCos.V1=drugID2Name.V4")
drugSymSimilarAll <- sqldf("select * from drugSymSimilarAll\n                         left join drugID2Name on\n                         drugSymSimilarAll.V2=drugID2Name.V4")
View(drugSymSimilarAll)
drugSymSimilarAll <- na.omit(drugSymSimilarAll)
save(drugSymCos, sparseMat2, drugSimpID, drugSymSimilarAll, file = "drugsimilar.rdata")

nrow(drugSymSimilarAll)
drugSymSimilarAll_nor0 <- drugSymSimilarAll[drugSymSimilarAll[, 5] != 0, 
    ]
View(drugSymSimilarAll_nor0)
write.csv(drugSymSimilarAll_nor0, file = "./result/final/drugSymSimilarAll_1112.csv")
 
