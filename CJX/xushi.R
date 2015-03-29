# qizhi='“Distending-pain' OR 'Wandering pain' OR 'string
# pain' OR 'Tightness' OR 'oppressive pain' OR 'stuffiness
# unstable pain' OR 'depression' OR 'dysphoria' OR 'sigh' OR
# 'Belching' OR 'string pulse'' qixu=''Secret anguish' OR
# 'vague pain' OR 'Dull pain' OR 'Dyspnea' OR 'Shortness of
# breath' OR 'Hypodynamia' OR 'Spiritlessness' OR
# 'Disinclination to say' OR 'Dizziness' OR 'Symptoms aggravate
# after activity' OR 'spontaneous perspiration' OR 'Light
# tongue' OR 'Light tongue' OR 'Ptosis of stomach' OR 'Ptosis
# of kidney' OR 'ptosis' OR 'Weak pulse''
qizhiqixu = read.csv("D:\\deskTop\\LPD\\cjx\\qizhiqixu.csv", stringsAsFactors = F)
View(qizhiqixu)
qizhi = qizhiqixu[1:11, 1]
qixu = qizhiqixu[1:nrow(qizhiqixu), 2]

# qizhiPmid<-downloadPmid(qizhi)
nrow(qixuPmid)



# 获取气滞气虚pmid
qizhiPmid <- neiRes
qixuPmid <- neiRes

View(qizhiPmid)


kk <- 0
neiRes <- matrix(NA, 1, 1)
n <- 0
zz <- NA
errorRes <- NA
ii = 1
for (ii in 1:length(qizhi)) {
    
    molName <- qizhi[ii]
    
    
    tryCatch({
        
        prestr = paste(c("\"", molName, "\" [title/abstract]"), 
            collapse = "")
        # prestr=molName
        pmid <- downloadPmid(prestr)
        if (is.matrix(pmid)) {
            kk <- kk + 1
            # pmid0<-paste(pmid,collapse = ','); neiRes[kk,1]<-molName;
            # neiRes[kk,2]<-nrow(pmid); neiRes[kk,3]<-pmid0;
            tmp = pmid
            
            
            print(nrow(pmid))
            neiRes <- rbind(neiRes, tmp)
            
            neiRes <- unique(neiRes)
            
            print(paste("kk", kk))
        }
    }, error = function(e) {
        n <<- n + 1
        errorRes[n] <<- ii
        
        print(e)
    })
    
}

# 与NEI文献pmid取交集
greNEI[, 1] <- as.character(greNEI[, 1])
qizhi2NEI <- intersect(qizhiPmid, greNEI[, 1])
# 获取qizhi相关文献
qizhiLit <- as.data.frame(result0)
nrow(qixuLit)
length(qizhi2NEI)  #关于气滞的文献；
qixu2NEI <- intersect(qixuPmid[, 1], greNEI[, 1])
length(qixu2NEI)
class(qixuPmid[, 1])
length(qizhiPmid)


greNEI <- as.data.frame(NEIWenXian, stringsAsFactors = F)
nrow(greNEI)
aa <- grepl("cold", greNEI[, 2:3])
paste(c(greNEI[2:5, 2], greNEI[2:5, 3]))
greNEI[, 2] <- as.character(greNEI[, 2])
greNEI[, 3] <- as.character(greNEI[, 3])
class(greNEI[, 2])

aa <- apply(X = greNEI[, 2:3], "paste", c(collapse = ""))
View(aa)
flag <- NA
coldNum <- matrix(NA, nrow(cold), 2)
qixuLit0 <- NA
for (ii in 1:length(qixu)) {
    flag <- grepl(qixu[ii], greNEI[, 3], perl = T)
    # flag<-grepl(paste(c('\\b',qixu[ii],'\\b'),collapse =
    # ''),greNEI[,3],perl=T) flag<-grepl('hot',greNEI[,2])
    index <- which(flag == T)
    qixuLit0 <- c(qixuLit0, greNEI[index, 1])
    print(length(index))
}
ii = 1
View(qizhiLit0)

View(qixuLit)

aa <- match(qizhiLit0, qizhiLit[, 1])
length(aa)
length(qizhiLit0)
nrow(qizhiLit)
nrow(qizhiPmid)




# 联合匹配

errorRes <- matrix(NA, 10000, 2)
warnRes <- matrix(NA, 10000, 2)

n <- 0
m <- 0
kk <- 0
neiRes <- NA
ii = 1
jj = 1
for (ii in 1:nrow(nei)) {
    for (jj in 1:length(qizhi)) {
        molName <- nei[ii, 1]
        meshName <- qizhi[jj]
        print(paste("jj", jj))
        
        tryCatch({
            
            prestr = paste(c("\"", molName, "\"[title/abstract] AND \"", 
                meshName, "\"[title/abstract]"), collapse = "")
            pmid <- downloadPmid(prestr)
            if (is.matrix(pmid)) {
                tmp = pmid
                neiRes <- rbind(neiRes, tmp)
                neiRes <- unique(neiRes)
                kk <- kk + 1
                print(paste("kk", kk))
            }
        }, error = function(e) {
            n <<- n + 1
            errorRes[n, 1] <<- ii
            errorRes[n, 2] <<- jj
            print(e)
        })
        
    }
    print(paste("ii", ii))
    
}
length(qizhi2NEI)
qixu2NEI
gene <- gene[, 3:4]  #全部基因，同义词
# 匹配 基因
neiGene <- read.csv("neiGene.csv", stringsAsFactors = F)
View(neiGene)
names(neiGene) <- c("id", "geneName")
View(geneDic)
View(humanGene)
library(sqldf)
gene <- sqldf("select * from neiGene left join geneDic  on neiGene.id=geneDic.id")
# gene 全部 的NEI相关基因
gene <- unique(gene)
geneDic <- as.data.frame(geneDic)
colnames(geneDic) <- c("id", "name")
kk <- 0
errorRes <- NA
geneLit <- matrix(NA, nrow(gene), 3)
for (ii in 1:nrow(gene)) {
    tryCatch({
        geneName <- gene[ii, 2]
        litBool <- grepl(paste(c("\\b", geneName, "\\b|\\(", geneName, 
            "\\)"), collapse = ""), qixuLit[, 3], perl = T)
        litIndex <- which(litBool == T)
        ii <- ii + 1
        geneLit[ii, 1] <- gene[ii, 1]
        geneLit[ii, 2] <- geneName
        geneLit[ii, 3] <- length(litIndex)
        print(ii)
    }, error = function(e) {
        kk <<- kk + 1
        errorRes[kk] <<- ii
        
    })
    
}

geneNum <- geneLit[geneLit[, 3] > 0, ]  #取共发生的基因
geneNum <- na.omit(geneNum)
qizhiGene <- unique(geneNum)
write.csv(geneNum, file = "qizhiGene0911.csv")  #气滞相关的基因写入文件

# 气虚文献
qixuLit <- as.data.frame(result0)
View(qixuLit)
geneLit <- na.omit(geneLit)
geneLit <- geneLit[geneLit[, 3] > 0, ]
qixuGene <- geneLit
View(qixuGene) 
