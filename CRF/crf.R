system("crf_learn template train.data model")

system.time(aa <- system("crf_test -m model test.data", intern = T))
library(koRpus)
# set.kRp.env(TT.cmd='C:\\TreeTagger\\bin\\tree-tagger.exe')#useless
# parse the file test.txt,important step
Text <- read.delim("Text.txt", na.string = " ")
tagged.results <- treetag("train.txt", treetagger = "manual", lang = "en", 
    TT.options = list(path = "C:\\TreeTagger", preset = "en"))
# 'test.txt' indicate the path of file that you need

test <- tagged.results@TT.res
result <- cbind.data.frame(aa[, 1], test[, 2], aa[, 2])
write.table(result, "train.txt", quote = F, col.names = F, row.names = F)  #各种没有



write.table(x = aa[, 1], file = "train.txt", quote = F, col.names = F, 
    row.names = F)
writeLines(as.character(aa[, 1]), "train.txt")


# train data pre-process
trainRaw <- read.delim("trainRaw.txt")
trainRaw001 <- read.delim("trainRaw001.txt")  #空行以XXX代替 
View(trainRaw)
write.table(x = trainRaw001[, 1], file = "train01.txt", quote = F, 
    col.names = F, row.names = F)
# train01.txt 含有一列词
train01lin <- read.delim("train01lin.txt", header = F)
# 合并
trainData <- cbind.data.frame(train01lin[, c(1, 3)], trainRaw001[, 
    2])
# 导出最终的trainset
write.table(x = trainData, file = "trainData.txt", quote = F, col.names = F, 
    row.names = F)




# protein and gene
library(ff)
ffload("../textMining/xxProtein")
View(xxProtein)
zzzPro <- paste("zzz", xxProtein[, 1])  #为pmid增加标示符zzz
preProtein <- cbind.data.frame(zzzPro, xxProtein[, c(1, 2, 3)])  #合并
write.table(x = preProtein[, c(1, 3, 4)], file = "proteinRaw.txt", 
    quote = F, col.names = F, row.names = F)
# genia tagger 处理

read.delim("D:/deskTop/xxTM/")
geneRaw <- read.delim("D:/deskTop/xxTM/xxgeneRawlin.txt", header = F)
write.table(x = geneRaw[, c(1, 3)], file = "geneData.txt", quote = F, 
    col.names = F, row.names = F)


# 后续处理
geneData <- read.delim("D:/deskTop/xxTM/geneResult.txt", header = F)
proteinData <- read.delim("D:/deskTop/xxTM/proteinRawlin.txt", 
    header = F)
proteinData <- proteinData[, c(1, 3)]
write.table(x = proteinData, file = "proteinData.txt", quote = F, 
    col.names = F, row.names = F)
proteinData <- read.delim("proteinRes.txt", header = F)
proteinData[substr(proteinData$V1, 1, 3) == "zzz", 3] <- "A"
View(proteinData)
class(proteinData$V3)
proteinData$V3 <- as.character(proteinData$V3)
proteinDataTmp <- proteinData[proteinData$V3 != "O", ]
View(proteinDataTmp) 
