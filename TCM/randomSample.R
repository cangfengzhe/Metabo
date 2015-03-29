# 随机抽取文献

molName <- read.csv("./result/molName2.csv", stringsAsFactors = F)
View(molName)
library(sqldf)
molName2Sym <- sqldf("select molSym.* from molName left join molSym\n                   on molName.moleculeName=molName")

randomSample <- sample(unique(molName2Sym$pmid), 2000)
length(randomSample)
randomSample <- as.data.frame(randomSample)
randomAbstract <- sqldf("select * from randomSample left join pubmed_abstract\n                      on randomSample.randomSample=pubmed_abstract.pmid")

randomAbstract <- sqldf("select * from randomAbstract left join molName2Sym\n                      on randomAbstract.pmid=molName2Sym.pmid")
View(randomAbstract)
randomAbstract <- unique(randomAbstract)
randomAbstract <- randomAbstract[, c(8, 9, 3, 4, 5, 6, 7)]
randomAbstract1 <- randomAbstract[randomAbstract[, 5] != "", ]
randomAbstract <- randomAbstract1[1:2000, ]
View(randomAbstract) 
