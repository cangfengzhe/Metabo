
# 寻找85分子对应的症状
library(sqldf)
molSym <- read.csv('D:\\Program Files\\RStudio\\RFile\\TCM\\result\\final\\network\\molecule-symptom.csv',
                 stringsAsFactors = F)

mol <- unique(molSym[,1])
mol <- as.data.frame(mol)
mol <- sample(mol[,1],100)
mol <- as.character(mol)
mol <- as.data.frame(mol)

# mol <- read.csv('D:\\Program Files\\RStudio\\RFile\\TCM\\result\\final\\network\\80mol.csv', stringsAsFactors = F)

out <- sqldf('select * from mol left join molSym on mol.mol = molSym.name')

write.csv(out,file='zuotu.csv')
