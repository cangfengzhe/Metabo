# 处理甲基化芯片数捐�<U+3E65>

# install.packages('lumi')
library(lumi)
# source('http://bioconductor.org/biocLite.R') biocLite('BiocUpgrade')

# biocLite('lumi')

# fileName <-
# 'D:/deskTop/GSE28647_signal_intensities.txt/GSE28647_signal_intensities.txt'
# load the data example.lumiMethy <- lumiMethyR(fileName,
# lib='IlluminaHumanMethylation27k.db')
install.packages("dplyr")
biocLite("genefilter")
biocLite("Biobase") 
