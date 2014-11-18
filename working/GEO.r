#stroke GEO data analysis
library(GEOquery)
gse <- getGEO(filename = system.file("/GSE781_family.soft.gz",
                                     + package = "GEOquery"))
source("http://bioconductor.org/biocLite.R")
 biocLite("ArrayExpress")
gse <- getGEO(filename="D:\\DeskTop\\GSE33725.gz",destdir ="D:\\DeskTop")
#.gz文件必须是SOFT formatted family file(s)
save(gse,file='geo.rdata')
install.packages('arrayQualityMetrix')
library
meta(gse)
Meta(gse)
