# lumi package is used to process the Illumina Infinium 27k and 450k methylation microarray data
fileName <- 'D:/deskTop/GSE28647_signal_intensities.txt/GSE28647_signal_intensities.txt'

example.lumiMethy <- lumiMethyR(fileName,seq='\t', lib="IlluminaHumanMethylation27k.db")
??.getFileSeparator

library(limma)
browseVignettes("limma")

source("http://www.bioconductor.org/biocLite.R")
biocLite("limma")


setClass("Person",slots=list(name="character",age="numeric"))

# 创建Person的子类
setClass("Son",slots=list(father="Person",mother="Person"),contains="Person")

# 实例化Person对象
father<-new("Person",name="F",age=44)
mother<-new("Person",name="M",age=39)
# 可以继承实例化
son<-new("Son",name="S",age=16,father=father,mother=mother)
son@name

slot(son, 'name')
isS4(son)
# 实例化一个Son对象
setValidity('Persion',funtion(object){
  if(object@age<0) stop
})


setGeneric("work",function(object) standardGeneric("work"))
setMethod("work", signature(object = "Person"), function(object) cat(object@name , "is working") )
work(son)
class(x)


