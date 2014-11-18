#采用正则表达式从已下载的stroke文献中提取其他疾病
#结果strokeDis分3列存储，mesh疾病，匹配文献数目，pubmedid
library(ff)
ffload('result')
meshDis<-read.csv('meshDis.csv',header=F)
strokeDis<-matrix(NA,nrow(meshDis),3)
colnames(strokeDis)<-c('mesh','count','pmid')
stroke<-as.data.frame(result[,c(1,3)])
n<-0;
error0<-NA
ii=1
for(ii in 452: nrow(meshDis)){
  tryCatch(
  {
    tmp<-grep(as.character(meshDis[ii,1]),stroke[,2])
    strokeDis[ii,1]<-as.character(meshDis[ii,1])
    strokeDis[ii,2]<-length(tmp)
    strokeDis[ii,3]<-paste(stroke[tmp,1],collapse = ',')
    print(ii)
    if(is.integer(ii/200)==T){
      save(strokeDis,file='strokeDis.rdata')
    }
  },
  error =function(e){   #捕获错误
    n<<-n+1;
    error0[n]<<-ii; #将错误保存到 error0 
    print('chucuole');
  }
    )
}
load('strokeDis.rdata')
class(strokeDis[,2])

strokeD<-as.data.frame(strokeDis,stringsAsFactors=F)
strokeD[1,]
strokeD[,2]<-as.numeric(strokeD[,2])
class(strokeD[,2])
sort(strokeD[1:20,2])
strokeDis[,2]<-as.numeric(strokeDis[,2])
class(strokeDis[,2])

