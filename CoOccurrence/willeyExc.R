#ii=3,jj=8,ii=13,jj=7
library(ff)
library(ffbase)
aa<-willey('drought','wheat')
date()

#250 2min
species<-read.csv('species.csv',header=F,stringsAsFactors=F)
condition<-c('drought','water deficit stress')
xingtaixue<-read.csv('xingtaixue.csv',header=F,stringsAsFactors=F)
degree<-read.csv('degree.csv',header=F,stringsAsFactors=F)

Lit<-data.frame(keyword1=NA,keyword2=NA,title=NA,doi=NA,time=NA);
Lit<-as.ffdf(Lit);

kk<-0
#干旱-物种文献ganhan_wuzhongLit保存在 ganhan_wuzhong.rdata,ganhan_wuzhong.csv文件 

#写入 mysql
shenglixue<-read.csv('shenglixue.csv',header=F,stringsAsFactors=F)
View(shenglixue)
shenglixue<-unique(shenglixue)
xingtaixue<-unique(xingtaixue)
degree<-unique(degree)
nrow(degree)
nrow(xingtaixue)

kk<-0
errRes<-matrix(NA,1000,2)
for (ii in  1:nrow(shenglixue)){ 
 # keyword_1=shenglixue[ii,1]
  keyword_1='lipid peroxidation'
  for (jj in 1:nrow(degree)){  
   # keyword_1=xingtaixue[jj,1]
   keyword_2=degree[jj,1]
    #keyword_1=xingtaixue[errRes[jj,1],1]
    #keyword_2=degree[errRes[jj,2],1]
    tryCatch({
      willeyFullText(keyword_1,keyword_2)
      #if(!is.na(Lit0)){
     # Lit<-rbind(Lit,Lit0)
     #   Lit0<-as.ffdf(Lit0)
     # Lit<- ffdfappend(Lit,Lit0, adjustvmode=F)
      print(paste(c('ii:',ii,'/31;jj:',jj,'/23'), collapse=''))
      #}
    },error=function(e){
      kk<<-kk+1;
      errRes[kk,1]<<-ii;
      errRes[kk,2]<<-jj;
      print(e)
    })
   
    
  }
}


#特殊词
shenglixueteshu<-read.csv('shenglixueteshu.csv',header=F,stringsAsFactors=F)
xingtaiteshu<-read.csv('xingtaiteshu.csv',header=F,stringsAsFactors=F)
View(shenglixueteshu)
kk<-0
errResTeshu<-matrix(NA,100,2)
  for (ii in 1:nrow(shenglixueteshu)){ 
    keyword_1=shenglixueteshu[ii,1]
    keyword_2=shenglixueteshu[ii,2]
    tryCatch({
      willeyFullTextwater(keyword_1,keyword_2)
      #if(!is.na(Lit0)){
      # Lit<-rbind(Lit,Lit0)
      #   Lit0<-as.ffdf(Lit0)
      # Lit<- ffdfappend(Lit,Lit0, adjustvmode=F)
      print(paste(c('ii:',ii), collapse=''))
      #}
    },error=function(e){
      kk<<-kk+1;
      errResTeshu[kk,1]<<-ii;
      
      print(e)
    })
    
    
  }
}



#处理 err

  kk=1
  for (jj in 1:7){  
    # keyword_1=xingtaixue[jj,1]
    keyword_1=aa[jj,1]
    keyword_2=aa[jj,2]
    #keyword_1=xingtaixue[errRes[jj,1],1]
    #keyword_2=degree[errRes[jj,2],1]
    tryCatch({
      willeyFullText(keyword_1,keyword_2)
      #if(!is.na(Lit0)){
      # Lit<-rbind(Lit,Lit0)
      #   Lit0<-as.ffdf(Lit0)
      # Lit<- ffdfappend(Lit,Lit0, adjustvmode=F)
      print(paste(c('ii:',ii,'/31;jj:',jj,'/23'), collapse=''))
      #}
    },error=function(e){
      kk<<-kk+1;
      errRes[kk,1]<<-ii;
      errRes[kk,2]<<-jj;
      print(e)
    })
    
    
  }


