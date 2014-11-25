cold<-read.csv('cold.csv',header=F,stringsAsFactors=F)
View(cold)
nei<-read.csv('nei.csv',header=F,stringsAsFactors=F)
View(nei)
hot<-read.csv('hot.csv',header=F,stringsAsFactors=F)


#获取文献pmid
kk<-0
neiRes<-matrix(NA,1,1)
n<-0
zz<-NA
errorRes<-NA;
for (ii in 1: nrow(nei)){
  
    molName<-nei[ii,1];
   
    
    tryCatch({
      
      prestr=paste(c(molName,'[title/abstract]'),collapse='')
      #prestr=molName
      pmid<-downloadPmid(prestr);
      if(is.matrix(pmid)){
        kk<-kk+1;
        #pmid0<-paste(pmid,collapse = ',');
        #neiRes[kk,1]<-molName;       
        #neiRes[kk,2]<-nrow(pmid);
        #neiRes[kk,3]<-pmid0;
        tmp=pmid;
        zz[ii]<-nrow(pmid)
        print(molName)
        print(nrow(pmid))
        neiRes<-rbind(neiRes,tmp);
        
        neiRes<-unique(neiRes);
        
        print(paste('kk',kk))
      }
    }  
    ,error=function(e){
      n<<-n+1;
      errorRes[n]<<-ii;
      
      print(e);
    })
    
}
neiRes<-na.omit(neiRes)

#获取文献摘要
library(ff)
library(ffbase)
source('../textMining/regexp.R')
source('../textMining/pubmedText.R')
pmid='';
title='';
abstract='';
journal='';
year='';
result<-data.frame(pmid,title,abstract,journal,year) #存放数据
result0<-as.ffdf(result)  #转换成ff::ffdf格式以硬盘存储数据

#设置连接pubmed的url ，详见3pubmed API
preStr<-'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=';
typeStr<-'&retmode=xml&rettype=abstract';

# ii=1  #设置循环起始位置 
k=20   #设置每次加载的pmid个数
n=0
error0=NA
inputMat<-qizhi2NEI;
for(ii in 1:(as.integer(length(inputMat)/k)+1)){
  tryCatch({
    idList<-paste(inputMat[((ii-1)*k+1):(ii*k)],collapse=','); # 多个pmid以“，”隔开
    url<-paste( c(preStr,idList,typeStr), collapse='')  #  读取的url
    
    result<-pubmedText(url); #调用 pubmedText函数，获得k个文章的内容
    result0<-ffdfappend(result0,result, adjustvmode=F); #将result累加
    print(ii) #打印ii
  },
  error =function(e){   #捕获错误
    n<<-n+1;
    error0[n]<<-ii; #将错误保存到 error0 
    print('chucuole');
  }
  
  )
  
}

#对得到的文献进行正则表达式 匹配
greNEI<-as.data.frame(NEIWenXian,stringsAsFactors=F)
nrow(greNEI)
aa<-grepl('cold',greNEI[,2:3])
paste(c(greNEI[2:5,2],greNEI[2:5,3]))
greNEI[,2]<-as.character(greNEI[,2])
greNEI[,3]<-as.character(greNEI[,3])
class(greNEI[,2])

aa<-apply(X =greNEI[,2:3],'paste',c('collapse'=''))
View(aa)
flag<-NA
coldNum<-matrix(NA,nrow(cold),2)
coldLit<-NA
for(ii in  1: nrow(hot)){
  flag<-grepl(hot[ii,1],greNEI[,3],perl = T)
  
 #flag<-grepl('hot',greNEI[,2])
  index<- which(flag==T)
 coldLit<-rbind(coldLit,greNEI[index,])
  
}
nrow(coldLit)


#联合匹配

errorRes<-matrix(NA,10000,2)
warnRes<-matrix(NA,10000,2)

n<-0;
m<-0;
kk<-0
neiRes<-NA
for (ii in 1: nrow(nei)) {
  for (jj in 1: nrow(hot)){
    molName<-nei[ii,1];
    meshName<-hot[jj,1];
    print(paste('jj',jj))
    
    tryCatch({
      
      prestr=paste(c('"',molName,'" AND "',meshName,'"'),collapse='')
      pmid<-downloadPmid(prestr);
      if(is.matrix(pmid)){
        tmp=pmid;       
        neiRes<-rbind(neiRes,tmp);        
        neiRes<-unique(neiRes);
        kk<-kk+1;
        print(paste('kk',kk))
      }
    }  
    ,error=function(e){
      n<<-n+1;
      errorRes[n,1]<<-ii;
      errorRes[n,2]<<-jj;
      print(e);
    })
    
  }
  print(paste('ii',ii))

}

#NEI与寒症相关文献
NEIColdPmid<-neiRes[,1]
NEIHotPmid<-neiRes[,1]
View(NEIColdPmid)
NEIColdPmid<-as.data.frame(NEIColdPmid)
NEIHotPmid<-as.data.frame(NEIHotPmid)
save(NEIColdPmid,file='NEIColdPmid.rdata')
save(NEIHotPmid,file='NEIHotPmid.rdata')




#下载NEI寒症文献
source('D:/Program Files/RStudio/RFile/textMining/pubmedText.R')
NEIColdLit<-pubmedText(NEIColdPmid)
NEIHotLit<-result0
View(result0)
NEIColdLit<-result0
ffsave(NEIColdLit,file='NEIColdLit')
View(NEIHotLit)
View(NEIColdLit)
ffsave(NEIColdLit,file='NEIColdLit')
ffsave(NEIHotLit,file='NEIHotLit')
ffload(file='NEIHotLit')
rm(NEIHotLit)
#匹配寒症基因

gene<-read.csv('NEIGene.csv',stringsAsFactors=F)
View(gene)



gene0<-gene[gene[,2]!=gene[4,2],]
View(gene0)

geneName='zda'
grepl(paste(c('\\b',geneName,'\\b|\\(',geneName,'\\)'),collapse = ''),'abcd dad dfsadf(zda)',perl=T)

#正则表达式匹配
ii=0
geneLit<-matrix(NA,nrow(gene0),2)
for (geneName in gene0[,2]){
  litBool<-grepl(paste(c('\\b',geneName,'\\b|\\(',geneName,'\\)'),collapse = ''),NEIColdLit[,3],perl=T)
  litIndex<-which(litBool==T)
  ii<-ii+1;
  geneLit[ii,1]<-geneName;
  geneLit[ii,2]<-length(litIndex)
  print(ii)
}
View(NEIColdLit)
library(ff)
ffload(file='NEIHotLit')


#正则表达式匹配所有人类基因
humGene<-read.csv('HuGene.csv',header=F,stringsAsFactors=F)
FilterGene<-read.csv('allFilterGene.csv',header=F,stringsAsFactor=F)
geneLit<-matrix(NA,nrow(FilterGene),2)
ii=0
for (geneName in FilterGene[,1]){
  litBool<-grepl(paste(c('\\b',geneName,'\\b|\\(',geneName,'\\)'),collapse = ''),NEIColdLit[,3],perl=T)
  litIndex<-which(litBool==T)
  ii<-ii+1;
  geneLit[ii,1]<-geneName;
  geneLit[ii,2]<-paste(litIndex,collapse = ',')
  print(ii)
}



#寻找基因两两关系


kk<-0;
ii=0
commom<-matrix(NA,1000,3)
for (ii in 1: nrow(geneLit)){
  aa=strsplit(geneLit[ii,2],',')[[1]]
  for (jj in  (ii+1) : nrow(geneLit)){
    bb=strsplit(geneLit[jj,2],',')[[1]]
    cc=intersect(aa,bb);
    if(length(cc)>0){
      kk<-kk+1;
      commom[kk,1]<-geneLit[ii,1];
      commom[kk,2]<-geneLit[jj,1];
      commom[kk,3]<-length(cc);
    }
  }
}

