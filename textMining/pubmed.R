#主函数，输入文章的pmid（多个以逗号隔开）可以得到文章的题目，摘要，杂志名称 ，摘要
#load('stroke.rdata');
library(ff)
library(ffbase)
source('regexp.R')
source('pubmedText.R')
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
for(ii in 1:(as.integer(length(proteinID)/k)+1)){
  tryCatch({
    idList<-paste(proteinID[((ii-1)*k+1):(ii*k)],collapse=','); # 多个pmid以“，”隔开
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

