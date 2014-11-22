library(ff)
library(ffbase)
library(XML)
load('matchWord.rdata')
source('downloadPmid.R')
source('regexp.R')
source('pubmedText.R')
pmid=NA;
title=NA
abstract=NA
journal=NA
year=NA
result<-data.frame(pmid,title,abstract,journal,year) #存放数据
result0<-as.ffdf(result)  #转换成ff::ffdf格式以硬盘存储数据
preStr<-'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=';
typeStr<-'&retmode=xml&rettype=abstract';
wordPmid<-NA
word<-NA
id<-NA
wordPmid<-data.frame(word,id)
wordPmid1<-data.frame(word,id)


nn=0
ee<-0
errorID<-NA
proteinPmid1<-data.frame(word,id)
#合并0和1
proteinPmid<-data.frame(word,id)
proerrorID<-NA;
bb<-0
ii=112
ii=8036

for (ii in 1: length(errorID) )
{ 
  tryCatch({
    
     pmid<-downloadPmid(geneWord[errorID[ii]]);
  
     
     if(is.na(pmid)==TRUE){
       print(ii);
        next};
     if(is.data.frame(pmid)==TRUE){
       n=nrow(pmid)
       wordPmid<-data.frame(word,id);
        wordPmid[1:n,1]<-geneWord[errorID[ii]];
        wordPmid[1:n,2]<-as.character(pmid[1:n,1]);
       wordPmid1<-rbind(wordPmid1,wordPmid);
        print(n);
     }
  },error=function(e){
   mm<<-mm+1;
    print(e);
    errorID1[mm]<<-ii
    })
     
}


for (ii in 1: length(proerrorID) )#length(proteinWord) 
{ 
  tryCatch({
    aa<-proerrorID[ii]
    pmid<-downloadPmid(proteinWord[aa]);
    
    
    if(is.na(pmid)==TRUE){
      print(ii);
      next};
    if(is.data.frame(pmid)==TRUE){
      n=nrow(pmid)
      proteinPmid<-data.frame(word,id);
      proteinPmid[1:n,1]<-proteinWord[aa];
      proteinPmid[1:n,2]<-as.character(pmid[1:n,1]);
     proteinPmid1<-rbind(proteinPmid1,proteinPmid);
      print(n);
     
    }
  },error=function(e){
    bb<<-bb+1;
    print(e);
    proerrorID1[bb]<<-aa})
  
}

