library(XML)
pmidCount<-function(str){
  #by Pidong Li
  #2014-08-06
  #?Ȼ?ȡ???????׵???Ŀ
  #  prestr=paste(c('(',str1,' [MeSH Terms] OR ',str1,'[Title/Abstract])','(',str2,' [MeSH Terms] OR ',str2,'[Title/Abstract])'),collapse='')
  # prestr=paste(c('"',str,'" [MeSH Terms] OR "',str,'" [Title/Abstract]'),collapse='')
  #prestr=paste(c(str,' [MeSH Terms] OR ',str,' [Title/Abstract]'),collapse='')
 
    prestr<-str
    
    preurl<-paste(c('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',prestr),collapse='')
    prexmlfile<-htmlParse(preurl,encoding="UTF-8",asTree = TRUE)#??ȡhtml?ļ?
    countNode<-getNodeSet(prexmlfile,"//count");
    pmidCount<-sapply(countNode, xmlValue)
    pmidCount<-as.data.frame(pmidCount,stringsAsFactors = FALSE)
    as.numeric(pmidCount[1,1])->retMax;  
   # print(str)
    retMax
  
}

#  url<-paste(c('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',str,'&RetMax=',retMax,'&RetStart=',retStart),collapse='')

#写入数据库
engine<-
library(RSQLite)
drv<-dbDriver('SQLite');
sqlite<-dbConnect(drv,dbname='sqlit_0920.db')
dbGetQuery(sqlite,'create table error (keyword01 varchar(200),keyword02 varchar(200)')
dbGetQuery(sqlite,'create table comorbidigy (keyword01 varchar(200),keyword02 varchar(200),count int)')
dbDisconnect(sqlite)

pmidCount.2<-function(str01,str02){
  #by Pidong Li
  #2014-08-06
  #?Ȼ?ȡ???????׵???Ŀ
  #  prestr=paste(c('(',str1,' [MeSH Terms] OR ',str1,'[Title/Abstract])','(',str2,' [MeSH Terms] OR ',str2,'[Title/Abstract])'),collapse='')
  # prestr=paste(c('"',str,'" [MeSH Terms] OR "',str,'" [Title/Abstract]'),collapse='')
  #prestr=paste(c(str,' [MeSH Terms] OR ',str,' [Title/Abstract]'),collapse='')
  tryCatch(
    {
      prestr<-paste(c('"',str01,'" [Majr:NoExp] AND "',str02,'" [Majr:NoExp]'))
      
      preurl<-paste(c('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',prestr),collapse='')
      prexmlfile<-htmlParse(preurl,encoding="UTF-8",asTree = TRUE)#??ȡhtml?ļ?
      countNode<-getNodeSet(prexmlfile,"//count");
      pmidCount<-sapply(countNode, xmlValue)
      pmidCount<-as.data.frame(pmidCount,stringsAsFactors = FALSE)
      as.numeric(pmidCount[1,1])->retMax;  
      # print(str)
      print(paste(str01,str02))
      strRes<-paste('insert into comorbidity values("',str01,'","',str02,'","',retMax,'")');
      dbGetQuery(sqlite,strRes);
      
    },error=function(e){
      print(e);
      errStr<-paste(c('insert into error values("',str01,'","',str02,'")'))
      dbGetQuery(sqlite,errStr);
      
    })
 
  
}


