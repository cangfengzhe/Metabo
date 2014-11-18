library(XML)
downloadPmid<-function(str){
  #by Pidong Li
  #2014-08-06
#  prestr=paste(c('(',str1,' [MeSH Terms] OR ',str1,'[Title/Abstract])','(',str2,' [MeSH Terms] OR ',str2,'[Title/Abstract])'),collapse='')
# prestr=paste(c('"',str,'" [MeSH Terms] OR "',str,'" [Title/Abstract]'),collapse='')
 #prestr=paste(c(str,' [MeSH Terms] OR ',str,' [Title/Abstract]'),collapse='')
   prestr<-str;
 
  preurl<-paste(c('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',prestr),collapse='')
  prexmlfile<-htmlParse(preurl,encoding="UTF-8",asTree = TRUE)#??ȡhtml?ļ?
  countNode<-getNodeSet(prexmlfile,"//count");
  pmidCount<-sapply(countNode, xmlValue)
  pmidCount<-as.data.frame(pmidCount,stringsAsFactors = FALSE)
  as.numeric(pmidCount[1,1])->retMax;
  if(retMax==0){
    Pmid=NA
    return(Pmid)  #return NA
  }
  if(retMax<=20 &  retMax!=0){
    nodeName<-getNodeSet(prexmlfile,"//id");# get the node and attribute
    Pmid<-sapply(nodeName,xmlValue) #get the attribute
    Pmid<-as.matrix(Pmid)      
    
    return(Pmid) 
  }
  #??ȡ???? pmid
  if(retMax>20) {
  url<-paste(c('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',prestr,'&RetMax=',retMax),collapse='')
  
  xmlfile<-htmlParse(url,encoding="UTF-8",asTree = TRUE)#??ȡhtml?ļ?
  nodeName<-getNodeSet(xmlfile,"//id");# get the node and attribute
  Pmid<-sapply(nodeName, xmlValue) #get the attribute
  Pmid<-as.matrix(Pmid)  

 return(Pmid) 
  }
  
}

#  url<-paste(c('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',str,'&RetMax=',retMax,'&RetStart=',retStart),collapse='')

