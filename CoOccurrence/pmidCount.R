library(XML)
pmidCount<-function(str){
  #by Pidong Li
  #2014-08-06
  #?Ȼ?ȡ???????׵???Ŀ
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
  
  retMax

  
}

#  url<-paste(c('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',str,'&RetMax=',retMax,'&RetStart=',retStart),collapse='')
