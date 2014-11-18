downloadPmid<-function(str){

  #?Ȼ?ȡ???????׵???Ŀ
  prestr=paste(c(str,' [MeSH Terms] OR ',str,'[Title/Abstract]'),collapse='')
  preurl<-paste(c('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',prestr),collapse='')
  prexmlfile<-htmlParse(preurl,encoding="UTF-8",asTree = TRUE)#??ȡhtml?ļ?
  countNode<-getNodeSet(prexmlfile,"//count");
  pmidCount<-sapply(countNode, xmlValue)
  pmidCount<-as.data.frame(pmidCount,stringsAsFactors = FALSE)
  as.character(pmidCount[1,1])->retMax;
  #??ȡ???? pmid
  
  url<-paste(c('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',prestr,'&RetMax=',retMax),collapse='')
  
  xmlfile<-htmlParse(url,,encoding="UTF-8",asTree = TRUE)#??ȡhtml?ļ?
  nodeName<-getNodeSet(xmlfile,"//id");# get the node and attribute
  Pmid<-sapply(nodeName, xmlValue) #get the attribute
  Pmid<-as.matrix(Pmid)

  print(str)
  Pmid  
}

#  url<-paste(c('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',str,'&RetMax=',retMax,'&RetStart=',retStart),collapse='')

View(word[,1])
write.csv(word,file='word.csv')
