newDownload<-function(str){
  
#   prestr=paste(c('"',str,'" [MeSH Terms] OR "',str,'" [Title/Abstract]'),collapse='')
  # prestr=paste(c(str,' [MeSH Terms] OR ',str,' [Title/Abstract]'),collapse='')
  prestr<-str
  basestr<-'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term='
  preurl<-paste(c(basestr,prestr,'&RetMax=10000'),collapse='')
  prexmlfile<-htmlParse(preurl,encoding="UTF-8",asTree = TRUE)#??ȡhtml?ļ?
  countNode<-getNodeSet(prexmlfile,"//count");
  pmidCount<-sapply(countNode, xmlValue)
  
  as.integer(pmidCount[1])->retMax;
  if(retMax==0 ){
    
    return(0)  #return NA
  }
  if(retMax>1000000){
    
    return(NA)  #return NA
  }
  if(retMax<=10000 &  retMax!=0){
    nodeName<-getNodeSet(prexmlfile,"//id");# get the node and attribute
    Pmid<-sapply(nodeName,xmlValue) #get the attribute
          
    
    return(Pmid) 
  }
  if(retMax>10000){
    Pmid<-NA
    NUM<-10000
    for(ii in 1:as.integer(retMax/NUM)){
      RetStart=as.integer((ii-1)*NUM);      
      url<-paste(c('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',prestr,'&RetMax=10000&RetStart=',RetStart),collapse='')
      xmlfile<-htmlParse(url,encoding="UTF-8",asTree = TRUE)#??ȡhtml?ļ?
      nodeName<-getNodeSet(xmlfile,"//id");# get the node and attribute
      tmp<-sapply(nodeName, xmlValue) #get the attribute
     
      start<-(ii-1)*NUM+1;
      
      Pmid[start:(start+length(tmp)-1)]<-tmp
      print(ii)
    }
    return(Pmid)
  }
  
  
}




