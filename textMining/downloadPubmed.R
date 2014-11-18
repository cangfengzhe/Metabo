#采用XML识别来做，效果不好
install.packages('tm')
install.packages('dplyr')
install.packages('ff')
library(tm);
library( XML );
library( dplyr)
library(ff)
downloadPubmed<-function(idList,k){
# example
# http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=17284678,9997&retmode=text&rettype=abstract
preStr<-'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=';
typeStr<-'&retmode=xml&rettype=abstract';
#
#idList<-'17284678,9997';
	url<-paste( c(preStr,idList,typeStr), collapse='')

	xmlFile<-htmlParse( url, encoding="UTF-8", asTree = TRUE);

 #PubmedArticle<-getNodeSet(xmlFile,'//pubmedarticleset')
#PubmedArticle1<-sapply(PubmedArticle, xmlValue);
	pmid <- getNodeSet( xmlFile, '//medlinecitation/pmid')
  pmid1<-sapply(pmid, xmlValue);
	articleTitle <- getNodeSet( xmlFile, '//articletitle');
articleTitle1<-sapply(articleTitle, xmlValue);
	abstract <- getNodeSet( xmlFile, '//abstract');
abstract1 <-sapply(abstract , xmlValue);
	jurTitle <- getNodeSet( xmlFile, '//title');#ArticleTitle
jurTitle1 <-sapply(jurTitle , xmlValue);
  year <- getNodeSet( xmlFile, '//pubdate/year');
year1 <-sapply(year , xmlValue);
if(length(pmid1)==k & length(articleTitle1)==k & length(abstract1) & length(jurTitle1)==k &length(year1)==k)

textCmb <- data.frame(pmid1, articleTitle1, abstract1, jurTitle1, year1);


bgtext<-as.ffdf(textCmb)
}


