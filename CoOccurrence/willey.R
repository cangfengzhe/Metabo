
library(ff)
library(ffbase)
keyWord_1='drought'
keyWord_2='wheat'
#abstract
willey <- function(keyWord_1,keyWord_2){
  
urlStr_1<-'http://onlinelibrary.wiley.com/advanced/search/results/reentry?scope=allContent&dateRange=allDates&inTheLastList=6&startYear=&endYear=&queryStringEntered=false&searchRowCriteria[0].queryString=';
urlStr_2<-'&searchRowCriteria[0].fieldName=abstract&searchRowCriteria[0].booleanConnector=and&searchRowCriteria[1].queryString=';
#全文索引  urlStr_1<-'http://onlinelibrary.wiley.com/advanced/search/results/reentry?scope=allContent&dateRange=allDates&inTheLastList=6&startYear=&endYear=&queryStringEntered=false&searchRowCriteria[0].queryString=';
#         urlStr_2<-‘&searchRowCriteria[0].fieldName=all-fields&searchRowCriteria[0].booleanConnector=and&searchRowCriteria[1].queryString=’；
&searchRowCriteria[1].fieldName=all-fields&searchRowCriteria[1].booleanConnector=and&searchRowCriteria[2].fieldName=all-fields&searchRowCriteria[2].booleanConnector=and&start=1&ordering=relevancy
litNum<-1;
url<-paste(c(urlStr_1,
             keyWord_1,
             urlStr_2,
             keyWord_2,
             '&searchRowCriteria[1].fieldName=abstract&searchRowCriteria[1].booleanConnector=and&searchRowCriteria[2].fieldName=all-fields&searchRowCriteria[2].booleanConnector=and&start=',
             litNum,'&ordering=relevancy&publicationFacet=journal'),
           collapse='')
webContent<-readLines(url, encoding = "UTF-8")

text <- paste(webContent, collapse = "")  #多行文本连成一行，有利于使用正则表达式
#将 文本按照 ariticle 进行分割，对每一个ariticle进行处理

numStr <- "(?<=There are <em>)[0-9]*?(?=</em> results for)"
num<- regexpResult(numStr, text) #自写函数

#noFoundStr<-'No results found for'
if(num==''){
    NA;  
}
 else if(as.numeric(num)<=20){
  result<-getContent(text)
  keyword<-data.frame(keyword1=rep(keyWord_1,nrow(result)),keyword2=rep(keyWord_2,nrow(result)))
  result<-cbind(keyword,result)
  result<-as.ffdf(result)
  result;
 }
  else{
    num<-as.numeric(num)
    result<-getContent(text)
    pageNum<-as.integer(num/20)
  # print(date())
    for (ii in  1: pageNum ){
      
      litNum<-ii*20+1;
      url<-paste(c(urlStr_1,
                   keyWord_1,
                   urlStr_2,
                   keyWord_2,
                   '&searchRowCriteria[1].fieldName=abstract&searchRowCriteria[1].booleanConnector=and&searchRowCriteria[2].fieldName=all-fields&searchRowCriteria[2].booleanConnector=and&start=',
                   litNum,'&ordering=relevancy&publicationFacet=journal'),
                 collapse='')
      webContent<-readLines(url, encoding = "UTF-8")
      
      text <- paste(webContent, collapse = "")
      result0<-getContent(text)
      #print(nrow(result0))
      result<-rbind(result,result0)  
     #print(date())
    print(paste(ii,'/',pageNum))
    }
  keyword<-data.frame(keyword1=rep(keyWord_1,nrow(result)),keyword2=rep(keyWord_2,nrow(result)))
  result<-cbind(keyword,result)
  result<-as.ffdf(result)
    result;
  }
   
}



## 全文索引

willeyFullText <- function(keyWord_1,keyWord_2){
  
  #urlStr_1<-'http://onlinelibrary.wiley.com/advanced/search/results/reentry?scope=allContent&dateRange=allDates&inTheLastList=6&startYear=&endYear=&queryStringEntered=false&searchRowCriteria[0].queryString=';
  #urlStr_2<-'&searchRowCriteria[0].fieldName=abstract&searchRowCriteria[0].booleanConnector=and&searchRowCriteria[1].queryString=';
  urlStr_1<-'http://onlinelibrary.wiley.com/advanced/search/results/reentry?scope=allContent&dateRange=allDates&inTheLastList=6&startYear=&endYear=&queryStringEntered=false&searchRowCriteria[0].queryString=';
  urlStr_2<-'&searchRowCriteria[0].fieldName=all-fields&searchRowCriteria[0].booleanConnector=and&searchRowCriteria[1].queryString=';
  # 
  urlStr_3<-'&searchRowCriteria[1].fieldName=all-fields&searchRowCriteria[1].booleanConnector=and&searchRowCriteria[2].fieldName=all-fields&searchRowCriteria[2].booleanConnector=and&start=';

             
  litNum<-1;
  url<-paste(c(urlStr_1,
               keyWord_1,
               urlStr_2,
               keyWord_2,
               urlStr_3,
               litNum,'&ordering=relevancy&publicationFacet=journal'),
             collapse='')
  webContent<-readLines(url, encoding = "UTF-8")
  
  text <- paste(webContent, collapse = "")  #多行文本连成一行，有利于使用正则表达式
  #将 文本按照 ariticle 进行分割，对每一个ariticle进行处理
  
  numStr <- "(?<=There are <em>)[0-9]*?(?=</em> results for)"
  num<- regexpResult(numStr, text) #自写函数
  
  #noFoundStr<-'No results found for'
  if(num==''){
    NA;  
  }
  else if(as.numeric(num)<=20){
    result<-getContent(text)
    keyword<-data.frame(keyword1=rep(keyWord_1,nrow(result)),keyword2=rep(keyWord_2,nrow(result)))
    result<-cbind(keyword,result)
    #result<-as.ffdf(result)
    result;
  }
  else{
    num<-as.numeric(num)
    result<-getContent(text)
    pageNum<-as.integer(num/20)
    # print(date())
    for (ii in  1: pageNum ){
      
      litNum<-ii*20+1;
      url<-paste(c(urlStr_1,
                   keyWord_1,
                   urlStr_2,
                   keyWord_2,
                   urlStr_3,
                   litNum,'&ordering=relevancy&publicationFacet=journal'),
                 collapse='')
      webContent<-readLines(url, encoding = "UTF-8")
      
      text <- paste(webContent, collapse = "")
      result0<-getContent(text)
      #print(nrow(result0))
      result<-rbind(result,result0)  
      #print(date())
      print(paste(ii,'/',pageNum,'/',nrow(result0)))
    }
    keyword<-data.frame(keyword1=rep(keyWord_1,nrow(result)),keyword2=rep(keyWord_2,nrow(result)))
    result<-cbind(keyword,result)
    #result<-as.ffdf(result)
    result;
  }
  
}
  
  
  

getContent<-function(text){
  litAllStr <- "<div class=\"citation article.*?</div>"
  litAll <- gregexpResult(litAllStr, text)
  
  titleStr<-'(?<=shape="rect">).*?(?=</a>)';
  title<-regexpResult(titleStr,litAll)
  doiStr<-'(?<=DOI:&nbsp;).*?(?=</p>)'
  doi<-regexpResult(doiStr,litAll);
  timeStr<-'(?<=Article first published online :).*(?=, DOI)'
  time<-regexpResult(timeStr,litAll)
  result<-data.frame(title,doi,time)
  result;
}
