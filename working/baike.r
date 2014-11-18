data<-read.csv('D:\\deskTop\\weijing.csv',header=FALSE)
data
fix(data)
data<-as.matrix(data)
data[1,4]==data[1,6]
newData<-matrix(data=NA,4000,2)
n=0;
ii=1;
jj=3;
for(ii in 1:nrow(data) )
  {
  for(jj in 2:48){
    if (!data[ii,jj]==data[1,5]){
      n=n+1;
      newData[n,1]<-data[ii,1];
      newData[n,2]<-data[ii,jj];
     
    }
    else break;
      
  }
}
weijing<-read.csv('D:\\deskTop\\weijing1.csv')
library(XML)
library(tm)
ii=1
weijing<-as.matrix(weijing)
shuxing<-matrix(data=NA,4000,6)
for (ii in 1:nrow(weijing)){
 
prestr=weijing[ii,3]

preurl<-paste(c('http://www.baidu.com/s?ie=UTF-8&wd=',prestr),collapse='')
urlHtml<-readLines(preurl)
prexmlfile<-htmlParse(preurl,encoding="UTF-8",asTree=F)#??取html?募?
countNode<-getNodeSet(prexmlfile,'/html');
baike<-grep("百科", urlHtml)
pmidCount<-sapply(countNode, xmlValue)
baike<-grep("百科", pmidCount)
yao<-grep('药',pmidCount)
zhongyao<-grep('中药',pmidCount)
zhiwu<-grep("植物", pmidCount)
dongwu<-grep("动物", pmidCount)

length(gongxiao)
shuxing[ii,1]<-length(baike);
shuxing[ii,2]<-length(yao);
shuxing[ii,3]<-length(zhongyao);
shuxing[ii,4]<-length(zhiwu);
shuxing[ii,5]<-length(dongwu);
}
length(aa)
regexec
