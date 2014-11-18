species<-read.csv('speciesClass.csv',stringsAsFactors=F)
View(species)
classData<-read.csv('classData.csv',stringsAsFactor=F)
row.names(species)<-species[,1]
species<-species[,2:ncol(species)]

shi<-c(1:11,36,37,39:49,74,75)
xu<-c(25:35,63:73)
he<-c(12:24,50:62)
shiData<-species[,shi]

xuData<-species[,xu]
#heData<-classData[,he]
View(shiData)
t.test(shiData[1,],xuData[1,])
aovRes<-matrix(NA,nrow(classData),9)
colnames(aovRes)<-c('biomarker','aovPvalue','shi-hePvalue','xu-hePvalue','xushiPvalue',
                    'aovPvalue','shi-hePvalue','xu-hePvalue','xushiPvalue')
Nvalue<-matrix(NA,nrow(species),4)
for(ii in 1:nrow(species)){
  Nvalue[ii,1]<-row.names(species)[ii]
  Nvalue[ii,2]<-shapiro.test(as.numeric(shiData[ii,]))$p.value
  Nvalue[ii,3]<-shapiro.test(as.numeric(xuData[ii,]))$p.value
  #Nvalue[ii,4]<-shapiro.test(as.numeric(heData[ii,]))$p.value
  
}
View(Nvalue)
aa<-shapiro.test(as.numeric(shiData[ii,]))
aa
aa$p.value
for(ii in 1:nrow(species)){
  tmp<-data.frame(data=c(as.numeric(shiData[ii,]),as.numeric(xuData[ii,]),as.numeric(heData[ii,])),
                  type=c(rep('shi',length(shiData[1,])),rep('xu',length(xuData[1,])),rep('he',length(heData[1,]))))
  lm<-aov(tmp[,1]~tmp[,2])
  result<-summary(lm)
  pvalue<-result[[1]]$Pr[1]
  aovRes[ii,1]=row.names(classData)[ii]
  aovRes[ii,2]=pvalue;
  tu <- TukeyHSD(lm)
  aovRes[ii,3]<-tu[[1]][1,4]
  aovRes[ii,4]<-tu[[1]][2,4]
  aovRes[ii,5]<-tu[[1]][3,4]
  
  
}

for(ii in c(3,4,12,13,14)){
  tmp<-data.frame(data=c(as.numeric(shiData[ii,]),as.numeric(xuData[ii,]),as.numeric(heData[ii,])),
                  type=c(rep('shi',length(shiData[1,])),rep('xu',length(xuData[1,])),rep('he',length(heData[1,]))))
  aa<-kruskal.test(tmp[,1],tmp[,2])
  aovRes[ii,6]=aa$p.value
  
  
  #shi-he
  tmp<-data.frame(data=c(as.numeric(shiData[ii,]),as.numeric(heData[ii,])),
                  type=c(rep('shi',length(shiData[1,])),rep('he',length(heData[1,]))))
  aovRes[ii,7]=kruskal.test(tmp[,1],tmp[,2])$p.value
  
  #xuhe
  tmp<-data.frame(data=c(as.numeric(xuData[ii,]),as.numeric(heData[ii,])),
                  type=c(rep('xu',length(xuData[1,])),rep('he',length(heData[1,]))))
  aovRes[ii,8]=kruskal.test(tmp[,1],tmp[,2])$p.value
  
  #shi-xu
  tmp<-data.frame(data=c(as.numeric(shiData[ii,]),as.numeric(xuData[ii,])),
                  type=c(rep('shi',length(shiData[1,])),rep('xu',length(xuData[1,]))))
  aovRes[ii,9]=kruskal.test(tmp[,1],tmp[,2])$p.value
  
}
aa<-kruskal.test(tmp[,1],tmp[,2])
plot(lm)


#t检验
t.test
tRes<-matrix(NA,nrow(species),2)
for(ii in 1:nrow(species)){
  tmp<-data.frame(data=c(as.numeric(shiData[ii,]),as.numeric(xuData[ii,])),
                  type=c(rep('shi',length(shiData[1,])),rep('xu',length(xuData[1,]))))
  lm<-t.test(tmp[,1]~tmp[,2])
  tRes[ii,2]<-lm$p.value
  tRes[ii,1]<-row.names(species)[ii]
}
View(tRes)
meanRes<-NA
for(ii in 1:nrow(species)){
  #tmp<-data.frame(data=c(as.numeric(shiData[ii,]),as.numeric(xuData[ii,])),
  #                type=c(rep('shi',length(shiData[1,])),rep('xu',length(xuData[1,]))))
  
  meanShi<-mean(as.numeric(shiData[ii,]))
  meanXu<-mean(as.numeric(xuData[ii,]))
  if(meanXu>meanShi){res<-meanXu/meanShi*-1}
  else{
    res<-meanShi/meanXu
  }
  
  meanRes[ii]<-res
}
names(meanRes)<-row.names(species)
View(meanRes)

View(meanRes)

ggplot(BOD, aes(x = Time, y = demand)) + geom_bar(stat = "identity")


barplot(meanRes,density=0,axisnames=T,cex.axis=1,cex.names=1,
        border=10,names.arg=T,xlim=c(0,10),axis.lty=2)
barplot(meanRes,axisnames=F,xlim=c(0,30))
axis(1, labels =F,at=c(0:15),pos=0)
ggplot(meanRes)
text(3,12, par("usr")[3] - 0.25, srt = 90, adj =0.5,
     labels =names(meanRes) , xpd = TRUE,pos=2)

aa=matrix(NA,16,2)
aa[,1]=names(meanRes)
aa[,2]=meanRes
ggplot(aa,aes(x=time,y=demand)+)
text(1:16, par("usr")[3] - 0.25, srt = 90, adj =5,
     labels =names(meanRes) , xpd = TRUE,pos=2)

axis(1,outer=F, at=1:16,padj=-1,hadj=0.5, lab=names(meanRes),cex.axis = 1,tick=F,pos=0)



rhi1<-data.frame(Species=c("S. fredii", "R. etli", "R. leguminosarum", "Rhizobium sp.", "M. amorphae", "Unknown", "M. mediterraneum"), Total=c(28, 8,7,4,3,3,1))
perc<-paste(round((rhi$Total)/sum(rhi$Total), 2)*100, "%)", sep="")
rhi1barplot<-barplot(rhi$Total, col="steelblue", ylim=c(0, 30), names.arg=c(expression(italic("S. fredii")), expression(italic("R. etli")),expression(italic("R. leguminosarum")),expression(paste(italic("Rhizobium "), "sp.")),expression(italic("M. amorphae")),"Unknown",expression(italic("M. mediterraneum"))),xlab="根瘤菌种类", ylab="种内菌株数", font.lab=2, border="white")
text(cex=0.8,x=rhi1barplot, y=rhi1$Total+1.5, xpd=TRUE, lab=paste(rhi1$Total, perc, sep=" ("))
box(bty="l")




tuData<-meanRes[c(4,7,10)]
barplot(tuData,xlim=c(0,20),ylim=c(-2,3),width=1,lwd=1,
        col=c('gray','gray','gray'))
axis(1,outer=T,pos=0,labels=F)

data<-read.csv('t_test.csv',stringsAsFactors=F)
View(data)
barplot(data[,5],ylim=c(-6,8),width=1,lwd=1,col=c(rep('#F4A460',16),rep('#5CACEE',45)),cex.axis =2)
axis(1,outer=T,pos=0,labels=F)
nrow(data)

pca.score<-read.csv('pca_score.csv',stringsAsFactors=F)
View(pca.score)
row.names(pca.score)<-pca.score[,1]
#pca t.test
pca.t.test<-matrix(NA,49,2)
for(ii in 2:49){
  p<-t.test(pca.score[,ii]~pca.score[,1])$p.value
  pca.t.test[ii,1]<-ii-1;
  pca.t.test[ii,2]<-p
}
View(pca.t.test)
pca.score[,1]<-as.factor(pca.score[,1])
write.csv(pca.t.test,file='pca_t_test.csv')
