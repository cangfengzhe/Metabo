rm(list=ls())
ex<-read.table("E:\\ex.txt",header=T);#读取ex数据
attach(ex)#将数据框写入内存
save(ex,file="E:\\ex.Rdata");#将工作空间保存

ks.test(x1,x2)
#对各变量进行结构与关系分析

#定义 偏度系数 峭度系数 函数
skewness<-function (x, na.rm = FALSE) {
  if (na.rm) 
    x <- x[!is.na(x)]
  sum((x - mean(x))^3)/(length(x) * sd(x)^3)
}#计算样本偏度系数；
kurtosis<- function (x, na.rm = FALSE) 
{
  if (na.rm) 
    x <- x[!is.na(x)]
  sum((x - mean(x))^4)/(length(x) * var(x)^2) - 3
}#计算样本峭度系数。
boxplot(ex[,1:5]) #前5个变量的箱型图
colNum=4; #colNum表示数据框ex的列标
newData<-ex[,colNum];
summary(newData) #样本概述 
var(newData)   #方差
sd(newData)   #标准差
fivenum(newData) #五数(最小值，四分之一分位数，中位数，四分之三分位数，最大值)
skewness(newData, na.rm = FALSE) #样本偏度系数
kurtosis(newData, na.rm = FALSE) #样本峭度系数
hist(newData,freq=F,main=paste(names(ex)[colNum],"的直方图"),labels=T,xlab=names(ex)[colNum],border='blue')
lines(density(newData, bw = "nrd0"),col="red")#在直方图中，加入密度曲线
stem(newData) #茎叶图

plot(ecdf(newData), do.points=FALSE, verticals=TRUE)#绘制经验分布函数图
lines(newData, pnorm(newData, mean=mean(newData), sd=sd(newData)), lty=3,col="red")#在经验分布函数图上添加理论分布函数；
qqnorm(newData,main=paste(names(ex)[colNum],"的正态QQ图")); #绘制正态qq图
qqline(newData,col="red")#QQ图相应直线
plot(newData,z,xlab=names(ex)[colNum]) #每列数值与y做趋势图
qqplot(y,z)#每列数值与y做qq图
qqline(newData,col="red") #QQ图相应直线

#统计检验法,检验样本是否服从正态分布
ks.test(newData, "pnorm",mean=mean(newData),sd=sd(newData)) #if 样本数>500，k-s，else s-w，此处样本数为1000选用K-S检验，
# shapiro.test(newData)




aa
shapiro.test(x2)



#方差齐性检验
for (ii in 1:5){
  for (jj in ii+1:5){
    
  }
}
library(Rcmdr)
leveneTest(x3,group,center=median)

ks.test(x1, "pnorm",mean=mean(x1),sd=sd(x1)) #

#R语言运行环境 R 3.0.3 64bit 
#建立一个有7个以上不同类型变量的数据框,并对其进行相应管理操作
#建立数据框
rm(list=ls()) #清除所有变量
myData<-data.frame( ID=c(1,2,3),
                    Name=c("xiaoA","xiaoB","xiaoC"),
                    Sex=c("M","M","F"),
                    Age=c(22,23,21),
                    Weight=c(68.032,65.332,50.123),
                    Height=c(173,168,160),
                    Score=c(98.99,95.25,90.15))
save(myData,file="E:\\ex.Rdata");#将工作空间保存
#对数据框进行操作
#选取特定行
attach(myData)#对表中变量进行选择需使用 attach（）；
#如果不用attach()，需要用$提取数据框内某一列数据。
myData[1,] #选择第一行
myData[-1,]#选择第一行以外的其他行
myData[Height>170,]#选择身高大于170的行

#选取特定列
myData[,1] #选择第一列
myData[,-1] #选择第一列以外的其他列
#数据框 合并
newRData<-rbind(myData,myData);#按行合并
newCData<-cbind(myData,myData);#按列合并
#新建数据框
addData<-data.frame( newID=c(2,1,3),
                     Add=c(100,200,300))
attach(addData)
# ID 为myData addData 共有变量，以ID为标准，合并两个数据框
merge(myData,addData,by.x="ID",by.y="newID")
# 根据score 列进行排序，从低到高
myData[order(myData[,7],decreasing=F),]

