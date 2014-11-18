#R语言运行环境： R 3.0.3 64bit，R studio 进行编写

#建立一个有7个以上不同类型变量的数据框,并对其进行相应管理操作

#建立数据框

rm(list=ls());#清除所有变量

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