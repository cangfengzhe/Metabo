X1<-rep(c(0,0,1,1,0,0,1,1),c(63,63,44,265,136,107,57,151))
X2<-rep(c(0,1,0,1,0,1,0,1),c(63,63,44,265,136,107,57,151))
y<-rep(c(1,1,1,1,0,0,0,0),c(63,63,44,265,136,107,57,151))
tcl<-data.frame(X1,X2,y)
fit<-glm(y~X1+X2,data=tcl,family='binomial')
sum<-summary(fit)
#计算优势比
sum$coefficients[2:3,1]
OR<-exp(sum$coefficients[2:3,1])
OR


#多远线性回归
data<-read.csv('E:\\code\\xutang.csv')
attach(data)
sum ((x1-mean(x1))*(x2-mean(x2)))
Lxx=data.frame(x1=c(3152.9359,1601.3205,1813.8103),x2=c(1601.3205,2615.397,1138.2987),x3=c(1813.8103,1138.29870,1384.5344)
)
Lxx
Lxx<-t(Lxx)
Lxy=data.frame(17598.1090,12920.4551,11192.9526)
Lxy<-t(Lxy)
solve(Lxx)
solve(Lxx)%*%Lxy  #计算偏相关系数

#通径分析
x1<-c(59,63,63,64,63,61,65,62,63,57,60,60,67,68,66,60,69,58,65,69)
x2<-c(71,73,73,82,74,75,74,68,74,67,71,70,77,73,77,80,76,70,83,88)
y<-c(14,15,17,21,20,18,17,17,21,14,16,14,18,17,20,19,21,14,23,24)
data<-data.frame(x1,x2,y) #列与列间求协方差矩阵
data
lm<-lm(y~x1+x2)
summary(lm)
pianhuigui(x2,x2)

# 离差阵
pianhuigui<-function(x1,x2){
  y=sum((x1-mean(x1))*(x2-mean(x2)));
  y
}

r<-cor(data[-3])
x1x2<-cor(x1,x2)
x1y<-cor(x1,y)
x2y<-cor(x2,y)
Rxy<-data.frame(x1y,x2y)
Rxy<-t(Rxy)
data<-as.matrix(data)
solve(r)%*%Rxy

install.packages('agricolae')
library(agricolae)

使用包计算 偏相关系数..
install.packages('corpcor')
library('corpcor')
cor<-cor(data)
cor2pcor(cor)
#验证
cor1<-solve(cor)
cor1[1,2]/sqrt(cor1[1,1]*cor1[2,2])