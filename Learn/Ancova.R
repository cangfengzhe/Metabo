# 协方差分析
rm(list = ls())
li2.6 <- read.table("E:\\code\\bioSta\\li2.2.6.txt")
detach()
attach(li2.6)
f <- as.factor(f)
fit <- lm(y ~ f + x + f * x, li2.6)
a <- anova(fit)  #只看交互项 f:x,p>0.05说明fx没有交互作用，差异不显著，即平行，平行就会有共斜率
# 进行离回归分析
lxx <- sum((x[f == 1] - mean(x[f == 1]))^2)  #  x1离差平方和
+sum((x[f == 2] - mean(x[f == 2]))^2) + sum((x[f == 3] - mean(x[f == 3]))^2)
lxy <- sum((x[f == 1] - mean(x[f == 1])) * (y[f == 1] - mean(y[f == 1]))) + 
    sum((x[f == 2] - mean(x[f == 2])) * (y[f == 2] - mean(y[f == 2]))) + 
    sum((x[f == 3] - mean(x[f == 3])) * (y[f == 3] - mean(y[f == 3])))
b = lxy/lxx  #共斜率
f1 <- y[f == 1] - b * (x[f == 1] - mean(x))
y[f == 1] - b * (x[f == 1] - mean(x))
f2 <- y[f == 2] - b * (x[f == 2] - mean(x))
f3 <- y[f == 3] - b * (x[f == 3] - mean(x))
li2.60 <- data.frame(f0 = c(f1, f2, f3), factor = f)
attach(li2.60)
aov(f0 ~ f, data = li2.60)
li2.60
model <- aov(f0 ~ f, li2.60)
summary(model)
li2.60
f0
data.frame(f1, f2, f3)
x 
