library(pls)
vignette('pls-manual')
data(gasoline)
pls.options(parallel = 4) # 并行, 4核运算
gasTrain <- gasoline[1:50,]
gasTest <- gasoline[51:60,]
gas1 <- plsr(octane ~ NIR, ncomp = 10, data = gasTrain, validation = "LOO")
summary(gas1)
# 选择主成分
plot(RMSEP(gas1), legendpos = "topright")
plot(pls::R2(gas1), legendpos = "topright")
# 验证
plot(gas1, ncomp = 4, asp = 1, line = TRUE)
# 多个主成分图
plot(gas1, plottype = "scores", comps = 1:3)
plot(gas1, plottype = "coef", ncomp=1:3, legendpos = "bottomleft",
          labels = "numbers", xlab = "nm")
# 主成分贡献率
explvar(gas1)

# Loading plot for the gasoline data
plot(gas1, "loadings", comps = 1:2, legendpos = "topleft",
      labels = "numbers", xlab = "nm")
abline(h = 0)

# 预测 
out <- predict(gas1, ncomp =1:3 , newdata = gasTest) # ncomp 采用的主成分数

predplot(gas1, ncomp = 2, newdata = gasTest, asp = 1, line = TRUE)


mvr()
