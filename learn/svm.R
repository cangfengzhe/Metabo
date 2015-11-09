set.seed(1)
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y==1, ] <- x[y==1, ] + 1

plot(x, col = (3-y))
dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y~., data = dat, kernel = 'linear', cost = 10, scale = F)
plot(svmfit, dat)
summary(svmfit)
svmfit$decision.values
svmfit$index %>% length()
svmfit$SV # 支持向量
svmfit$decision.values #计算出的数值, 根据该数值确定class

# 
svmfit <- svm(y~., data = dat, kernel = 'linear', cost = 0.1, scale = F, decision.values = T, probability = T)
out <- predict(svmfit, dat, decision.values = T, probability = T)

fitted_value <- attributes(out)
summary(out)

class(out)

plot(svmfit, dat)
summary(svmfit)
svmfit$index
tune()
