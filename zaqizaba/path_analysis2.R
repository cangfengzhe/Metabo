data(gasoline)
aa <- plsr(octane~. ,data=gasoline, subset = 1:20)
summary(aa)


bb <- plsr(octane~. ,data=gasoline[1:20,])
summary(bb)
identical(aa$scores, bb$scores)
dd <- gasoline$NIR[1:20, ] %>% as.matrix() %*% bb$loading.weights
identical(bb$scores, dd)
bb$scores-dd

class(aa$scores)
class(bb$scores)


row_num <- nrow(gasoline)
gasoline$octane %>% mean
y <- c(rep(1, 30), rep(0, 30))
dat <- cbind(y, gasoline$NIR) %>% as.data.frame()
aa <- plsr(y~., data= dat)


y2 <- class2ind(y %>% as.factor())
colnames(y2) <- c('a', 'b')
dat2 <- cbind(y2, gasoline$NIR) %>% as.data.frame()
tmpData <- data.frame(x=1:60)

tmpData$y <- y2
tmpData$x <- x
bb <- plsr(y~x, data = tmpData)

identical(aa$scores, bb$scores)
summary(aa)
summary(bb)

View(dat)

library(reshape2)
acast(y, y)
