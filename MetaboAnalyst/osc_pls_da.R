library(pls)
data("gasoline")
odd <- (1: (ceiling(nrow(gasoline)/2)))*2
even <- odd -1

gasoline$NIR <- scale(gasoline$NIR, scale = FALSE,
                      center = colMeans(gasoline$NIR[odd,]))
gasoline.pls <- plsr(octane ~ ., data = gasoline,
                     ncomp = 5, subset = odd,
                     validation = "LOO")
Xtr <- gasoline$NIR
ww <- gasoline.pls$loading.weights[,1]
pp <- gasoline.pls$loadings[,1]
w.ortho <- pp - crossprod(ww, pp)/crossprod(ww) * ww
t.ortho <- Xtr %*% w.ortho
p.ortho <- crossprod(Xtr, t.ortho) / c(crossprod(t.ortho))
Xcorr <- Xtr - tcrossprod(t.ortho, p.ortho)

# 对2nd component 进行处理
gasoline.osc1 <- data.frame(octane = gasoline$octane[odd],# y
                             NIR = Xcorr)
gasoline.opls1 <- plsr(octane ~ ., data = gasoline.osc1,
                        ncomp = 5, validation = "LOO")
pp2 <- gasoline.opls1$loadings[,1]
w.ortho2 <- pp2 - crossprod(ww, pp2)/crossprod(ww) * ww
t.ortho2 <- Xcorr %*% w.ortho2
p.ortho2 <- crossprod(Xcorr, t.ortho2) / c(crossprod(t.ortho2))
Xcorr2 <- Xcorr - tcrossprod(t.ortho2, p.ortho2)

Xtst <- gasoline$NIR[even,]
t.tst <- Xtst %*% w.ortho
p.tst <- crossprod(Xtst, t.tst) / c(crossprod(t.tst))
Xtst.osc1 <- Xtst - tcrossprod(t.tst, p.tst)
gasoline.opls1.pred <- predict(gasoline.opls1,
                              newdata = Xtst.osc1,
                               ncomp = 2)

t.tst2 <- Xtst.osc1 %*% w.ortho2
p.tst2 <- crossprod(Xtst.osc1, t.tst2) / c(crossprod(t.tst2))
Xtst.osc2 <- Xtst.osc1 - tcrossprod(t.tst2, p.tst2)
gasoline.osc2 <- data.frame(octane = gasoline$octane[odd],
                             NIR = Xcorr2)
gasoline.opls2.pred <- predict(gasoline.opls2,
                                newdata = Xtst.osc2, ncomp = 1)

# predict -----
Xtst <- gasoline$NIR[even,]
t.tst <- Xtst %*% w.ortho
p.tst <- crossprod(Xtst, t.tst) / c(crossprod(t.tst))
Xtst.osc1 <- Xtst - tcrossprod(t.tst, p.tst)
gasoline.opls1.pred <- predict(gasoline.opls1,                                 newdata = Xtst.osc1,
           ncomp = 2)


t.tst2 <- Xtst.osc1 %*% w.ortho2
p.tst2 <- crossprod(Xtst.osc1, t.tst2) / c(crossprod(t.tst2))
Xtst.osc2 <- Xtst.osc1 - tcrossprod(t.tst2, p.tst2)
gasoline.opls2 <- plsr(octane ~ ., data = gasoline.osc2,
                        ncomp = 5, validation = "LOO")
gasoline.opls2.pred <- predict(gasoline.opls2,
                             newdata = Xtst.osc2,
                              ncomp = 1)

cor(gasoline.opls2.pred %>% as.numeric(), gasoline$octane[even])
lda

