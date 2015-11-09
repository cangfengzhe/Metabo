# Orthogonal Signal Correction and OPLS

library(pls)

ortho <- function(x, weights, loadings){
  # 减去正交数据
  w.ortho <- loadings - crossprod(weights, loadings)/crossprod(weights) * weights
  t.ortho <- x %*% w.ortho
  p.ortho <- crossprod(x, t.ortho) / c(crossprod(t.ortho))
  out <- x - tcrossprod(t.ortho, p.ortho)
  return(out)
}

proc_train <- function(x, y, ncomp){
  if(!is.matrix(x)) x <- as.matrix(x)
  if(!is.numeric(y)) y <- as.numeric(y)
  out <- list()
  for(ii in 1: ncomp){
    # Xcorr <- x
    input <- cbind(x, y) # 组合x, y作为新的数据
    input <- as.data.frame(input)
    out_pls <- plsr(y ~ ., data = input,
                    ncomp = ii,
                    validation = "none")

    ww <- out_pls$loading.weights[, ii]
    pp <- out_pls$loadings[, ii]
#     w.ortho <- pp - crossprod(ww, pp)/crossprod(ww) * ww
#     t.ortho <- x %*% w.ortho
#     p.ortho <- crossprod(x, t.ortho) / c(crossprod(t.ortho))
#     x <- x - tcrossprod(t.ortho, p.ortho)
    x <- ortho(x, ww, pp)
    # 保存ww和pp, 做为测试集使用
    para <- list(ww = ww, pp = pp)
    out[[ii]] <- para
    var_name <- paste(c('comp',ii), collapse = '')
    names(out)[ii] <- var_name
    
  }
  out$corr_x <- x
  out$ncomp <- ncomp
  dat <- cbind(y, out$corr_x)
  names(dat)[1] <- 'y'
  attr(dat, 'para') <- out
  return(dat)
}


proc_test <- function(train_data, x){
  if(!is.matrix(x)) x <- as.matrix(x)
  para <- attr(train_data, 'para')
  ncomp <- para$ncomp
  print(ncomp)
  for(ii in 1: ncomp){
    x <- ortho(x, para[[ii]]$ww, para[[ii]]$pp)
  }
  return(x)
}


crossval <- function(x, y, type = c('CV', 'LOO'), cv_num=10, ncomp=5){
  # x: 指标
  # y: 分类
  if(!is.factor(y)) y <- as.factor(y)
  type <- match.arg(type)
  row_num <- nrow(x)
  
  if(type == 'CV'){
    # 交叉验证
    samp <- sample(1:row_num, row_num, replace=F)
    val_num <- floor(row_num/cv_num)
    test_pre <- ldply(1: cv_num, .progress = 'text', function(ii){
      test_n <- samp[((cv_num -1)*val_num +1) : (cv_num*val_num)]
      test_data_y <- y[test_n]
      test_data_x <- x[test_n,]
      train_data_x <- x[-test_n, ]
      train_data_y <- y[-test_n]
      train_data <- proc_train(train_data_x, train_data_y, ncomp)
      test_data_x <- proc_test(train_data, test_data_x)
      train_data_pls <- plsda(train_data_x, train_data_y, ncomp = ncomp, probMethod = "Bayes")
      test_data_y_pre <- predict(train_data_pls, test_data_x)
      data.frame(real=test_data_y, pre = test_data_y_pre)
    })
    table(test_pre$real, test_pre$pre)
  }else {
    # LOO
    test_pre <- sapply(1:row_num, function(ii){
      test_data_x <- x[ii, ]
      test_data_y <- y[ii,]
      train_data_x <- x[-ii,]
      train_data_y <- y[-ii]
      train_data_y <- proc_train(train_data_x, train_data_y, ncomp)
      test_data_x <- proc_test(train_data, test_data_x)
      train_data_pls <- plsda(train_data_x, train_data_y, ncomp = ncomp, probMethod = "Bayes")
      test_data_y_pre <- predict(train_data_pls, test_data_x)
      
    })
    
    table(y, test_pre)
  }
  
}


# main example

dat <- read.csv('./data/Neg.csv')
rownames(dat) <- dat[,1]
x <- dat[, -c(1, 2)]
y <- as.factor(dat[, 2])
levels(y) <- c(0, 1)
y <- as.numeric(as.character(y))

# odd <- (1:ceiling(nrow(dat)/2))*2-1
# 
# train_data <- proc_train(x, y, ncomp = 5)
# # train_data <- proc_train(x, y, ncomp = 5)
# 
# test_data <- proc_test(x, train_data)
# 
# library(caret)
# useBayes   <- plsda(train_data[, -1], train_data[, 1] %>% as.factor(), ncomp = 5,
#                     probMethod = "Bayes")
# 
# pre <- predict(useBayes, test_data)
# 
# confusionMatrix(predict(useBayes, test_data),
#                 y)

library(pls)
library(caret)
crossval(x, y, type = 'LOO')



