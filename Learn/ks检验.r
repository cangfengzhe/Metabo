function (x, y, ..., alternative = c("two.sided", "less", "greater"), 
          exact = NULL) 
{
  alternative <- match.arg(alternative) #匹配参数
  DNAME <- deparse(substitute(x))  #deparse 将命令转换成字符串  #substitute 替换命令
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 1L) 
    stop("not enough 'x' data")
  PVAL <- NULL
  if (is.numeric(y)) {  #如果y是数值型变量
    DNAME <- paste(DNAME, "and", deparse(substitute(y)))  #paste 用于连接字符串，
    y <- y[!is.na(y)]
    n.x <- as.double(n)  #转换为双精度
    n.y <- length(y)
    if (n.y < 1L) 
      stop("not enough 'y' data")
    if (is.null(exact)) 
      exact <- (n.x * n.y < 10000) #判断成绩是否小于1000
    METHOD <- "Two-sample Kolmogorov-Smirnov test"
    TIES <- FALSE
    n <- n.x * n.y/(n.x + n.y)  #x，y的积除以和
    w <- c(x, y)  #合并两个向量
    z <- cumsum(ifelse(order(w) <= n.x, 1/n.x, -1/n.y))  #累计加和 ，order返回w从小到大的顺序，如果返回值小于等于x的个数
    if (length(unique(w)) < (n.x + n.y)) {    #unique 去掉重复值
      if (exact) {
        warning("cannot compute exact p-value with ties")  #因为有结不能精确计算p-value
        exact <- FALSE 
      }
      else warning("p-value will be approximate in the presence of ties")
      z <- z[c(which(diff(sort(w)) != 0), n.x + n.y)] #diff a2-a1+a3-a2+a4-a3 which 返回正确的数值的序号
      TIES <- TRUE  #有结
    }
    STATISTIC <- switch(alternative, two.sided = max(abs(z)), 
                        greater = max(z), less = -min(z))
    nm_alternative <- switch(alternative, two.sided = "two-sided", 
                             less = "the CDF of x lies below that of y", greater = "the CDF of x lies above that of y")
    if (exact && (alternative == "two.sided") && !TIES) 
      PVAL <- 1 - .Call(C_pSmirnov2x, STATISTIC, n.x, n.y)
  }
  else {
    if (is.character(y))   #y是字符型变量
      y <- get(y, mode = "function", envir= parent.frame())  #搜索函数，返回函数命令，可以用y来代替 返回函数进行运算
    if (!is.function(y)) 
      stop("'y' must be numeric or a function or a string naming a valid function")
    METHOD <- "One-sample Kolmogorov-Smirnov test"
    TIES <- FALSE
    if (length(unique(x)) < n) {
      warning("ties should not be present for the Kolmogorov-Smirnov test")
      TIES <- TRUE
    }
    if (is.null(exact)) 
      exact <- (n < 100) && !TIES  #x个数小于100 并且没有结可以进行精确计算
    x <- y(sort(x), ...) - (0:(n - 1))/n #做差？？
    STATISTIC <- switch(alternative, two.sided = max(c(x, 
                                                       1/n - x)), greater = max(1/n - x), less = max(x))
    if (exact) {
      PVAL <- 1 - if (alternative == "two.sided") 
        .Call(C_pKolmogorov2x, STATISTIC, n)
      else {
        pkolmogorov1x <- function(x, n) {
          if (x <= 0) 
            return(0)
          if (x >= 1) 
            return(1)
          j <- seq.int(from = 0, to = floor(n * (1 - 
                                                   x)))
          1 - x * sum(exp(lchoose(n, j) + (n - j) * log(1 - 
                                                          x - j/n) + (j - 1) * log(x + j/n)))
        }
        pkolmogorov1x(STATISTIC, n)
      }
    }
    nm_alternative <- switch(alternative, two.sided = "two-sided", 
                             less = "the CDF of x lies below the null hypothesis", 
                             greater = "the CDF of x lies above the null hypothesis")
  }
  names(STATISTIC) <- switch(alternative, two.sided = "D", 
                             greater = "D^+", less = "D^-")
  if (is.null(PVAL)) {
    pkstwo <- function(x, tol = 1e-06) {
      if (is.numeric(x)) 
        x <- as.double(x)
      else stop("argument 'x' must be numeric")
      p <- rep(0, length(x))
      p[is.na(x)] <- NA
      IND <- which(!is.na(x) & (x > 0))
      if (length(IND)) 
        p[IND] <- .Call(C_pKS2, p = x[IND], tol)
      p
    }
    PVAL <- ifelse(alternative == "two.sided", 1 - pkstwo(sqrt(n) * 
                                                            STATISTIC), exp(-2 * n * STATISTIC^2))
  }
  PVAL <- min(1, max(0, PVAL))
  RVAL <- list(statistic = STATISTIC, p.value = PVAL, alternative = nm_alternative, 
               method = METHOD, data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}
<bytecode: 0x000000000a3c1ca8>
  <environment: namespace:stats>
  