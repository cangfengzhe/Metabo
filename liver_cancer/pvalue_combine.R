Stouffer.test <- function(p, w) { # p is a vector of p-values
  if (missing(w)) {
    w <- rep(1, length(p))/length(p)
  } else {
    if (length(w) != length(p))
      stop("Length of p and w must equal!")
  }
  Zi <- qnorm(1-p) 
  Z  <- sum(w*Zi)/sqrt(sum(w^2))
  p.val <- 1-pnorm(Z)
  return(c(Z = Z, p.value = p.val))
}

Fisher.test <- function(p) {
  Xsq <- -2*sum(log(p))
  p.val <- pchisq(Xsq, df = 2*length(p), lower.tail = FALSE)
  return(c(Xsq = Xsq, p.value = p.val))
}

p <- c(.01, .2)
Stouffer.test(p = p)  # p-value = 0.017
Fisher.test(p = p)  #
