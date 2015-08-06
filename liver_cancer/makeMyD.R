conditions <- c(rep(1,228),rep(2,168))
k <- length(unique(conditions))
m <- dim(X)[1]
labels <- 1:m
if (!is.null(dimnames(X)[[1]])) 
  labels <- dimnames(X)[[1]]
p <- choose(m, 2)
D <- matrix(NaN, p, k)
ii <- rep.int(seq_len(m - 1), times = rev(seq_len(m - 1)))
foo <- function(x) {
  return(x:m)
}
jj <- unlist(sapply(2:m, FUN = foo))
# dimnames(D)[[1]] <- mapply(labels[ii], gpsep, labels[jj], 
#                            FUN = paste, sep = "")
# dimnames(D)[[2]] <- paste("Condition", 1:k, sep = "")
dimnames(D)[[1]] <- 1:length(ii)
rownames(D)[1:100] <- 1:100

useBWMC <- T


for (j in 1:k) {
  cases <- t(X[, conditions == j])
  if (useBWMC) 
    casx <- bwmc(cases)
  if (!useBWMC) 
    casx <- cor(cases)
  D[, j] <- casx[lower.tri(casx)]
  if (useBWMC) {
    if (sum(D[, j] == Inf) > 0) 
      stop(paste("Infinite BWMC values detected in condition ", 
                 j, "!\nPlease check for large quantities of non-unique ", 
                 "values, or set useBWMC= to FALSE", sep = ""))
    if (sum(is.na(D[, j])) > 0) 
      stop(paste("0/0 BWMC values detected in condition ", 
                 j, "!\nPlease check for large quantities of non-unique ", 
                 "values, or set useBWMC= to FALSE", sep = ""))
  }
}
return(D)
}
