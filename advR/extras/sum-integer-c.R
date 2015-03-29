sum1 <- cfunction(signature(x = "integer", y = "integer"), "\n  int n = length(x);\n  SEXP z = allocVector(INTSXP, n);\n\n  int *px = INTEGER(x);\n  int *py = INTEGER(y);\n  int *pz = INTEGER(z);\n\n  for (int i = 0; i < n; i++) {\n    pz[i] = px[i] + py[i];\n  }\n\n  return z;")

sum2 <- cfunction(signature(x = "integer", y = "integer"), "\n  int n = length(x);\n  SEXP z = allocVector(INTSXP, n);\n\n  int *px = INTEGER(x);\n  int *py = INTEGER(y);\n  int *pz = INTEGER(z);\n\n  for (int i = 0; i < n; i++) {\n    if (px[i] == NA_INTEGER || py[i] == NA_INTEGER) {\n      pz[i] = NA_INTEGER;\n    } else {\n      pz[i] = px[i] + py[i];\n    }\n  }\n\n  return z;\n")

x <- sample(1000, 10, rep = T)
y <- sample(1000, 10, rep = T)
stopifnot(all.equal(sum1(x, y), x + y))
stopifnot(all.equal(sum2(x, y), x + y))

plus <- function(x, y) x + y

library(microbenchmark)
options(digits = 3)
microbenchmark(sum1(x, y), sum2(x, y), plus(x, y), times = 10000) 
