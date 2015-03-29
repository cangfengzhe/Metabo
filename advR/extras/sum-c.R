library(inline)


sum1 <- cfunction(signature(x = "numeric"), "\n  double *px = REAL(x);\n  int n = length(x);\n\n  double sum = 0;\n\n  for (R_xlen_t i = 0; i < n; i++) {\n    sum += px[i];\n  }\n\n  return ScalarReal(sum);\n")

sum2 <- cfunction(signature(x = "numeric"), "\n  double *px = REAL(x);\n  int n = length(x);\n\n  double sum = 0;\n\n  for (R_xlen_t i = 0; i < n; i++) {\n    if (ISNA(px[i])) continue;\n    sum += px[i];\n  }\n\n  return ScalarReal(sum);\n")

x <- runif(1000)
stopifnot(all.equal(sum1(x), sum(x)))
stopifnot(all.equal(sum2(x), sum(x)))

library(microbenchmark)
options(digits = 3)
microbenchmark(sum1(x), sum2(x), sum3(x), sum(x)) 
