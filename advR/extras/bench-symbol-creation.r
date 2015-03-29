library(inline)
new_symbol <- cfunction(NULL, "\n  SEXP symbol = install(\"test\");\n  return(symbol);\n")
new_string <- cfunction(NULL, "\n  SEXP symbol = mkString(\"test\");\n  return(symbol);\n")

library(microbenchmark)
microbenchmark(new_symbol(), new_string(), times = 10000, "test", 
    as.name("test")) 
