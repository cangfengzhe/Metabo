# debug
f <- function(x){
  a <- 1
  b <- 2
  c <- a + b
  
  c+x
}
system.time({
  # a <- sapply(1:30000, f)
  a <-  sapply(1:500000, function(ii){
     # b[ii] <<-
      f(ii)
  })
})

system.time({
  for(ii in 1: 500000){
    b[ii] <- f(ii)
  }
})
system.time({
 c <-  f(1:5000000)
})
head(c)
length(c)
