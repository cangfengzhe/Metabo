prefix_feature <- function(data, num = c(3,4,5), bind = T){
  data <- check_feature(data)
  out <- sapply(num, function(ii){
    substring(data[,1], 1, ii)
  })

  if(bind){
    out <- cbind(data, out)
  }else{
    out <- cbind(data[,1], out)
  }
 out
}