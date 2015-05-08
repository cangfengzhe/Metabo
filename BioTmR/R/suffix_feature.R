suffix_feature <- function(data, num = c(3,4,5), bind = T){
  data <- check_feature(data)

  out <- sapply(num, function(ii){
    word_len <- nchar(data[,1]);
    substr(data[,1], word_len-ii+1, word_len)
  })
  out <- as.data.frame(out, stringsAsFactors = F);
  if(bind){
    out <- cbind(data, out)
  }else{
    out <- cbind(data[,1], out)
  }
  out
}