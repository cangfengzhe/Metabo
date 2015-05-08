

bind_feature <- function(data, cols, pos, sep = '_', bind = T){
  data <- check_feature(data);

  if(length(cols)!= length(pos)) stop('cols and pos must be corresponding')

  out <- ldply(1:length(data[,1]), .progress = 'text', function(ii){
    bind_word <- sapply(1:length(cols), function(jj){
      pos1 <- ii+pos[jj]
      if(pos1 > 0){
        data[pos1, cols[jj]]
      }else{
        data[ii, cols[jj]]
      }
    })
    paste(bind_word, collapse = sep)

  })
  if(bind){
    out <- cbind(data, out)
  }else{
    out <- cbind(data[,1], out)
  }
 out
}