
context_feature <- function(data,col, win, bind = T, ...){
  data <- check_feature(data)
  data[,col] <- as.character(data[,col])
  win_min <- min(win)
  out <- llply(win,..., function(ii){
   print(paste('processing the  window',ii))
    out1 <- ldply(1:length(data[,1]), .progress = 'text', function(jj){

      if((jj+ii)>0) {
        data[jj+ii, col]
      }else{
          data[jj, col]
        }

    })

  })
  out <- do.call('cbind', out)
  if(bind){
    out <- cbind(data, out)
  }else{
    out <- cbind(data[,1], out)
  }
  out
}