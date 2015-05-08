

dict_feature <- function(data, dictionary, bind = T){
  data <- check_feature(data)
  if(!is.character(dictionary)) stop('input dictionary data must be  charactor vector')
   dictionary <- unique(dictionary)
  out <- plyr::ldply(1:length(data[, 1]), .progress = 'text', function(ii){
    d1 = length(grep(paste(c('\\b',data[ii, 1],'\\b'), collapse = ''), dictionary, ignore.case = T) )
    d2 = length(grep(paste(c('^', data[ii, 1], '$'), collapse = ''), dictionary, ignore.case = T))

    c(d1,d2)
  })

  colnames(out) <- c('dictionary_part', 'dictionary_exact')

  if(bind){
    out <- cbind(data, out)
  }else{
    out <- cbind(data[,1], out)
  }

}