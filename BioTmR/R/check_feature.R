check_feature <- function(data){
  if (missing(data))
    stop("no data available in input")

  if(!is.data.frame(data) & !is.character(data) & !is.matrix(data))
    stop('input data must be data.frame, matrix or charactor')

  if(is.character(data) | is.matrix(data))
    data <- as.data.frame(data, stringsAsFactors = F)
  if(!is.character(data[,1])) data[,1] <- as.character(data[,1])
  data
}

