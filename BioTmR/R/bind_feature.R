## this shows you how to write roxygen comments

#' First line is title
#'
#' This is a brief description.
#'
#' Anything else after the description goes to the Details section.
#'
#' You can write several paragraphs.
#' @param x explanation of \code{x}
#' @param ... explanation of \code{...}
#' @return The value returned by this function.
#' @author Who are you?
#' @seealso \code{\link[tools]{file_ext}}, \code{\link[tools]{file_path_sans_ext}}
#' @references \url{https://github.com/yihui/rmini}
#' @importFrom tools file_ext file_path_sans_ext
#' @export
#' @examples split_filename('foo.bar')

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