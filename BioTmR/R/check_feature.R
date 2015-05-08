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

