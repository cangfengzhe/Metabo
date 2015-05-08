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
wshape_feature <- function(data, normalize = T, bind = T){

  data <- check_feature(data)
  if(normalize){
   out <-  gsub('[A-Z]+','X',data[,1]) %>%
      gsub('[a-z]+','x', .) %>%
       gsub('\\d+', '0', .)%>%
      gsub('[^a-zA-Z0-9]+', '_', .)
    }else{

      out <-   gsub('[A-Z]','X',data[,1]) %>%
          gsub('[a-z]','x', .) %>%
          gsub('\\d', '0', .)%>%
          gsub('[^a-zA-Z0-9]', '_', .)
    }

  if(bind){
    out <- cbind(data, out)
  }else{
      out <- cbind(data[,1], out)
      }

 out

}



