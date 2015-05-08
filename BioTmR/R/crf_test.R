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
#' @useDynLib BioTmR
crf_test <- function(test_data, model_path='model.out' , nbest = '0', cost_factor = '1', label = F){
  if(!is.data.frame(test_data) & is.matrix(test_data)) stop('test_data must be data.fram or matrix.')
  if(!file.exists(model_path)) stop('the model file does not exist.')
  if(label){
    label_data <-  test_data[,ncol(test_data)] %>% as.data.frame()
    test_data <- test_data[, -ncol(test_data)]
  }
  test_path <- paste(c(tempdir(), 'test_data_', rnorm(1)), collapse = '')
  write.table(test_data, test_path, row.names = F, na="", quote = F, col.names = F)

  output_path <- paste(c(tempdir(), 'crf_test_out_data_', rnorm(1)), collapse = '')
  crf_test_(test_path, model_path, output_path, nbest, cost_factor)
 if(file.info(output_path)$size == 0) stop('error: input test data is invalid.')
  out <- read.delim(output_path, sep = '\t', blank.lines.skip = F, stringsAsFactors = F, header = F)

   if(label){
     cbind(out,label_data)
     }else{
      out
     }
}