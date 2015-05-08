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
genia_tagger <- function(file="/Users/lipidong/Desktop/genia.txt",named_entity=T){
  if(missing(file)) stop('please input file name.')
  if(!file.exists(file)) stop('the file does not exist.')
  tryCatch({
    raw_txt <- genia_(file)
  }, error=function(e){
        e
  })

  out <- strsplit(raw_txt, '\\n') %>%
    unlist() %>%
    strsplit('\\t') %>%
    do.call('rbind', .) %>%
    as.data.frame(., stringsAsFactors = F)
  colnames(out) <- c('word', 'word_stem', 'part_of_speech', 'chunk_tags', 'named_entity')

  if(!named_entity) {
    out <- out[,-5]
  }
  out
}

