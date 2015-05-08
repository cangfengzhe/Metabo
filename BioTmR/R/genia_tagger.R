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

