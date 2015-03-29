
# this function can be replaced by regmatches get the article's
# title ,abstract,journal and pubdate get the string of regular
# expression

gregexpResult <- function(pattern, text) {
    strPos <- gregexpr(pattern, text, perl = T)  #get the start and the length of string 
    
    strStart <- as.numeric(strPos[[1]])  #convert to the numeric
    
    result <- substring(text, strStart, (strStart + attr(strPos[[1]], 
        "match.length") - 1))
}


 
