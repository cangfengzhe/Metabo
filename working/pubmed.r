library(XML)
word <- read.csv("D:\\DeskTop\\strokeWord.csv")
ii = 1
error<-matrix(data=NA,53,1)
total1 <- matrix("Pmid")
ii=10
for (ii in 10:length(row.names(word))) {
    tmpWord <- as.character(word[ii, 1])
    str <- paste(c(tmpWord, "<-downloadPmid(\"", tmpWord, "\")"), collapse = "")
    tryCatch({
    system.time(
    eval(parse(text = str))
    )
    },  error =function(e) {error[ii]<-word[ii,1]})
    comStr = paste(c("total=rbind(total,", tmpWord, ");"), collapse = "")
    tryCatch({
    eval(parse(text = comStr))
    },  error = function(e) {error[ii]<-word[ii,1]})
    total<-unique(total)
    print(ii)
}
system.time(
      cc<-rbind(aa,bb))
system.time(
      aaa<-rbind(blood,brain))
