
library(ff)
library(ffbase)
keyWord_1 = keyword_1
keyWord_2 = keyword_2


## 全文索引 09-14 重新修改，加入drought 进行限制
install.packages("RSQLite")
library(RSQLite)
drv <- dbDriver("SQLite")
sqlite <- dbConnect(drv, dbname = "sqlit0915.db")
dbDisconnect(sqlite)
# dbWriteTable(sqlite,'xingtaixue',Litdf)

dbGetQuery(sqlite, "select count(*) from shenglixue ")
dbGetQuery(sqlite, "delete from  xingtaixue where keyword1=\"opened\"")
dbGetQuery(sqlite, "select distinct keyword1 from xingtaixue ")
aa <- dbGetQuery(sqlite, "select * from errshengliwater")
# dbGetQuery(sqlite,'drop table err')
dbWriteTable(sqlite, "err", aa)
aa <- dbGetQuery(sqlite, "select * from errshengli")
dbGetQuery(sqlite, "create table errshengli (keyword_1 varchar(200),keyword_2 varchar(200),errPage int)")
dbGetQuery(sqlite, "create table logshengli (keyword_1 varchar(200),keyword_2 varchar(200),ii int,pageNum int)")
# nrow(Litdf)= 216832


willeyFullText <- function(keyWord_1, keyWord_2) {
    tryCatch({
        
        
        
        # urlStr_1<-'http://onlinelibrary.wiley.com/advanced/search/results/reentry?scope=allContent&dateRange=allDates&inTheLastList=6&startYear=&endYear=&queryStringEntered=false&searchRowCriteria[0].queryString=';
        # urlStr_2<-'&searchRowCriteria[0].fieldName=abstract&searchRowCriteria[0].booleanConnector=and&searchRowCriteria[1].queryString=';
        urlStr_1 <- "http://onlinelibrary.wiley.com/advanced/search/results/reentry?scope=allContent&dateRange=allDates&inTheLastList=6&startYear=&endYear=&queryStringEntered=false&searchRowCriteria[0].queryString=drought&searchRowCriteria[0].fieldName=abstract&searchRowCriteria[0].booleanConnector=and&searchRowCriteria[1].queryString=\""
        urlStr_2 <- "\"&searchRowCriteria[1].fieldName=fulltext&searchRowCriteria[1].booleanConnector=and&searchRowCriteria[2].queryString=\""
        # 
        urlStr_3 <- "\"&searchRowCriteria[2].fieldName=fulltext&searchRowCriteria[2].booleanConnector=and&start="
        
        
        litNum <- 1
        url <- paste(c(urlStr_1, keyWord_1, urlStr_2, keyWord_2, urlStr_3, 
            litNum, "&ordering=relevancy&publicationFacet=journal"), collapse = "")
        webContent <- readLines(url, encoding = "UTF-8")
        
        text <- paste(webContent, collapse = "")  #多行文本连成一行，有利于使用正则表达式
        # �<U+383C><U+3E36> 文本按照 ariticle
        # 进行分割，对每一个ariticle进行处理
        
        numStr <- "(?<=There are <em>)[0-9]*?(?=</em> results for)"
        num <- regexpResult(numStr, text)  #自写函数
        
        # noFoundStr<-'No results found for'
        if (num == "") {
            print("NA")
        } else if (as.numeric(num) <= 20) {
            result <- getContent(text)
            keyword <- data.frame(keyword1 = rep(keyWord_1, nrow(result)), 
                keyword2 = rep(keyWord_2, nrow(result)))
            result <- cbind(keyword, result)
            # result<-as.ffdf(result)
            flag <- dbWriteTable(sqlite, "shenglixue", result, append = T)
            # result;
        } else {
            num <- as.numeric(num)
            result <- getContent(text)
            keyword <- data.frame(keyword1 = rep(keyWord_1, nrow(result)), 
                keyword2 = rep(keyWord_2, nrow(result)))
            result <- cbind(keyword, result)
            flag <- dbWriteTable(sqlite, "shenglixue", result, append = T)
            logStr <- paste(c("insert into logshengli values (\"", keyWord_1, 
                "\",\"", keyWord_2, "\",", 0, ",", nrow(result), ")"), collapse = "")
            flag <- dbGetQuery(sqlite, logStr)
            pageNum <- as.integer(num/20)
            # print(date())
            for (ii in 1:pageNum) {
                
                litNum <- ii * 20 + 1
                url <- paste(c(urlStr_1, keyWord_1, urlStr_2, keyWord_2, 
                  urlStr_3, litNum, "&ordering=relevancy&publicationFacet=journal"), 
                  collapse = "")
                webContent <- readLines(url, encoding = "UTF-8")
                
                text <- paste(webContent, collapse = "")
                result <- getContent(text)
                # print(nrow(result0)) result<-rbind(result,result0)
                keyword <- data.frame(keyword1 = rep(keyWord_1, nrow(result)), 
                  keyword2 = rep(keyWord_2, nrow(result)))
                result <- cbind(keyword, result)
                # print(date())
                flag <- dbWriteTable(sqlite, "shenglixue", result, append = T)
                logStr <- paste(c("insert into logshengli values (\"", keyWord_1, 
                  "\",\"", keyWord_2, "\",", ii, ",", nrow(result), ")"), 
                  collapse = "")
                flag <- dbGetQuery(sqlite, logStr)
                print(paste(ii, "/", pageNum, "/", nrow(result)))
            }
            
            # result<-as.ffdf(result)
            
            
            # result;
        }
        
    }, error = function(e) {
        print(e)
        str <- paste(c("insert into errshengli values(\"", keyWord_1, "\",\"", 
            keyWord_2, "\",", ii, ")"), collapse = "")
        tmpstr <- dbGetQuery(sqlite, str)
        
    })
    cat(paste(keyWord_1, ",", keyWord_2, "\n"))
    
}



# 'water deficit stress'
dbGetQuery(sqlite, "create table errshengliwater (keyword_1 varchar(200),keyword_2 varchar(200),errPage int)")
dbGetQuery(sqlite, "create table logshengliwater (keyword_1 varchar(200),keyword_2 varchar(200),ii int,pageNum int)")

willeyFullTextwater <- function(keyWord_1, keyWord_2) {
    tryCatch({
        
        
        
        # urlStr_1<-'http://onlinelibrary.wiley.com/advanced/search/results/reentry?scope=allContent&dateRange=allDates&inTheLastList=6&startYear=&endYear=&queryStringEntered=false&searchRowCriteria[0].queryString=';
        # urlStr_2<-'&searchRowCriteria[0].fieldName=abstract&searchRowCriteria[0].booleanConnector=and&searchRowCriteria[1].queryString=';
        urlStr_1 <- "http://onlinelibrary.wiley.com/advanced/search/results/reentry?scope=allContent&dateRange=allDates&inTheLastList=6&startYear=&endYear=&queryStringEntered=false&searchRowCriteria[0].queryString=\"water deficit stress\"&searchRowCriteria[0].fieldName=abstract&searchRowCriteria[0].booleanConnector=and&searchRowCriteria[1].queryString=\""
        urlStr_2 <- "\"&searchRowCriteria[1].fieldName=fulltext&searchRowCriteria[1].booleanConnector=and&searchRowCriteria[2].queryString=\""
        # 
        urlStr_3 <- "\"&searchRowCriteria[2].fieldName=fulltext&searchRowCriteria[2].booleanConnector=and&start="
        
        
        litNum <- 1
        url <- paste(c(urlStr_1, keyWord_1, urlStr_2, keyWord_2, urlStr_3, 
            litNum, "&ordering=relevancy&publicationFacet=journal"), collapse = "")
        webContent <- readLines(url, encoding = "UTF-8")
        
        text <- paste(webContent, collapse = "")  #多行文本连成一行，有利于使用正则表达式
        # �<U+383C><U+3E36> 文本按照 ariticle
        # 进行分割，对每一个ariticle进行处理
        
        numStr <- "(?<=There are <em>)[0-9]*?(?=</em> results for)"
        num <- regexpResult(numStr, text)  #自写函数
        
        # noFoundStr<-'No results found for'
        if (num == "") {
            print("NA")
        } else if (as.numeric(num) <= 20) {
            result <- getContent(text)
            keyword <- data.frame(keyword1 = rep(keyWord_1, nrow(result)), 
                keyword2 = rep(keyWord_2, nrow(result)))
            result <- cbind(keyword, result)
            # result<-as.ffdf(result)
            flag <- dbWriteTable(sqlite, "shenglixuewater", result, append = T)
            # result;
        } else {
            num <- as.numeric(num)
            result <- getContent(text)
            keyword <- data.frame(keyword1 = rep(keyWord_1, nrow(result)), 
                keyword2 = rep(keyWord_2, nrow(result)))
            result <- cbind(keyword, result)
            flag <- dbWriteTable(sqlite, "shenglixuewater", result, append = T)
            logStr <- paste(c("insert into logshengliwater values (\"", keyWord_1, 
                "\",\"", keyWord_2, "\",", 0, ",", nrow(result), ")"), collapse = "")
            flag <- dbGetQuery(sqlite, logStr)
            pageNum <- as.integer(num/20)
            # print(date())
            for (ii in 1:pageNum) {
                
                litNum <- ii * 20 + 1
                url <- paste(c(urlStr_1, keyWord_1, urlStr_2, keyWord_2, 
                  urlStr_3, litNum, "&ordering=relevancy&publicationFacet=journal"), 
                  collapse = "")
                webContent <- readLines(url, encoding = "UTF-8")
                
                text <- paste(webContent, collapse = "")
                result <- getContent(text)
                # print(nrow(result0)) result<-rbind(result,result0)
                keyword <- data.frame(keyword1 = rep(keyWord_1, nrow(result)), 
                  keyword2 = rep(keyWord_2, nrow(result)))
                result <- cbind(keyword, result)
                # print(date())
                flag <- dbWriteTable(sqlite, "shenglixuewater", result, append = T)
                logStr <- paste(c("insert into logshengliwater values (\"", 
                  keyWord_1, "\",\"", keyWord_2, "\",", ii, ",", nrow(result), 
                  ")"), collapse = "")
                flag <- dbGetQuery(sqlite, logStr)
                print(paste(ii, "/", pageNum, "/", nrow(result)))
            }
            
            # result<-as.ffdf(result)
            
            
            # result;
        }
        
    }, error = function(e) {
        print(e)
        str <- paste(c("insert into errshengliwater values(\"", keyWord_1, 
            "\",\"", keyWord_2, "\",", ii, ")"), collapse = "")
        tmpstr <- dbGetQuery(sqlite, str)
        
    })
    cat(paste(keyWord_1, ",", keyWord_2, "\n"))
    
}





getContent <- function(text) {
    litAllStr <- "<div class=\"citation article.*?</div>"
    litAll <- gregexpResult(litAllStr, text)
    
    titleStr <- "(?<=shape=\"rect\">).*?(?=</a>)"
    title <- regexpResult(titleStr, litAll)
    doiStr <- "(?<=DOI:&nbsp;).*?(?=</p>)"
    doi <- regexpResult(doiStr, litAll)
    timeStr <- "(?<=Article first published online :).*(?=, DOI)"
    time <- regexpResult(timeStr, litAll)
    result <- data.frame(title, doi, time)
    result
} 
