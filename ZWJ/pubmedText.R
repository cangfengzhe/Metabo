pubmedText <- function(url) {
  # get the article's pmid, title, abstract, journal name and publish date.
  #url
  #url 输入的网址链接
  # e.g. http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=24961184,24961183,24961148,24960992,24960298
              #&retmode=xml&rettype=abstract
  text <- readLines(url, encoding = "UTF-8") 
  #读取网页内容，encoding 不设置会在使用substring()时出现multibite 读取错误
  text <- paste(text, collapse = "")  #多行文本连成一行，有利于使用正则表达式
  #将 文本按照 ariticle 进行分割，对每一个ariticle进行处理
  articleStr <- "<PubmedArticle>.*?</PubmedArticle>"
  article <- gregexpResult(articleStr, text) #自写函数
  
  # pmid
  pmidStr <- "(?<=<PMID Version=\"1\">)[0-9]*?(?=</PMID>)"
  pmid <- regexpResult(pmidStr, article)
  
  # <ArticleTitle>
  titleStr <- "(?<=<ArticleTitle>).*?(?=</ArticleTitle>)"
  title <- regexpResult(titleStr, article)
  
  # </Abstract>
  abstractStr <- "(?<=<Abstract>).*?(?=</Abstract>)"
  abstract <- regexpResult(abstractStr, article)
  abstract <- gsub(pattern = "(<AbstractText.*?>)*(</AbstractText>)*", replacement = "", 
                   x = abstract, perl = T)
  abstract <- gsub(pattern = "\\s+", replacement = " ", x = abstract, perl = T)
  #多个空格替换成一个空格
  
  # <Title>
  journalStr <- "(?<=<ISOAbbreviation>).*?(?=</ISOAbbreviation>)"
  journal <- regexpResult(journalStr, article)
  
  # <PubDate>\n<Year>
  yearStr <- "(?<=<PubDate>).*?(?=</PubDate>)"
  year <- regexpResult(yearStr, article)
  #去除杂质 此处分多次剔除，写在一起出错了，错因不明待查
  year <- gsub(pattern = "(<Year>)*(<Month>)*", replacement = "", x = year, perl = T)
 
  year <- gsub(pattern = "(<Year>)*(</Year>)*(<Month>)*(</Month>)*(<Day>.*</Day>)*(\\s+)", 
               replacement = "", x = year, perl = T)
  year <- gsub(pattern = "(<MedlineDate>)*(</MedlineDate>)*", replacement = "", x = year, 
               perl = T)  
  
  pubmed <- data.frame(pmid, title, abstract, journal, year) #存储
  bgtext <- as.ffdf(pubmed) #转为ffdf格式 （ff package）
}