# NCBI E-utility Esummary
# write the data to the database
esummary <- function(url, conn, tableName) {
  
  #url 输入的网址链接
  # e.g. http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=gds&id=4700,4474,4399&retmode=xml&rettype=abstract
  text <- readLines(url, encoding = "UTF-8") 
  #读取网页内容，encoding 不设置会在使用substring()时出现multibite 读取错误
  text <- paste(text, collapse = "")  #多行文本连成一行，有利于使用正则表达式
  #将 文本按照 ariticle 进行分割，对每一个ariticle进行处理
  docSumStr <- "<DocSum>.*?</DocSum>"
  docSumReg <- gregexpr(docSumStr, text, perl =T)
  docSum <- regmatches(text, docSumReg)
  docSum <- unlist(docSum) 
  
  # gdsid
  gdsidStr <- "(?<=<Id>)[0-9]*?(?=</Id>)"
  gdsidReg <- gregexpr(gdsidStr, docSum, perl =T)
  gdsid <- regmatches(docSum, gdsidReg)
  gdsid <- unlist(gdsid) 
  titleStr <- '(?<=<Item Name="title" Type="String">).*?(?=</Item>)'
  titleReg <- gregexpr(titleStr, docSum, perl = T)
  title <- regmatches(docSum, titleReg)
  title <- unlist(title)
  accStr <- '(?<=</Id>[\\s]<Item Name="Accession" Type="String">).*?(?=</Item>)'
  accReg <- gregexpr(accStr, docSum, perl =T)
  acc <- regmatches(docSum, accReg)
  acc <- unlist(acc)
 
  sumStr <- '(?<=<Item Name="summary" Type="String">).*?(?=</Item>)'
  sumreg <- gregexpr(sumStr, docSum, perl = T)
  sum <- regmatches(docSum, sumreg)
  sum <- unlist(sum)

  typeStr <- '(?<=<Item Name="gdsType" Type="String">).*?(?=</Item>)'
  typeReg <- gregexpr(typeStr, docSum, perl = T)
  type <- regmatches(docSum, typeReg)
  type <- unlist(type)
  
  df <- data.frame(id = gdsid, acc = acc, title = title, summary = sum, type = type)
  dbWriteTable(conn, name = tableName, df, append = T);
}