# download GEO data related to the drug resistance in cancer
library(RSQLite)
# download GDS id ----
source('./downloadGdsid.R')
keyword <- '(("neoplasms"[MeSH Terms] OR cancer[All Fields]) AND "drug resistance"[MeSH Terms] OR ("drug"[All Fields] AND "resistance"[All Fields]) OR "drug resistance"[All Fields]) AND "Homo sapiens"[porgn:__txid9606]';
gdsIDList <- downloadGdsid(keyword)

# connect the database
conn <- dbConnect(drv = SQLite(), ":memory:")
#download summary related to the above id
source('./esummary.R')
fail <- NULL
k <- 20 # the number of id 
num <- as.integer(nrow(gdsIDList)/k)+1
ff <- 0
sapply(1:num, function(ii){
  idList<-paste(gdsIDList[((ii-1)*k+1):(ii*k),1],collapse=',')
  tryCatch({
    url <- paste(c('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=gds&id=',
                   idList, '&retmode=xml&rettype=abstract'), collapse = '')
    esummary(url, conn, 'gdsSummary')
    print(ii)
  }, error=function(e){
    ff <<- ff+1;
    fail[ff] <<- ii
  })
 
})

gds <- dbReadTable(conn, 'gdsSummary')
# filter the result
library(dplyr)
geoData <- filter(gds, grepl('(GSE)|(GDS)', acc))
write.csv(geoData, file='./data/geoData.csv')


#download GEO
library(GEOquery)
getGEOfile('GDS4700', './GeoData')
tempdir()
fai
