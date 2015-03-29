# download files about cellline z-score in GDSC database

linkTxt <- readLines("D:/deskTop/cellLine.txt")
head(linkTxt) %>% cat()

# extract all single line including we need ----
allText <- paste(linkTxt, collapse = " ")
pattern <- "<tr class=.*?(?=</tr>)"
outLine <- regmatches(allText, gregexpr(pattern, allText, perl = T))
outLinedf <- do.call("cbind", outLine)

# extract ID from the above result
patternID <- "(?<=CellLine/).*?(?=\">)"
cellID <- regmatches(outLinedf, gregexpr(patternID, outLinedf, 
    perl = T))

# download the file about z-score based the above cellID from
# database----

# get the corresponding url and path
preurl <- "http://www.cancerrxgene.org/translation/CellLine_Download_ZScore/"
urlList <- sapply(cellID, function(x) {
    url <- paste(c(preurl, x), collapse = "")
})

pathList <- sapply(cellID, function(x) {
    path <- paste(c("./zscore/", x, ".csv"), collapse = "")
})

# download the file and save to the directory './zscore'
sapply(seq_along(cellID), function(x) {
    download.file(urlList[x], pathList[x])
    print(x)
})
# write to the sqlite ---- in addition, zscore.db$con is equal
# to the return from dbConnect function
zscore.db <- src_sqlite("./data/ic.db", create = T)
sapply(pathList, function(x) {
    icData <- read.csv(x, stringsAsFactors = F)
    dbWriteTable(zscore.db$con, name = "ic50", icData, append = T)
    print(x)
})
ic50 <- tbl(zscore.db, "ic50")
head(ic50, 3)
# db_list_tables(zscore.db$con)
# db_drop_table(zscore.db$con,'ic50')

# process the mutation data----- import the mutation data from
# GDSC db
mutRaw <- read.csv("./data/gdscMutation.csv", stringsAsFactors = F)
mut <- mutRaw[, -c(2:5)]
View(mutation)
# extract the mutation position and gene from mutRaw3 regIndex
# <- grep (pattern = '.*p\\..*', mutRaw[x,]) regResult <-
# mutRaw[x,regIndex]
library(reshape2)
mutMelt <- melt(mut, id.vars = 1)
mutTbl <- as.tbl(mutMelt)
grepl("xx", c("xxafds", "dfs"))
mutFilter <- filter(mutTbl, grepl(".*p\\..*", value))
mutFilter <- as.data.frame(mutFilter)
library(splitstackshape)
mutSplit <- cSplit(mutFilter, splitCols = 3, sep = "::", direction = "wide")
mutSplit <- cSplit(mutSplit, splitCols = 3, sep = "|", direction = "long")

# export to csv

write.csv(mutSplit, file = "./data/procMutation.csv")

# exp(ic50 value)

ic50 <- read.csv("./data/database//InfoIC50.csv")
View(ic50)
value <- ic50[, 4:7]
valueExp <- exp(value)
View(valueExp) 
