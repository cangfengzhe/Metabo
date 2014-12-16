# 整理文献中得到的甲基化数捐�<U+3E65> time Tue Nov 25 14:34:19
# 2014
library(stringr)
library(RSQLite)
data <- read.csv("methyLit.csv", stringsAsFactors = F)
# View(data)
con <- dbConnect(dbDriver("SQLite"), "methylit.db")


process <- function(ii, colnum) {
    
    # 逐列进行,拆分一列，与其他列组合
    
    
    out <- strsplit(data[ii, colnum], "\\|")[[1]]
    out <- str_trim(out)
    if (out[1] != "") {
        df <- data.frame(id = data[ii, 1], disease = out, cellLines = data[ii, 
            3], drug = data[ii, 4], gene = data[ii, 5], row.names = NULL, stringsAsFactors = F)
        dbWriteTable(con, "methy", df, append = T)
    }
    
    print(ii)
}


# 测试代码
ii = 3
colnum = 2
# end

# 第二�<U+393C><U+3E37>
rowNum = 1:nrow(data)
sapply(rowNum, process, colnum = 2)

# 第三�<U+393C><U+3E37>
data <- dbReadTable(con, "methy")
dbGetQuery(con, "delete from methy")
rowNum = 1:nrow(data)
sapply(rowNum, process, colnum = 3) 
