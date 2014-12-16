
# 超几何检�<U+383C><U+3E63>
molName <- read.csv("./result/checkLiter.csv", stringsAsFactors = F)
View(molName)
library(sqldf)
drug_symptom_count <- as.data.frame(drug_symptom_count)
molName2Sym <- sqldf("select drug_symptom_count.* from molName left join drug_symptom_count\n                   on molName.moleculeName=drug_symptom_count.V1")
molNameUnique <- data.frame(name = unique(molName2Sym[, 1]))
symNameUnique <- data.frame(name = unique(molName2Sym[, 2]), stringsAsFactors = F)
symNameUnique[, 1] <- as.character(symNameUnique[, 1])

molNameAddSymName <- rbind(molNameUnique, symNameUnique)

View(molNameAddSymName)
source("../CJX/pmidCount.R")

library(RSQLite)
drv <- dbDriver(drvName = "SQLite")
conn <- dbConnect(drv, "xiaobai_1023")
dbGetQuery(conn, "delete from litCount")


literatureCount <- function(str) {
    tryCatch({
        keyword <- paste(c("\"", str, "\" [Majr:NoExp]"), collapse = "")
        count <- pmidCount(keyword)
        df <- data.frame(name = str, count = count)
        dbWriteTable(conn, "litCount", df, append = T)
    }, error = function(e) {
        print(e)
    })
    
    print(str)
}



count <- sapply(symNameUnique[, 1], literatureCount)

SymCount <- dbGetQuery(conn, "select * from litCount")
dbGetQuery(conn, "delete from litCount")
View(SymCount)

library(sqldf)
load(".rdata")
View(molName2Sym)

View(molSymCount)
View(SymCount)
hyperData <- sqldf("select * from molName2Sym left join molSymCount\n                 on molSymCount.name=molName2Sym.V1")
hyperData <- sqldf("select * from hyperData left join SymCount\n                 on SymCount.name=hyperData.V2")
View(hyperData)
hyperData <- hyperData[, c(1, 2, 6, 8, 3)]

# 计算超几�<U+393C><U+3E35>
df <- molSymCount1102
AllCount = 2.4e+07
df[, 3] <- as.numeric(df[, 3])
df[, 4] <- as.numeric(df[, 4])
df[, 5] <- as.numeric(df[, 5])
for (ii in 1:nrow(df)) {
    ABCount <- df[ii, 5]
    BCount <- df[ii, 4]
    ACount <- df[ii, 3]
    # mat<-matrix(c(ABCount,ACount-ABCount,BCount-ABCount,AllCount-ACount-BCount+ABCount),2)
    pvalue <- 1 - phyper(ABCount, BCount, AllCount - BCount, ACount)
    df[ii, 6] = pvalue
    
}
molSymCount1102 <- df
View(molSymCount1102)
View(hyperData)

write.csv(molSymCount1102, "./result/molSymCount1102.csv")








# validation
View(lit1000)
View(hyperData)
lit1000 <- read.csv("result/lit1000Result.csv", header = T, stringsAsFactor = F)
View(lit1000)
lit1000 <- lit1000[, c(2, 4, 9)]
lit1000Result <- sqldf("select hyperData.*,lit1000.V9 from lit1000 left join hyperData\n                     on lit1000.V2=hyperData.V1 and lit1000.V4=hyperData.V2")

lit1000Result <- sqldf("select * from lit1000 left join hyperData\n                     on lit1000.V2=hyperData.V1 and lit1000.V4=hyperData.V2")

write.csv(lit1000Result, file = "wenxianFinall.csv")
View(lit1000Result)
# lit1000Result<-lit1000Result[,c(1,2,3,)]
# lit1000Result<-unique(lit1000Result)
nrow(lit1000Result)
lit1000Result[, 7] <- as.numeric(lit1000Result[, 7])
rate <- function(value) {
    pos <- lit1000Result[lit1000Result[, 6] > value, 7]
    neg <- lit1000Result[lit1000Result[, 6] <= value, 7]
    (length(pos) - sum(pos) + sum(neg))/nrow(lit1000Result)
    
    
}
rate(0.01)

aa <- mapply(rate, seq(0.1, 1e-04, -1e-04)) 
