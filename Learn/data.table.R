library(data.table)
help('data.table')
DT <- data.table(V1=c(1L,2L),
                 V2=LETTERS[1:3], V3=round(rnorm(4),4), V4=1:12)
View(DT)
DT[,.c(1,2)]
