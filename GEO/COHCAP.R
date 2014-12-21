library(COHCAP)

dir <- system.file("extdata", package="COHCAP")
beta.file <- file.path(dir,"GSE42308_truncated.txt")
sample.file <- file.path(dir,"sample_GSE42308.txt")
expression.file <- file.path(dir,"expression-Average_by_Island_truncated.txt")
project.folder <- getwd()
project.name <- "450k_avg_by_island_test"

beta.table <- COHCAP.annotate(beta.file, project.name, project.folder,
                              platform="450k-UCSC")


COHCAP.qc(sample.file, beta.table, project.name, project.folder)
