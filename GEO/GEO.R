source("http://bioconductor.org/biocLite.R")
biocLite("GEOquery")

library(GEOquery)
source('http://www.bigre.ulb.ac.be/courses/statistics_bioinformatics/R-files/config.R')
dir.results <- file.path(dir.main, 'results','microarrays','berry_2010')
dir.create(dir.results, showWarnings=FALSE,recurs=TRUE)
dir.figures <- file.path(dir.results, 'figures')
dir.create(dir.figures, showWarnings=FALSE,recurs=TRUE)
export.formats.plots <- c('png', 'pdf')
getGEOfile("GSE19439", destdir=dir.results)

file.training <- file.path(dir.results, "GSE19439.soft.gz")
training.geo <- getGEO(filename=file.training)

file.training <- file.path(dir.results, "GSE19439.soft.gz")
training.geo <- getGEO(filename=file.training)

training.sample.names <- names(GSMList(training.geo))
training.sample.names
training.sample.nb <-length(training.sample.names)
training.sample.nb

Columns(GSMList(training.geo)[[1]])

training.probe.ids <- as.vector(Table(GSMList(training.geo)[[1]])$ID_REF)
training.probe.ids
training.probe.nb <- length(training.probe.ids)
training.probe.nb


dataset <- numeric()
for (x in training.sample.names) {
  dataset <- c(dataset, as.numeric(Table(GSMList(training.geo)[[x]])$VALUE))
}
training.raw <- data.frame(matrix(dataset, training.probe.nb, training.sample.nb))
colnames(training.raw) <- training.sample.names
rownames(training.raw) <- training.probe.ids


dataset <- numeric()
for (x in training.sample.names) {
  dataset <- c(dataset, as.numeric(Table(GSMList(training.geo)[[x]])$Detection))
}
training.detection <- data.frame(matrix(dataset, training.probe.nb, training.sample.nb))
colnames(training.detection) <- training.sample.names
rownames(training.detection) <- training.probe.ids

training.class <- read.table(training.class.file, row.names=1)

summary(training.raw)
c
heatmap(training.raw[,2:ncol(training.raw)])





