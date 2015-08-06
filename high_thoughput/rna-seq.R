

library(rtracklayer)
library(easyRNASeq)

library(airway)
dir <- system.file("extdata", package="airway", mustWork=TRUE)
list.files(dir)
sampleTable <- read_csv( file.path(dir,"sample_table.csv"))
filenames <- file.path(dir, paste0(sampleTable$Run, "_subset.bam"))

library("Rsamtools")
indexBam(filenames)
bamfiles <- BamFileList(filenames, yieldSize=2000000, index = character())

library("GenomicFeatures")
gtffile <- file.path(dir,"Homo_sapiens.GRCh37.75_subset.gtf")
(txdb <- makeTranscriptDbFromGFF(gtffile, format="gtf"))
genes <- exonsBy(txdb, by="gene")


annotParam <- AnnotParam(
  datasource=gtffile, type = 'gtf')


rnapara <- RnaSeqParam(annotParam = annotParam,
                       bamParam = BamParam(paired = T),
                       countBy =  "genes", precision = "read")

sortBam(filenames, paste(filenames, 'sort', sep = '_'))

sort_names <- sapply(filenames, function(x){
  sortBam(x, paste(x, 'sort', sep = '_'))
})

indexBam(sort_names)

bamFiles <- getBamFileList(sort_names)



aa <- simpleRNASeq(bamFiles, 
                   param = rnapara,
                   nnodes = 2,
                   verbose = T)


assay(aa)




# ---------------------

bamFiles <- getBamFileList(
  dir(path=system.file("extdata",
                       package="RnaSeqTutorial"),
      pattern="^[A,T].*\\.bam$",
      full.names=TRUE))

annotParam <- AnnotParam(system.file(
  "extdata",
  "Dmel-mRNA-exon-r5.52.gff3",
  package="RnaSeqTutorial"))

rnaSeqParam <- RnaSeqParam(annotParam=annotParam)

sexp <- simpleRNASeq(
  bamFiles=bamFiles,
  param=rnaSeqParam,
  verbose=TRUE
)


# ---------

mcf7 <- c('./raw_data/Mcf7CellPapAlnRep1.bam', './raw_data/Mcf7CellPapAlnRep2.bam')
indexBam(mcf7)
mcf7_sort <- sapply(mcf7, function(x){
  sortBam(x, paste(x, 'sort', sep = '_'))
  
})

indexBam(mcf7_sort)
bamFiles <- getBamFileList(mcf7_sort)



aa <- simpleRNASeq(bamFiles, 
                   param = rnapara,
                   nnodes = 2,
                   verbose = T)
assay(aa)
