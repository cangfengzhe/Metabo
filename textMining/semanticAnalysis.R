install.packages("koRpus")
library(koRpus)
# set.kRp.env(TT.cmd='C:\\TreeTagger\\bin\\tree-tagger.exe')#useless
# parse the file test.txt,important step
tagged.results <- treetag("test.txt", treetagger = "manual", lang = "en", TT.options = list(path = "C:\\TreeTagger", 
    preset = "en"))
# 'test.txt' indicate the path of file that you need
test <- tagged.results@TT.res
# @ indicate the data.frame,lttr indicate the length of word
attach(test)
stemCompletion(token[wclass == "noun" | wclass == "name"])  # get the stem of words

aa <- readLines("test.txt")

# about the usage of tm package
library(tm)
x <- c("apples", "tired", "had")
stemDocument(pmidCount, language = meta(pmidCount, "language"))
meta(str[[1]])
data("crude")
crude[[1]]
stemDocument(crude[[1]])
data("crude")
## Document access triggers the stemming function (i.e., all other documents
## are not stemmed yet)
tm_map(crude, stemDocument, lazy = TRUE)[[1]]
crude[[2]]
## Use wrapper to apply character processing
data("acq")
tm_term_score(acq[[1]], c("change"))
sapply(acq[1:10], tm_term_score, terms_in_General_Inquirer_categories("Positiv"))
tdm <- TermDocumentMatrix(crude)
inspect(tdm)
str <- pmidCount
str <- as.data.frame(str)
str <- rbind.data.frame(str, str)
to
VCorpus(str)
str <- VCorpus(VectorSource(str))  ##change character to corpus,important step

inspect(str)
dtm <- DocumentTermMatrix(pmidCount)
tdm <- TermDocumentMatrix(str)  # Matrix
inspect(tdm)
stemDocument(c("α2,6-sialylated", "stemming", "glycans", "doing"))
stemDocument(pmidCount)
NGramTokenizer(pmidCount)
dtm <- DocumentTermMatrix(str)
inspect(dtm)
aa <- tm_map(str, stemDocument, lazy = TRUE)
inspect(aa)[[1]]
inspect(str)  # view the  matrix
aa <- MC_tokenizer(str) 
