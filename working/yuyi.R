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
