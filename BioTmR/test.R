

train_data <- read.delim('./train.data', sep = ' ', blank.lines.skip = F, stringsAsFactors = F, header = F)

aa <- crf_train(train_data, template_data = './template')
test_data <- read.delim('./test.data', sep = ' ', blank.lines.skip = F, stringsAsFactors = F, header = F)

test_aa <- crf_test(test_data = test_data,model_path = aa, label = F)
data <- suffix_feature(test_data)
sapply(1:ncol(data), function(x){
  class(data[,x])
})
aa <- bind_feature(data, cols=2:4, pos=-2:0)
test_bb <- crf_test(test_data = test_data,model_path = aa, label = T)
View(test_aa)
