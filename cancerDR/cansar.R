install.packages("rvest")
library(rvest)
cansarHtml <- html("https://cansar.icr.ac.uk/cansar/cell-lines/browse_by_name/ALL/")
node <- html_node(cansarHtml, ".tablesorter") %>% html_table()
View(node)
data <- node[, c("Name", "Tissue", "Disease")]
View(data)
write.csv(data, "D:/deskTop/cansar.csv") 
