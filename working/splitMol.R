pubchem <- file("pubchem.txt", "r")
flag <- 1
line = readLines(pubchem, n = 1)

while (length(line) != 0) {
    
    if (flag == 1) {
        name <- paste(c(line, ".txt"), collapse = "")
        file.create(name)
        flag <- 0
        line <- paste(c(line, "\n"), collapse = "")
        cat(line, file = "temp.txt")
        file.append(name, "temp.txt")
    } else {
        line <- paste(c(line, "\n"), collapse = "")
        cat(line, file = "temp.txt")
        file.append(name, "temp.txt")
        if (line == "$$$$\n") {
            flag <- 1
        }
    }
    line <- readLines(pubchem, n = 1)
    
}
close(pubchem)
library(RODBC)
rExcel <- odbcConnectExcel2007("D:\\lipidong\\¹¤×÷\\anticancer_mol\\fenzi_new.xlsx")
data <- sqlFetch(rExcel, "pubchemÆ¥Åä")
close(rExcel)
file.rename
dir <- dir(pattern = "*.txt")
for (n in 1:length(dir)) {
    newName <- paste(c(substr(dir, 1, nchar(dir) - 4), ".sdf"), collapse = "")
}
data[1, 1]
newName
dir[1]
read.table("Clipboarel", header = "T") 
