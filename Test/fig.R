install. packages("vcd")
install.packages('vcd')
library(vcd)
require(vcd)
View(Arthritis)
aa<-table(Arthritis$Age,Arthritis$Sex)
View(aa)
barplot(aa)
aa<-aggregate(state.x77, list(Region = state.region), mean)
View(aa)

View(state.x77)
state.region
View(state.region)
