# connect to the usuc though mysql

usuc <- src_mysql(host = 'genome-mysql.cse.ucsc.edu', user = 'genomep', password = 'password' )

conn <- dbConnect(MySQL(), host = 'genome-mysql.cse.ucsc.edu', user = 'genomep', password = 'password' )

View(cor_ensg)
View(cor_ensg_essen)
