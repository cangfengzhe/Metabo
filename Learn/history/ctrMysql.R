rm(list = ls())
library(DBI)
library(RMySQL)
# conMysql <- dbConnect(dbDriver('MySQL'),host='192.168.137.33', port=3360,
# dbname = 'PostgreSQL', user='allen', password='082508')
conMysql <- dbConnect(dbDriver("MySQL"), dbname = "information_schema", user = "root", 
    password = "12345678")
dbContent = dbGetQuery(conMysql, "select table_name,column_name from columns where table_schema='postsql' and column_name like '%id%'")
conMysqlPost <- dbConnect(dbDriver("MySQL"), dbname = "postsql", user = "root", 
    password = "12345678")
# 通过eval函数 修改数据库 字段类型
for (ii in 1:51) {
    
    strDb = c("dbSendQuery(conMysqlPost,'alter table", dbContent[ii, 1], "change column", 
        dbContent[ii, 2], dbContent[ii, 2], "varchar(100)')")
    strChange = paste(strDb, collapse = " ")
    
    eval(parse(text = strChange))
    
}
# 添加索引 ALTER TABLE drug_claims ADD INDEX index_name1( `drug_claim_id`)
rm(strDb)
for (jj in 1:51) {
    strDb = c("dbSendQuery(conMysqlPost,'alter table ", dbContent[jj, 1], " add index index_name", 
        jj, "(`", dbContent[jj, 2], "`);')")
    strChange = paste(strDb, collapse = "")
    
    eval(parse(text = strChange))
} 
