# data from GSE50676, calulate the PARS score library(dplyr)
# library(sqldf) GM12892, GM12878， GM12891
V1 <- read.delim("/Users/lipidong/work/protein bundunce/data/PARS_GEO_Data/GSM1226158_norm.GM12878_V1.tab", 
    stringsAsFactors = F, header = F)

S1 <- read.delim("/Users/lipidong/work/protein bundunce/data/PARS_GEO_Data/GSM1226157_norm.GM12878_S1.tab", 
    stringsAsFactors = F, header = F)

# S1 V1 合并到一起
S1_V1 <- sqldf("select * from S1 inner join V1 on S1.V1=V1.V1")
rm(S1, V1)  # 删除S1, V1 清理内存
gc()  # 清理垃圾
S1_V1 <- S1_V1[, c(1, 2, 4)]  # 取1，2，4列
colnames(S1_V1) <- c("name", "S1", "V1")  # 命名列名
s1_data <- strsplit(S1_V1[, 2], ";")  # 根据分号拆分
v1_data <- strsplit(S1_V1[, 3], ";")
pars = data.frame()  # save the mean
sapply(1:nrow(S1_V1), function(ii) {
    # 循环S1_V1长度
    
    # 第ii个mRNA
    s1 <- as.numeric(s1_data[[ii]])  # s1中各碱基得分
    v1 <- as.numeric(v1_data[[ii]])  # v1中各碱基得分
    len <- length(s1)  # 全部碱基数目
    
    pos <- which((v1 + s1) > 0)  # s1+v1大于0的碱基位置
    len_mt0 <- length(pos)
    if (len_mt0 > 0) {
        # len_mt0>0表示s1+v1大于0的情况存在
        v1_mt0 <- v1[pos]  # v1中大于0的碱基得分
        s1_mt0 <- s1[pos]  # s1中大于0的碱基得分
        
        # 计算每个碱基的PARS score
        pars_base_mt0 <- sapply(1:length(pos), function(kk) {
            (v1_mt0[kk] + 1)/(s1_mt0[kk] + 1)
        })
        
        pars[ii, 1] <<- S1_V1[ii, 1]  # 第一列为基因名称
        pars[ii, 2] <<- len  # 第二列为基因全部碱基数目
        pars[ii, 3] <<- len_mt0  # 第三列为s1+v1大于0的碱基数目
        pars_sum <- sum(pars_base_mt0)  # 碱基得分的和
        pars[ii, 4] <<- pars_sum/len  #除以全部碱基数目
        pars[ii, 5] <<- pars_sum/len_mt0  # 除以s1+v1的碱基数目
    } else {
        # 如果s1+v1的情况不存在， 为了保证除数不为0，将商直接定义为0
        pars[ii, 1] <<- S1_V1[ii, 1]  # 第一列为基因名称
        pars[ii, 2] <<- len  # 第二列为基因全部碱基数目
        pars[ii, 3] <<- len_mt0  # 第三列为s1+v1大于0的碱基数目
        # pars[ii,2] <<- paste(pars_base, collapse = ';')
        pars[ii, 4] <<- 0
        pars[ii, 5] <<- 0
    }
    print(ii)
})
# 定义列名
colnames(pars) <- c("name", "all_sites_length", "s1+v1>0_length", 
    "all_sites_mean", "s1+v1>0_mean")
write.csv(pars, file = "./data/pars_GM12878_MBE.csv")  # 写入csv文件

# library(org.Hs.eg.db) gene2ref <-
# as.data.frame(org.Hs.egREFSEQ) gene2name <-
# as.data.frame(org.Hs.egGENENAME) pars_id <- sqldf('select
# pars.*,gene2name.gene_id, gene2name.gene_name from pars left
# join gene2ref on pars.V1 = gene2ref.accession left join
# gene2name on gene2name.gene_id = gene2ref.gene_id')
# pars_id$type <- NA pars_id$type[substr(pars_id$V1, 1, 2) ==
# 'NM'] = 'mRNA' pars_id$type[substr(pars_id$V1, 1, 2) == 'NR']
# = 'non-coding RNA' pars_id = pars_id[, c(1, 5, 2:4)]
# pars_GM12892 <- pars_id # save(pars_GM12878, file =
# './data/homo_pars.rdata') 
