table(ensg_mapping2$utr5_degree) %>% View
table(ensg_mapping2$utr3_degree) %>% View
table(ensg_mapping2$cds_degree) %>% View

utr3 <- 
  colnames(ensg_mapping2)
  mapply(function(x,y){
    filter(ensg_mapping2, utr3_degree >= x,  utr3_degree <= y, is.nan(pars_nat)==F, is.nan(pars_mbe) == F, is.na(pars_nat) == F, is.na(pars_mbe) == F, is.nan(mRNA_tissue_abun_avg) ==F, is.na(mRNA_tissue_abun_avg) ==F) %>% select(-c(1, 13, 14, 15)) %>% colMeans()}, c(1,2, 4, 7, 11, 16, 31),c(1,3, 6, 10, 15, 30, 1000)) 

  
pars_nat_avg <- mean(ensg_mapping3$pars_nat, na.rm = T)
pars_mbe_avg <- mean(ensg_mapping3$pars_mbe, na.rm = T)

pars_nona <- filter(ensg_mapping3, (pars_nat < (pars_nat_avg/2) | pars_nat > (pars_nat_avg*2)), is.na(pars_nat) == F, pars_nat != 0)
cor <- corr_f(pars_nona, c(1, 14, 15))
View(cor)


pars_mbe_nona <- filter(ensg_mapping3, (pars_mbe < (pars_mbe_avg/2) | pars_mbe> (pars_mbe_avg*2)), is.na(pars_mbe) == F, pars_mbe != 0)
cor_mbe <- corr_f(pars_mbe_nona, c(1, 14, 15))
View(cor_mbe)


