yeast_go_mf <- read_csv('./raw_data/yeast_go_data.csv', col_names = F) %>% unique()
nrow(yeast_go_stress)

yeast_go_stress <- read_csv('./raw_data/yeast_go_stress.csv', col_names = F)
dplyr::intersect(yeast_go_mf[,1], yeast_go_stress[,1]) %>% nrow()

SELECT distinct

dbxref.xref_key AS gp_acc
FROM term
INNER JOIN graph_path ON (term.id=graph_path.term1_id)
INNER JOIN association ON (graph_path.term2_id=association.term_id)
INNER JOIN gene_product ON (association.gene_product_id=gene_product.id)
INNER JOIN species ON (gene_product.species_id=species.id)
INNER JOIN dbxref ON (gene_product.dbxref_id=dbxref.id)
WHERE
term.name = 'molecular_function' AND species.genus = 'Saccharomyces' AND dbxref.xref_dbname = 'SGD'

mf_pars <- inner_join(yeast_go_mf, all_data, by = c('X1'='sgd_id')) %>% dplyr::select(pars) %>% na.omit() %>% as.data.frame()
stress_pars <- inner_join(yeast_go_stress, all_data, by = c('X1'='sgd_id')) %>% dplyr::select(pars) %>% na.omit() %>% as.data.frame()

View(stress_pars)

shapiro.test(stress_pars[,1] %>% as.numeric())
wilcox.test(stress_pars[,1], mf_pars[,1])
mean(stress_pars[,1])
mean(mf_pars[,1])
median(mf_pars[,1])
median(stress_pars[,1])
intersect(mf_pars[,1], stress_pars[,1]) %>% length()
nrow(stress_pars)
