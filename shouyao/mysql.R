library(RMySQL)
con = dbConnect(MySQL(), host = '192.168.88.109', user = 'root', password='gs1994', dbname='lsp')

raw_data <- dbGetQuery(con, '
select distinct LSP_VET_info_species.taxonomic_id, LSP_VET_info_species.scientific_name,
	LSP_VET_info_diseases.mesh_id, LSP_VET_info_diseases.disease_name,
                       LSP_VET_info_genes.geneid_ncbi, LSP_VET_info_genes.symbol,
                       LSP_VET_genes_diseases_relations.fdr
                       from
                       LSP_VET_genes_diseases_relations
                       left join LSP_VET_info_genes 
                       on LSP_VET_info_genes.index_ge = LSP_VET_genes_diseases_relations.index_ge
                       left join LSP_VET_info_diseases on LSP_VET_info_diseases.index_dis = LSP_VET_genes_diseases_relations.index_dis
                       left join LSP_VET_info_species on LSP_VET_info_species.taxonomic_id = LSP_VET_genes_diseases_relations.taxonomic_id;
                       
                       ')

write_csv(raw_data, './data/species_disease_gene.csv')
