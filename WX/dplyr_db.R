
load('./data/tidy.rdata')
sqlite <- src_sqlite('./data/sqlite.db')
system.time({
  copy_to(sqlite, all_data, indexes = list('entrez', 'ensembl'))
  copy_to(sqlite, PARS_id, indexes = list('name','gene_id'))
  aa <- tbl(sqlite, sql('select * from all_data left join PARS_id on all_data.entrez = PARS_id.gene_id or all_data.ensembl = PARS_id.name'))
})
system.time(({
  result <- collect(aa)
}))
 system.time({
  cc <- sqldf('select * from all_data left join PARS_id on all_data.entrez = PARS_id.gene_id or all_data.ensembl = PARS_id.name')
   
 }) 
write.csv(result, file = './data/utr3_mapping.csv')
group_by(all_data, entre)
