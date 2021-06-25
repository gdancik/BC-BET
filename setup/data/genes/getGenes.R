genes <- HGNChelper::getCurrentHumanMap()
genes <- unique(genes$Approved.Symbol)
saveRDS(genes, file = 'genes.rda')
