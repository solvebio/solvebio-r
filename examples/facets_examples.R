library(solvebio)
library(data.table)

login()

ClinVar <- Dataset.get_by_full_path("solveBio:public:/ClinVar/3.7.4-2017-01-30/Combined-GRCh37")

facets <- Dataset.facets(id = ClinVar$id, facets = list("clinical_significance", "gene_symbol"))

clin_sign_table <- as.data.table(facets$clinical_significance)
names(clin_sign_table) <- c("clinical_significace", "count")

gene_symbol_table <- as.data.table(facets$gene_symbol)
names(gene_symbol_table) <- c("gene_symbol", "count")

barplot(as.integer(gene_symbol_table$count),
        main = "Top 10 occurring Gene Symbol counts",
        names.arg = gene_symbol_table$gene_symbol,
        cex.names=0.6)

mean_count <- mean(as.integer(gene_symbol_table$count))

##################

ClinVar <- Dataset.get_by_full_path("solveBio:public:/ClinVar/3.7.4-2017-01-30/Combined-GRCh37")

whole_query <- Dataset.query(ClinVar, paginate=TRUE, limit = 10000)

h <- hist(whole_query$genomic_coordinates.start, breaks = "FD")

total_doc <- ClinVar$documents_count

perc <- Dataset.facets(id = ClinVar$id,
  '{"genomic_coordinates.start": {"facet_type": "percentiles"}}'
)

iqr <- perc$genomic_coordinates.start$`75.0` - perc$genomic_coordinates.start$`25.0`
binsize <- 2 * iqr / (total_doc ^ (1 / 3))
hist(whole_query$genomic_coordinates.start,
     breaks = binsize,
     main = "test")

hist <- Dataset.facets(id = ClinVar$id,
                      '{"genomic_coordinates.start": {"facet_type": "histogram"}}')
iqr <- perc$genomic_coordinates.start$`75.0` - perc$genomic_coordinates.start$`25.0`

hist_table <- as.data.table(hist$genomic_coordinates.start)
names(hist_table) <- c("genomic_coordinates.start", "count")
ord_hist <- hist_table[order(hist_table$count)]
iqr <- IQR(ord_hist$count)
binsize <- 2 * iqr / (length(ord_hist$count) ^ (1 / 3))
