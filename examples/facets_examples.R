facets = Dataset.facets("ClinVar/Combined",
                        list("clinical_significance", "gene_symbol"))

clin_sign_table = as.data.table(facets$clinical_significance)
names(clin_sign_table) <- c("clinical_significace", "count")

gene_symbol_table = as.data.table(facets$gene_symbol)
names(gene_symbol_table) <- c("gene_symbol", "count")

barplot(as.integer(gene_symbol_table$count))
mean_count = mean(as.integer(gene_symbol_table$count))

##################

dataset = Dataset.retrieve("clinvar/combined")
whole_query = Dataset.query("clinvar/combined", paginate = TRUE, limit = 10000)
h = hist(whole_query$genomic_coordinates.start, breaks = "FD")

total_doc = dataset$documents_count
perc = Dataset.facets(
  "clinvar/combined",
  '{"genomic_coordinates.start": {"facet_type": "percentiles"}}'
)
iqr = perc$genomic_coordinates.start$`75.0` - perc$genomic_coordinates.start$`25.0`
binsize = 2 * iqr / (total_doc ^ (1 / 3))
hist(whole_query$genomic_coordinates.start,
     breaks = binsize,
     main = "test")

hist = Dataset.facets("clinvar/combined",
                      '{"genomic_coordinates.start": {"facet_type": "histogram"}}')
iqr = perc$genomic_coordinates.start$`75.0` - perc$genomic_coordinates.start$`25.0`

hist_table = as.data.table(hist$genomic_coordinates.start)
names(hist_table) <- c("genomic_coordinates.start", "count")
ord_hist = hist_table[order(hist_table$count)]
iqr = IQR(ord_hist$count)
binsize = 2 * iqr / (length(ord_hist$count) ^ (1 / 3))
