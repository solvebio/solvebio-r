library(solvebio)
library(data.table)

# get the top 10 most common gene_symbols and clinical significance for the ClinVar/Combined dataset
facets = Dataset.facets("ClinVar/Combined", list("clinical_significance", "gene_symbol"))

# convert the outputs into corresponding data tables:
clin_sign_table = as.data.table(facets$clinical_significance)
gene_symbol_table = as.data.table(facets$gene_symbol)

# assign the names to the columns of data tables
names(clin_sign_table)<-c("clinical_significace","count")
names(gene_symbol_table)<-c("gene_symbol","count")

# counts are returned as strings, so convert to integers for further analysis, eg
barplot(as.integer(gene_symbol_table$count))
mean_count = mean(as.integer(gene_symbol_table$count))

##################
# plot histograms: either loading the complete query or using facets:
# Using the whole dataset or whole field of interest (eg genomic_coordinates.start) query:
dataset = Dataset.retrieve("clinvar/combined")
whole_query = Dataset.query("clinvar/combined",paginate=TRUE,limit = 10000,
                            fields=list("genomic_coordinates.start"))
# alternately, load only the field of interest, e.g. genomic_coordinates.start
gen_coord_start_query = Dataset.query("clinvar/combined",paginate=TRUE,limit = 10000,
                                      fields=list("genomic_coordinates.start"))
# R can calculate the Freedman–Diaconis binsize = use breaks = "FD", but requires loading the
hist = hist(whole_query$genomic_coordinates.start,breaks="FD",
            main = "Genomic.coordinates.start: count of term occurences",
            ylab = "count",xlab = "")

# Use facets without loading the whole dataset, use facets,  but calculate the interval size for the histogram
total_doc = dataset$documents_count
perc = Dataset.facets("clinvar/combined", '{"genomic_coordinates.start": {"facet_type": "percentiles"}}')
iqr = perc$genomic_coordinates.start$`75.0` - perc$genomic_coordinates.start$`25.0`
binsize = 2*iqr/(total_doc^(1/3))
# binsize = 3112312
hist = Dataset.facets("clinvar/combined", 
                      '{"genomic_coordinates.start": 
                      {"facet_type": "histogram", "interval":"3112312"}}')
#plot the facets histogram:
plot(hist$genomic_coordinates.start[,1],hist$genomic_coordinates.start[,2],type = "h")


