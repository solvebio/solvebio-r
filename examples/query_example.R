# Basic SolveBio R Demo
# Requires the solvebio R package
library(solvebio)

# log into your SolveBio account
login()

# Retrieve and access data about a dataset
ClinVar = Dataset.get_by_full_path("solveBio:public:/ClinVar/3.7.0-2015-12-06/Variants-GRCh38")

# Query the dataset using a filter.
# Set paginate=TRUE in order to query the whole dataset.
# The output of Dataset.query() is a data.frame.
filters = '[["gene_symbol", "BRCA1"]]'
q = Dataset.query(ClinVar, paginate=TRUE, filters=filters, limit=10000)
colnames(q)
head(q[,1:5])
dim(q)

# Narrow down your query using one or more filters
filters = '[["gene_symbol", "BRCA1"],["clinical_significance", "Pathogenic"],["review_status", "criteria provided, single submitter"]]'
q = Dataset.query(ClinVar, paginate=TRUE, filters=filters, limit=10000)
dim(q)
