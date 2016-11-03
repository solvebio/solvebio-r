# Basic SolveBio R Demo
# Requires the solvebio R package
library(solvebio)

# Retrieve and access data about a dataset
ClinVar = Dataset.retrieve('ClinVar/3.7.2-2016-08-02/Variants')

# Query the dataset using a filter.
# Set paginate=TRUE in order to query the whole dataset.
# The output of Dataset.query() is a data.frame.
filters = '[["gene_symbol", "BRCA1"]]'
q = Dataset.query('ClinVar/3.7.2-2016-08-02/Variants', paginate=TRUE, filters=filters, limit=10000)
colnames(q)
head(q[,1:5])
dim(q)

# Narrow down your query using one or more filters
filters = '[["gene_symbol", "BRCA1"],["clinical_significance", "Pathogenic"],["review_status", "criteria provided, single submitter"]]'
q = Dataset.query('ClinVar/3.7.2-2016-08-02/Variants', paginate=TRUE, filters=filters, limit=10000)
dim(q)
