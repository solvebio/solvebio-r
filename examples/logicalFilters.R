json <- '{"not": [["project_code__prefix", "PACA"]]}'
df = fromJSON(json)
q2 = Dataset.query('ICGC/2.0.0-21/Donor', paginate=TRUE,limit = 10000,
                   filters = list(list("project_code__prefix", "PACA")))

filters1 = '[{"not":["project_code__prefix","PACA"]}]'
query1 = Dataset.query("ICGC/2.0.0-21/Donor", filters=filters1,paginate=TRUE,limit = 10000)

filters2 = '[{"or":[["project_code__prefix","PACA"],["project_code__prefix","BRCA"]]}]'
query2 = Dataset.query("ICGC/2.0.0-21/Donor", filters=filters2,paginate=TRUE,limit = 10000)

filters = '[["date_name_changed__range",["1999-01-01","2016-01-01"]]]'
query = Dataset.query("HGNC/3.0.0-2016-10-24/HGNC", filters=filters)

filters = '[["donor_age_at_diagnosis__range",["20","55"]]]'
query = Dataset.query("ICGC/2.0.0-21/Donor", filters=filters)

query = Dataset.query("HGNC/3.0.0-2016-10-24/HGNC", filters=filters)
