library(solvebio)
library(data.table)

login(api_key="<api_key>", api_host="<api_host>")

# Define a gene symbol filter to extract only the EGFR mutations reported in 3D HotSpots:
genes_filter <- list(list('gene_symbol', paste("EGFR")))

# download EGFR signficiant residues from 3D HotSpots on SolveBio:
HotSpots <- Dataset.get_by_full_path("solveBio:public:/3DHotSpots/1.0.0-2017-01/SignificantResidues-GRCh37")
EGFR_sig_res <- as.data.table(Dataset.query(id = HotSpots$id,
                                            filters = genes_filter, 
                                            fields = c('variant_sbid', 'gene_symbol', 
                                                       'amino_acid_position', 'reference_amino_acid', 
                                                       'variant_amino_acid'),
                                            paginate=TRUE))

head(subset(EGFR_sig_res, select = -variant_sbid))
dim(EGFR_sig_res)

# Add SwissProt ID to the EGFR_sig_res data tabel to enable protein features lookups:
# download EGFR swissprot record from SolveBio
SwissProt <- Dataset.get_by_full_path("solveBio:public:/SwissProt/1.0.0-2017-02/SwissProt")
EGFR_swissprot <- as.data.table(Dataset.query(id = SwissProt$id,
                                              filters=genes_filter, 
                                              fields = c('swissprot_id'), paginate=TRUE))
EGFR_swissprot

# add swissprot ID column to EGFR signicificant residues data set
EGFR_sig_res$swissprot_id <- EGFR_swissprot

# Now, that we have EGFR swissprot_id, we can easily look up EGFR protein features from the SwissProt Features dataset, which is keyed by swissprot_id.
# create a filter for the SwissProt Features dataset to return only EGFR features
features_filter <- list(list('swissprot_id', paste(EGFR_swissprot)))

# download the EGFR SwissProt Features
SwissProt_Features <- Dataset.get_by_full_path("solveBio:public:/SwissProt/1.0.0-2017-02/Features")
EGFR_sp_features = as.data.table(Dataset.query(id = SwissProt_Features$id,
                                               filter = features_filter, 
                                               fields = c('start_value', 'stop_value', 'feature', 'swissprot_id'), 
                                               paginate = TRUE))
# SwissProt Features for EGFR:
head(EGFR_sp_features)
dim(EGFR_sp_features)

# Add Features column to the EGFR_sig_res dataset and populate it with the features that map to the mutation positions:
# create a vector of all unique amino acid positions 
unique_aapos <- unique(EGFR_sig_res$amino_acid_position)

# add all features from the SwissProt Features dataset that
# map to each variant position in EGFR_sig_res
for (pos in unique_aapos){
    f <- EGFR_sp_features$feature[ EGFR_sp_features$start_value <= pos 
                                   & EGFR_sp_features$stop_value >= pos ]
    EGFR_sig_res$feature[EGFR_sig_res$amino_acid_position == pos] <- unique(list(f))
}
colnames(EGFR_sig_res)
dim(EGFR_sig_res)

# Unique SwissProt Features that were found for the 3D HotSpots EGFR mutations:
unique(unlist(EGFR_sig_res$feature))

# From 3DHotSpots variant information, which includes gene and amino acid position and change, SolveBio generates a variant ID in genomic coordinates. This enables look up of data from genomics data sources, such as SolveBio's ClinVar/Combined dataset.
# download the ClinVar/Combined dataset for EGFR:
ClinVar_Combined <- Dataset.get_by_full_path("solveBio:public:/ClinVar/3.7.4-2017-01-30/Combined-GRCh37")
cv_comb <- as.data.table(Dataset.query(id = ClinVar_Combined$id,
                                      filter=genes_filter, 
                                      fields=c('clinical_significance', 'phenotype', 'variant_sbid'),
                                      paginate=TRUE))
cv_comb$variant_sbid <- toupper(cv_comb$variant_sbid) 
dim(cv_comb)

# reshape to delist variant_sbid in order to merge
EGFR_sig_res <- as.data.table(EGFR_sig_res)
EGFR_sig_res$rowID<-seq.int(nrow(EGFR_sig_res))
EGFR_sig_res <- EGFR_sig_res[, length_vsbid:=lapply(EGFR_sig_res$variant_sbid,length)]
EGFR_sig_res$length_vsbid <- unlist(EGFR_sig_res$length_vsbid)
extra_rows <- data.table(rep(EGFR_sig_res$rowID, EGFR_sig_res$length_vsbid))
names(extra_rows) <- c("rowID")
expand_EGFR_sig_res <- merge(EGFR_sig_res, extra_rows)
varsbids <- unlist(EGFR_sig_res$variant_sbid)
setnames(expand_EGFR_sig_res, "variant_sbid", "variant_sbid_list")
expand_EGFR_sig_res[, 'variant_sbid'] <- varsbids


# head(expand_EGFR_sig_res$variant_sbid)
# dim(expand_EGFR_sig_res)
EGFR_sig_res <- expand_EGFR_sig_res

# We will now do an inner merge on variant_sbid, to find the 3D HotSpot variants that have also been annotated in ClinVar:
# merge with ClinVar on variant_sbid
hotspots_clinvar <- merge(EGFR_sig_res, cv_comb, by='variant_sbid')
colnames(hotspots_clinvar)
dim(hotspots_clinvar)

# What type of clinical significance and phenotypes come up for the 3D HotSpot variants found ClinVar?
unique(unlist(hotspots_clinvar$clinical_significance))
unique(unlist(hotspots_clinvar$phenotype))

# Finally, we will merge our reduced dataset with ICGC EGFR variant data, to find the 3D HotSpot variants that are annotated in both ClinVar and ICGC:
# download the ICGC EGFR data:
ICGC_Mutation <- Dataset.get_by_full_path("solveBio:public:/ICGC/3.0.0-23/SimpleSomaticMutation-GRCh37")
icgc_egfr <- as.data.table(Dataset.query(id = ICGC_Mutation$id,
                                         filter=genes_filter, 
                                         fields=c('variant_sbid', 'occurence'),
                                         paginate=TRUE))
icgc_egfr$variant_sbid <- toupper(icgc_egfr$variant_sbid)

# merge the hotspot_clinvar data table with ICGC on variant_sbid
hotspots_cv_icgc <- merge(hotspots_clinvar, icgc_egfr, by='variant_sbid')
dim(hotspots_cv_icgc)

##ICGC occurrence information for the 3D HotSpot variants that come up in both ICGC and ClinVar:
hotspots_cv_icgc$occurence
