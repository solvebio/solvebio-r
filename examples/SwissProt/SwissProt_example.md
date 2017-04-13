SolveBio presents SwissProt as readily usable tables that can go
straight into R data frames. With simple commands, users can query and
filter SwissProt without having to download all of SwissProt onto their
computer. In this example, we show how easy it is to quickly extract
very specific information for your gene of interest. We use the 'BRCA1'
gene as an example.

    library(solvebio)
    library(data.table)

login to SolveBio:

    login(api_key="<api_key>", api_host="<api_host>")

Define a filter to extract the BRCA1 entry:

    genes = c("BRCA1")
    genes_filter = list(list('gene_symbol', paste(genes)))

Run your query on the main SwissProt dataset:

    swissprot = Dataset.query('SwissProt/1.0.0-2017-02/SwissProt',  filters=genes_filter, paginate=TRUE)
    colnames(swissprot)

    ##  [1] "description"             "swissprot_id"           
    ##  [3] "sequence"                "host_organism"          
    ##  [5] "synonyms"                "entry_name"             
    ##  [7] "molecule_type"           "taxonomy_id"            
    ##  [9] "sequence_length"         "_commit"                
    ## [11] "eco_id"                  "host_taxonomy_id"       
    ## [13] "created"                 "organism_classification"
    ## [15] "orf_names"               "annotation_update"      
    ## [17] "molecular_weight"        "organelle"              
    ## [19] "gene_symbol"             "refseq_nm_id"           
    ## [21] "accessions"              "keywords"               
    ## [23] "sequence_update"         "_id"                    
    ## [25] "organism"                "data_class"

    swissprot

    ##                                                                                                                                                                                                                                                                                                                                                          description
    ## 1 RecName: Full=Breast cancer type 1 susceptibility protein; EC=2.3.2.27 {ECO:0000269|PubMed:10500182, ECO:0000269|PubMed:12887909, ECO:0000269|PubMed:12890688, ECO:0000269|PubMed:16818604, ECO:0000269|PubMed:18056443, ECO:0000269|PubMed:20351172}; AltName: Full=RING finger protein 53; AltName: Full=RING-type E3 ubiquitin transferase BRCA1 {ECO:0000305};
    ##   swissprot_id
    ## 1       P38398
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  sequence
    ## 1 MDLSALRVEEVQNVINAMQKILECPICLELIKEPVSTKCDHIFCKFCMLKLLNQKKGPSQCPLCKNDITKRSLQESTRFSQLVEELLKIICAFQLDTGLEYANSYNFAKKENNSPEHLKDEVSIIQSMGYRNRAKRLLQSEPENPSLQETSLSVQLSNLGTVRTLRTKQRIQPQKTSVYIELGSDSSEDTVNKATYCSVGDQELLQITPQGTRDEISLDSAKKAACEFSETDVTNTEHHQPSNNDLNTTEKRAAERHPEKYQGSSVSNLHVEPCGTNTHASSLQHENSSLLLTKDRMNVEKAEFCNKSKQPGLARSQHNRWAGSKETCNDRRTPSTEKKVDLNADPLCERKEWNKQKLPCSENPRDTEDVPWITLNSSIQKVNEWFSRSDELLGSDDSHDGESESNAKVADVLDVLNEVDEYSGSSEKIDLLASDPHEALICKSERVHSKSVESNIEDKIFGKTYRKKASLPNLSHVTENLIIGAFVTEPQIIQERPLTNKLKRKRRPTSGLHPEDFIKKADLAVQKTPEMINQGTNQTEQNGQVMNITNSGHENKTKGDSIQNEKNPNPIESLEKESAFKTKAEPISSSISNMELELNIHNSKAPKKNRLRRKSSTRHIHALELVVSRNLSPPNCTELQIDSCSSSEEIKKKKYNQMPVRHSRNLQLMEGKEPATGAKKSNKPNEQTSKRHDSDTFPELKLTNAPGSFTKCSNTSELKEFVNPSLPREEKEEKLETVKVSNNAEDPKDLMLSGERVLQTERSVESSSISLVPGTDYGTQESISLLEVSTLGKAKTEPNKCVSQCAAFENPKGLIHGCSKDNRNDTEGFKYPLGHEVNHSRETSIEMEESELDAQYLQNTFKVSKRQSFAPFSNPGNAEEECATFSAHSGSLKKQSPKVTFECEQKEENQGKNESNIKPVQTVNITAGFPVVGQKDKPVDNAKCSIKGGSRFCLSSQFRGNETGLITPNKHGLLQNPYRIPPLFPIKSFVKTKCKKNLLEENFEEHSMSPEREMGNENIPSTVSTISRNNIRENVFKEASSSNINEVGSSTNEVGSSINEIGSSDENIQAELGRNRGPKLNAMLRLGVLQPEVYKQSLPGSNCKHPEIKKQEYEEVVQTVNTDFSPYLISDNLEQPMGSSHASQVCSETPDDLLDDGEIKEDTSFAENDIKESSAVFSKSVQKGELSRSPSPFTHTHLAQGYRRGAKKLESSEENLSSEDEELPCFQHLLFGKVNNIPSQSTRHSTVATECLSKNTEENLLSLKNSLNDCSNQVILAKASQEHHLSEETKCSASLFSSQCSELEDLTANTNTQDPFLIGSSKQMRHQSESQGVGLSDKELVSDDEERGTGLEENNQEEQSMDSNLGEAASGCESETSVSEDCSGLSSQSDILTTQQRDTMQHNLIKLQQEMAELEAVLEQHGSQPSNSYPSIISDSSALEDLRNPEQSTSEKAVLTSQKSSEYPISQNPEGLSADKFEVSADSSTSKNKEPGVERSSPSKCPSLDDRWYMHSCSGSLQNRNYPSQEELIKVVDVEEQQLEESGPHDLTETSYLPRQDLEGTPYLESGISLFSDDPESDPSEDRAPESARVGNIPSSTSALKVPQLKVAESAQSPAAAHTTDTAGYNAMEESVSREKPELTASTERVNKRMSMVVSGLTPEEFMLVYKFARKHHITLTNLITEETTHVVMKTDAEFVCERTLKYFLGIAGGKWVVSYFWVTQSIKERKMLNEHDFEVRGDVVNGRNHQGPKRARESQDRKIFRGLEICCYGPFTNMPTDQLEWMVQLCGASVVKELSSFTLGTGVHPIVVVQPDAWTEDNGFHAIGQMCEAPVVTREWVLDSVALYQCQELDTYLIPQIPHSHY
    ##   host_organism synonyms entry_name molecule_type taxonomy_id
    ## 1          NULL    RNF53      BRCA1            NA        9606
    ##   sequence_length            _commit eco_id host_taxonomy_id    created
    ## 1            1863 431821895740221876     NA             NULL 1994-10-01
    ##                                                                                                                                      organism_classification
    ## 1 Eukaryota, Metazoa, Chordata, Craniata, Vertebrata, Euteleostomi, Mammalia, Eutheria, Euarchontoglires, Primates, Haplorrhini, Catarrhini, Hominidae, Homo
    ##   orf_names annotation_update molecular_weight organelle gene_symbol
    ## 1        NA        2017-02-15           207721                 BRCA1
    ##                                                      refseq_nm_id
    ## 1 NM_007294.3, NM_007297.3, NM_007298.3, NM_007299.3, NM_007300.3
    ##                                                       accessions
    ## 1 P38398, E9PFZ0, O15129, Q1RMC1, Q3LRJ0, Q3LRJ6, Q6IN79, Q7KYU9
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      keywords
    ## 1 3D-structure, Acetylation, Activator, Alternative initiation, Alternative splicing, Cell cycle, Chromosome, Complete proteome, Cytoplasm, Direct protein sequencing, Disease mutation, DNA damage, DNA recombination, DNA repair, DNA-binding, Fatty acid biosynthesis, Fatty acid metabolism, Isopeptide bond, Lipid biosynthesis, Lipid metabolism, Metal-binding, Nucleus, Phosphoprotein, Polymorphism, Reference proteome, Repeat, Transcription, Transcription regulation, Transferase, Tumor suppressor, Ubl conjugation, Ubl conjugation pathway, Zinc, Zinc-finger
    ##   sequence_update                  _id              organism data_class
    ## 1      1995-02-01 AVteL4RIC0a5BPxHLn8w Homo sapiens (Human).   Reviewed

    dim(swissprot)

    ## [1]  1 26

SwissProt provides extensive protein annotation information. SolveBio
enables easy access and usability of this key information by extracting
it into its own 'Features' dataset, where each feature is keyed by
swissprot\_id. For our example, let's extract the "DOMAIN" features for
"BRCA1". We need to filter the 'Features' dataset by BRCA1's swissprot
ID and by the feature category 'DOMAIN'.

    # define the filter
    swissprot_feature_filter = list(list('swissprot_id', paste(swissprot$swissprot_id)), list('feature', paste('DOMAIN')))
    # query the dataset with the filter
    swissprot_features = Dataset.query('SwissProt/1.0.0-2017-02/Features', filters=swissprot_feature_filter, paginate=TRUE)
    colnames(swissprot_features)

    ##  [1] "_commit"             "swissprot_id"        "stop"               
    ##  [4] "feature"             "start"               "stop_value"         
    ##  [7] "start_value"         "_id"                 "feature_source"     
    ## [10] "feature_description"

    swissprot_features

    ##              _commit swissprot_id stop feature start stop_value
    ## 1 431823644774995769       P38398 1855  DOMAIN  1756       1855
    ## 2 431823644774995769       P38398 1736  DOMAIN  1642       1736
    ##   start_value                  _id feature_source
    ## 1        1756 AVteMprXsOzr6UjlvC9O               
    ## 2        1642 AVteMprXsOzr6UjlvC9P               
    ##                                feature_description
    ## 1 BRCT 2. {ECO:0000255|PROSITE- ProRule:PRU00033}.
    ## 2 BRCT 1. {ECO:0000255|PROSITE- ProRule:PRU00033}.

    dim(swissprot_features)

    ## [1]  2 10

From here, let's extract the actual amino acid sequences for the
domains:

    # add the sequence from SwissProt to the Features data frame
    swissprot_features <- merge(swissprot_features, swissprot[, c("swissprot_id", "sequence")], by="swissprot_id")
    swissprot_features <- as.data.table(swissprot_features)
    # add a column "domain_sequence" and extract from the whole sequence
    swissprot_features[,domain_sequence:=substr(sequence,start_value,stop_value)]

    ##    swissprot_id            _commit stop feature start stop_value
    ## 1:       P38398 431823644774995769 1855  DOMAIN  1756       1855
    ## 2:       P38398 431823644774995769 1736  DOMAIN  1642       1736
    ##    start_value                  _id feature_source
    ## 1:        1756 AVteMprXsOzr6UjlvC9O               
    ## 2:        1642 AVteMprXsOzr6UjlvC9P               
    ##                                 feature_description
    ## 1: BRCT 2. {ECO:0000255|PROSITE- ProRule:PRU00033}.
    ## 2: BRCT 1. {ECO:0000255|PROSITE- ProRule:PRU00033}.
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   sequence
    ## 1: MDLSALRVEEVQNVINAMQKILECPICLELIKEPVSTKCDHIFCKFCMLKLLNQKKGPSQCPLCKNDITKRSLQESTRFSQLVEELLKIICAFQLDTGLEYANSYNFAKKENNSPEHLKDEVSIIQSMGYRNRAKRLLQSEPENPSLQETSLSVQLSNLGTVRTLRTKQRIQPQKTSVYIELGSDSSEDTVNKATYCSVGDQELLQITPQGTRDEISLDSAKKAACEFSETDVTNTEHHQPSNNDLNTTEKRAAERHPEKYQGSSVSNLHVEPCGTNTHASSLQHENSSLLLTKDRMNVEKAEFCNKSKQPGLARSQHNRWAGSKETCNDRRTPSTEKKVDLNADPLCERKEWNKQKLPCSENPRDTEDVPWITLNSSIQKVNEWFSRSDELLGSDDSHDGESESNAKVADVLDVLNEVDEYSGSSEKIDLLASDPHEALICKSERVHSKSVESNIEDKIFGKTYRKKASLPNLSHVTENLIIGAFVTEPQIIQERPLTNKLKRKRRPTSGLHPEDFIKKADLAVQKTPEMINQGTNQTEQNGQVMNITNSGHENKTKGDSIQNEKNPNPIESLEKESAFKTKAEPISSSISNMELELNIHNSKAPKKNRLRRKSSTRHIHALELVVSRNLSPPNCTELQIDSCSSSEEIKKKKYNQMPVRHSRNLQLMEGKEPATGAKKSNKPNEQTSKRHDSDTFPELKLTNAPGSFTKCSNTSELKEFVNPSLPREEKEEKLETVKVSNNAEDPKDLMLSGERVLQTERSVESSSISLVPGTDYGTQESISLLEVSTLGKAKTEPNKCVSQCAAFENPKGLIHGCSKDNRNDTEGFKYPLGHEVNHSRETSIEMEESELDAQYLQNTFKVSKRQSFAPFSNPGNAEEECATFSAHSGSLKKQSPKVTFECEQKEENQGKNESNIKPVQTVNITAGFPVVGQKDKPVDNAKCSIKGGSRFCLSSQFRGNETGLITPNKHGLLQNPYRIPPLFPIKSFVKTKCKKNLLEENFEEHSMSPEREMGNENIPSTVSTISRNNIRENVFKEASSSNINEVGSSTNEVGSSINEIGSSDENIQAELGRNRGPKLNAMLRLGVLQPEVYKQSLPGSNCKHPEIKKQEYEEVVQTVNTDFSPYLISDNLEQPMGSSHASQVCSETPDDLLDDGEIKEDTSFAENDIKESSAVFSKSVQKGELSRSPSPFTHTHLAQGYRRGAKKLESSEENLSSEDEELPCFQHLLFGKVNNIPSQSTRHSTVATECLSKNTEENLLSLKNSLNDCSNQVILAKASQEHHLSEETKCSASLFSSQCSELEDLTANTNTQDPFLIGSSKQMRHQSESQGVGLSDKELVSDDEERGTGLEENNQEEQSMDSNLGEAASGCESETSVSEDCSGLSSQSDILTTQQRDTMQHNLIKLQQEMAELEAVLEQHGSQPSNSYPSIISDSSALEDLRNPEQSTSEKAVLTSQKSSEYPISQNPEGLSADKFEVSADSSTSKNKEPGVERSSPSKCPSLDDRWYMHSCSGSLQNRNYPSQEELIKVVDVEEQQLEESGPHDLTETSYLPRQDLEGTPYLESGISLFSDDPESDPSEDRAPESARVGNIPSSTSALKVPQLKVAESAQSPAAAHTTDTAGYNAMEESVSREKPELTASTERVNKRMSMVVSGLTPEEFMLVYKFARKHHITLTNLITEETTHVVMKTDAEFVCERTLKYFLGIAGGKWVVSYFWVTQSIKERKMLNEHDFEVRGDVVNGRNHQGPKRARESQDRKIFRGLEICCYGPFTNMPTDQLEWMVQLCGASVVKELSSFTLGTGVHPIVVVQPDAWTEDNGFHAIGQMCEAPVVTREWVLDSVALYQCQELDTYLIPQIPHSHY
    ## 2: MDLSALRVEEVQNVINAMQKILECPICLELIKEPVSTKCDHIFCKFCMLKLLNQKKGPSQCPLCKNDITKRSLQESTRFSQLVEELLKIICAFQLDTGLEYANSYNFAKKENNSPEHLKDEVSIIQSMGYRNRAKRLLQSEPENPSLQETSLSVQLSNLGTVRTLRTKQRIQPQKTSVYIELGSDSSEDTVNKATYCSVGDQELLQITPQGTRDEISLDSAKKAACEFSETDVTNTEHHQPSNNDLNTTEKRAAERHPEKYQGSSVSNLHVEPCGTNTHASSLQHENSSLLLTKDRMNVEKAEFCNKSKQPGLARSQHNRWAGSKETCNDRRTPSTEKKVDLNADPLCERKEWNKQKLPCSENPRDTEDVPWITLNSSIQKVNEWFSRSDELLGSDDSHDGESESNAKVADVLDVLNEVDEYSGSSEKIDLLASDPHEALICKSERVHSKSVESNIEDKIFGKTYRKKASLPNLSHVTENLIIGAFVTEPQIIQERPLTNKLKRKRRPTSGLHPEDFIKKADLAVQKTPEMINQGTNQTEQNGQVMNITNSGHENKTKGDSIQNEKNPNPIESLEKESAFKTKAEPISSSISNMELELNIHNSKAPKKNRLRRKSSTRHIHALELVVSRNLSPPNCTELQIDSCSSSEEIKKKKYNQMPVRHSRNLQLMEGKEPATGAKKSNKPNEQTSKRHDSDTFPELKLTNAPGSFTKCSNTSELKEFVNPSLPREEKEEKLETVKVSNNAEDPKDLMLSGERVLQTERSVESSSISLVPGTDYGTQESISLLEVSTLGKAKTEPNKCVSQCAAFENPKGLIHGCSKDNRNDTEGFKYPLGHEVNHSRETSIEMEESELDAQYLQNTFKVSKRQSFAPFSNPGNAEEECATFSAHSGSLKKQSPKVTFECEQKEENQGKNESNIKPVQTVNITAGFPVVGQKDKPVDNAKCSIKGGSRFCLSSQFRGNETGLITPNKHGLLQNPYRIPPLFPIKSFVKTKCKKNLLEENFEEHSMSPEREMGNENIPSTVSTISRNNIRENVFKEASSSNINEVGSSTNEVGSSINEIGSSDENIQAELGRNRGPKLNAMLRLGVLQPEVYKQSLPGSNCKHPEIKKQEYEEVVQTVNTDFSPYLISDNLEQPMGSSHASQVCSETPDDLLDDGEIKEDTSFAENDIKESSAVFSKSVQKGELSRSPSPFTHTHLAQGYRRGAKKLESSEENLSSEDEELPCFQHLLFGKVNNIPSQSTRHSTVATECLSKNTEENLLSLKNSLNDCSNQVILAKASQEHHLSEETKCSASLFSSQCSELEDLTANTNTQDPFLIGSSKQMRHQSESQGVGLSDKELVSDDEERGTGLEENNQEEQSMDSNLGEAASGCESETSVSEDCSGLSSQSDILTTQQRDTMQHNLIKLQQEMAELEAVLEQHGSQPSNSYPSIISDSSALEDLRNPEQSTSEKAVLTSQKSSEYPISQNPEGLSADKFEVSADSSTSKNKEPGVERSSPSKCPSLDDRWYMHSCSGSLQNRNYPSQEELIKVVDVEEQQLEESGPHDLTETSYLPRQDLEGTPYLESGISLFSDDPESDPSEDRAPESARVGNIPSSTSALKVPQLKVAESAQSPAAAHTTDTAGYNAMEESVSREKPELTASTERVNKRMSMVVSGLTPEEFMLVYKFARKHHITLTNLITEETTHVVMKTDAEFVCERTLKYFLGIAGGKWVVSYFWVTQSIKERKMLNEHDFEVRGDVVNGRNHQGPKRARESQDRKIFRGLEICCYGPFTNMPTDQLEWMVQLCGASVVKELSSFTLGTGVHPIVVVQPDAWTEDNGFHAIGQMCEAPVVTREWVLDSVALYQCQELDTYLIPQIPHSHY
    ##                                                                                         domain_sequence
    ## 1: QDRKIFRGLEICCYGPFTNMPTDQLEWMVQLCGASVVKELSSFTLGTGVHPIVVVQPDAWTEDNGFHAIGQMCEAPVVTREWVLDSVALYQCQELDTYLI
    ## 2:      STERVNKRMSMVVSGLTPEEFMLVYKFARKHHITLTNLITEETTHVVMKTDAEFVCERTLKYFLGIAGGKWVVSYFWVTQSIKERKMLNEHDFEV

SolveBio extracts RefSeq transcript IDs to the main SwissProt database,
so it is easy to quickly find genomic sequence references for the
proteins.

    swissprot$refseq_nm_id

    ## [[1]]
    ## [1] "NM_007294.3" "NM_007297.3" "NM_007298.3" "NM_007299.3" "NM_007300.3"

SwissProt provides a comprehensive cross-reference list for its entries
which enable easy mapping of its entries to other databases. Let's look
at a summary of the top 10 databses which have cross references for
BRCA1. We will use SolveBio's facets to do this.

    crossreference_filter = list(list('swissprot_id', paste(swissprot$swissprot_id)))
    cross_reference_facets = Dataset.facets('SwissProt/1.0.0-2017-02/CrossReferences', filters=crossreference_filter,list('database'))
    cross_reference_facets

    ## $database
    ##       [,1]       [,2]
    ##  [1,] "GO"       "82"
    ##  [2,] "PDB"      "27"
    ##  [3,] "PDBsum"   "27"
    ##  [4,] "EMBL"     "16"
    ##  [5,] "Reactome" "16"
    ##  [6,] "Ensembl"  "8" 
    ##  [7,] "InterPro" "8" 
    ##  [8,] "Orphanet" "6" 
    ##  [9,] "CCDS"     "5" 
    ## [10,] "MIM"      "5"
