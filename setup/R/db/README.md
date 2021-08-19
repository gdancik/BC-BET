# create_de_tables

## Database structure

This script sets up the entire mongo database, with the following tables:

- ds_expr: expression data for each dataset 'ds'
- ds_clinical: clinical data for each dataset 'ds'
- tumor, stage, grade: DE results for tumor, stage, and grade
- survival, survival_hg_mi, survival_lg_nmi: survival results, with latter two blimited to 'hg' and 'mi' or 'lg' and 'nmi' samples
- stats_tumor, stats_grade, etc: sample size statistics for various analyses
- genes: collection of valid gene names for querying 

## Genes

- Update the list of hgnc official genes from ../../data/genes/ by running Rscript getGenes.R
- When expression data is added to mongo, the gene collection will be updated 

## Shiny

The BC-BET shiny web app will use the database as follows:

- Summary results: queries 'tumor', 'grade', 'stage', and survival tables 
- DE Plots: queries 'expr' and 'clinical' tables for *datasets contained in summary results*
- Survival Plots: same as above, but limits to 'nmi 'and 'lg' or 'mi' and 'hg' where appropriate or determined by special cases. 

## Special Cases:

- Special cases are currently hard-coded in both create_de_tables.R and shiny scripts. 
- Cohorts 'mda1' and 'mda2' contain hg, mi tumors or mi tumors with unspecified grade, respectively. 
- For these, survival analysis of *all* patients are stored in the *survival_hg_mi* collection.
- In *shiny*, survival plots for these datasets are handled appropriately.

