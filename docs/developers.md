# BC-BET [![update_platforms](https://github.com/gdancik/BC-BET/workflows/platforms/badge.svg)](https://github.com/gdancik/BC-BET/actions/workflows/platforms.yml) [![process_data](https://github.com/gdancik/BC-BET/actions/workflows/process.yml/badge.svg)](https://github.com/gdancik/BC-BET/actions/workflows/process.yml) [![.github/workflows/create_db.yml](https://github.com/gdancik/BC-BET/actions/workflows/create_db.yml/badge.svg)](https://github.com/gdancik/BC-BET/actions/workflows/create_db.yml)

Version 2 (under development) of the *Bladder Cancer Biomarker Evaluation Tool (BC-BET)*.

## Description
Rapid evaluation of candidate diagnostic and prognostic gene expression biomarkers in 13 bladder cancer patient cohorts (N = 1454). BC-BET calculates how strongly a gene's expression is associated with tumor presence (distinguishing tumor from normal samples), tumor grade (distinguishing low- from high-grade tumors), tumor stage (distinguishing non-muscle invasive from muscle invasive samples), and patient outcome (e.g., disease-specific survival) in each cohort. BC-BET also allows for evaluation of methylation biomarkers in 4 cohorts (N = 271 samples), for distinguishing tumor from normal samples, and for expression analysis across 40 commonly used bladder cancer cell lines. 

More information and a link to the tool can be found at the BC-BET homepage: https://gdancik.github.io/bioinformatics/BCBET.html.
## Citation
Dancik, G.M. An online tool for evaluating diagnostic and prognostic gene expression biomarkers in bladder cancer. BMC Urol 2015, 15:59. ([link](http://biomedcentral.com/1471-2490/15/59)) 

## BC-BET setup (for developers - setup from scratch)

### Update platform data

From the BC-BET home directory, run
```
cd setup/R 
Rscript get_platforms.R
```

### Process datasets

From the BC-BET home directory, run

```
cd setup/R/process
Rscript run_all.R
```

### Create mysql and mongo databases

#### Initialize empty databases using lamp-rm

Use docker to launch a lamp-rm stack by following the instructions at https://github.com/gdancik/lamp-rm

#### Add data

MySQL is used to store DE results, while Mongo is used to store expression and clinical data. 

To add all data, run the following from the BC-BET home directory:

```
cd setup/R/db 
Rscript create_de_tables.R --mongo=yes
```

For options, run
```
Rscript create_de_tables.R --help
```

Database data will be stored to volumes *lamp-rm_mysql-data* and *lamp-rm_mongo-data* and will persist as long as the volumes are not deleted.

#### Create docker database images (for distribution to others)

Docker database images can be created for distribution to others. The following code creates mysql and mongo database images from the docker volumes and pushes the images, *gdancik/bcbet-mysql* and *gdancik/bcbet-mongo*, to docker.
From the BC-BET home directory, run

```
cd setup/R/db 
./save-db-docker.sh
```

### Run BC-BET from docker database images

Use docker compose with a modified version of lamp-rm (see https://github.com/gdancik/BC-BET/tree/main/db#readme). Note that *lamp-rm* will need to be shutdown in order to tdo this.

From the BC-BET home directory, run

```
cd db 
docker compose up -d
```
