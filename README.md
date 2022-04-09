# BC-BET [![update_platforms](https://github.com/gdancik/BC-BET/workflows/platforms/badge.svg)](https://github.com/gdancik/BC-BET/actions/workflows/platforms.yml) [![process_data](https://github.com/gdancik/BC-BET/actions/workflows/process.yml/badge.svg)](https://github.com/gdancik/BC-BET/actions/workflows/process.yml) [![.github/workflows/create_db.yml](https://github.com/gdancik/BC-BET/actions/workflows/create_db.yml/badge.svg)](https://github.com/gdancik/BC-BET/actions/workflows/create_db.yml)

Version 2 of the *Bladder Cancer Biomarker Evaluation Tool (BC-BET)*.

## Description
Rapid evaluation of candidate diagnostic and prognostic gene expression biomarkers in 15 bladder cancer patient cohorts (N = 1559). BC-BET calculates how strongly a gene's expression is associated with tumor presence (distinguishing tumor from normal samples), tumor grade (distinguishing low- from high-grade tumors), tumor stage (distinguishing non-muscle invasive from muscle invasive samples), and patient outcome (e.g., disease-specific survival) in each cohort. Versions of BC-BET for the evaluation of methylation biomarkers in 4 cohorts (N = 271 samples), for distinguishing tumor from normal samples, and for expression analysis across 40 commonly used bladder cancer cell lines are also available upon request. 

BC-BET: <a href = "https://bioinformatics.easternct.edu/app/bcbet">https://bioinformatics.easternct.edu/app/bcbet</a>

## Citation
Dancik, G.M. An online tool for evaluating diagnostic and prognostic gene expression biomarkers in bladder cancer. BMC Urol 2015, 15:59. ([link](http://biomedcentral.com/1471-2490/15/59)) 

## Running BC-BET

BC-BET v2.0 is available from the following URL:
<a href = "https://bioinformatics.easternct.edu/app/bcbet">https://bioinformatics.easternct.edu/app/bcbet</a>


### Instructions for running  *BC-BET* locally using Docker 

In most cases, the web version (<a href = "https://bioinformatics.easternct.edu/app/bcbet">https://bioinformatics.easternct.edu/app/bcbet</a>) will be preferred. However, BC-BET can also be run locally using Docker:

1. First, download and install docker from https://docs.docker.com/get-docker/.

2. Save the docker compose script, available at https://raw.githubusercontent.com/gdancik/BC-BET/main/local/docker-compose.yml, to a file named 'docker-compose.yml'

3. From your docker terminal, run the following from the directory containing the *docker-compose.yml* file:

    ```
    docker compose up -d
    ```

    Note that the first time you do this may take several minutes.

4. You can now access *BC-BET* by opening your browser and entering the following:

    ```
    http://localhost:3838
    ```

    Note that it may take a minute or so for the mongo db to fully load.

    If interested, you can also access the mongo db directly from the following URL:

    ```
    http://localhost:8081/
    ```

5. If you wish to shutdown BC-BET, you can run the following docker command from the directory containing the docker-compose.yml file:

    ```
    docker compose down
    ```

### Instructions for developers

See [docs/developers.md](docs/developers.md)

### Page theme

This page uses a slightly modified version of the Cayman theme ([https://github.com/pages-themes/cayman](https://github.com/pages-themes/cayman))
