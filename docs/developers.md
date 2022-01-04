# BC-BET Developer Page

This page contains information for BC-BET developers for processing the data and building the database from scratch.

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
