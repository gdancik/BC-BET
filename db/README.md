# BC-BET database

Modified version of [lamp-rm](https://github.com/gdancik/lamp-rm) used for BC-BET mysql and mongo databases. This is essentially the same as *lamp-rm* except gdancik/bcbet-mysql and gdancik/bcbet-mongo databases are used. 

Simply run the following from this directory to launch mysql and mongo services with BC-BET data.

Note: gdancik/bcbet-mysql is currently not needed and is commented out.

```
docker compose up -d
```

You may also access the databases through a browser and going to [localhost:8080](localhost:8080). 

Note that changes made to the databases will not persist.

To stop the services run

```
docker-compose down -v
```

For more information, see [lamp-rm](https://github.com/gdancik/lamp-rm)

## Exporting BC-BET

Run the script

```
export-bcbet.sh mongo-directory
```

to export the database for use with *lamp-rm*. This script exports mysql data to the volume *lamp-rm_mongo-data* and copies the mongo data dump (from /docker-entrypoint-initdb.d) to the specified *mongo-directory* (that should be in *lamp-rm*).  
