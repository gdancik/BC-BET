# BC-BET database

Modified version of [lamp-rm](https://github.com/gdancik/lamp-rm) used for BC-BET mysql and mongo databases. This is essentially the same as *lamp-rm* except gdancik/bcbet-mysql and gdancik/bcbet-mongo databases are used. 

Simply run the following from this directory to launch mysql and mongo services with BC-BET data.

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



