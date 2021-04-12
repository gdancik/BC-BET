# BC-BET database

Modified version of [lamp-rm](https://github.com/gdancik/lamp-rm) used for BC-BET mysql and mongo databases (mongo coming soon).
  
### Start the webserver by running the following from the *db* directory:

```
docker-compose up
```

To run in the background (in a 'detached' state), type

```
docker-compose up -d
```

This will launch the webserver (PHP + MySQL + Mongo) using the gdancik/bcbet-mysql image. 

### Open a web browser and go to [localhost:8080](localhost:8080). 

Click on the links to test database and database functionality.

### To shutdown the docker containers, type the following from the *db* directory:

```
docker-compose down
```

### Connecting to the mysql database

To connect to mysql database from your local host, use

```
user=root
password=password
host=0.0.0.0
port=3000
```

