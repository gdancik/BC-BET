# Using shinyproxy

## Download docker images

```
docker pull gdancik/bcbet-ui
```

```
docker pull gdancik/bcbet-mongo
```

### Launch mongo

```
docker run -itd --name bcbet-mongo --env MONGO_INITDB_ROOT_USERNAME=root --env MONGO_INITDB_ROOT_PASSWORD=password -p 2000:27017 gdancik/bcbet-mongo
```

### set the mongo IP in bcbet-ui

```
docker/set_mongo_ip.sh
```

### run shinyproxy

```
nohup java -jar shinyproxy-2.6.1.jar &
```
