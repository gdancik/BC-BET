#!/bin/bash

# sets mongo ip in .Rprofile for gdancik/bcbet-ui docker container


docker run -itd -p 3838:3838 --name=mybcbet gdancik/bcbet-ui


x=$(docker inspect db_mongo_1 | grep Gateway | awk -F : 'FNR==3 {gsub(/[," ]/,""); print $2'})

echo "Setting mongo host ip to $x in gdancik/bcbet-ui"
docker exec mybcbet bash -c "echo 'local({options(mongo.host = \"$x:2000\")}); cat(\"hello there\n\")' >> ~/.Rprofile"

echo "Commiting to image"
docker commit mybcbet gdancik/bcbet-ui

docker stop mybcbet
