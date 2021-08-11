#!/bin/bash

# sets mongo ip in .Rprofile for gdancik/bcbet-ui docker container
# assumes mongo is running as bcbet-mongo

docker run -itd --name=mybcbet gdancik/bcbet-ui

x=$(docker inspect bcbet-mongo | grep IPAddress | awk -F : 'FNR==3 {gsub(/[," ]/,""); print $2'})

echo "Setting mongo host ip to $x in gdancik/bcbet-ui"
docker exec mybcbet bash -c "echo 'local({options(mongo.host = \"$x\")}); cat(\"hello there\n\")' >> ~/.Rprofile"

echo "Commiting to image"
docker commit mybcbet gdancik/bcbet-ui

docker stop mybcbet

