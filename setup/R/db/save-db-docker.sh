#!/usr/bin/env bash

set -e

trap 'cleanup' ERR

if [ "$#" -ne 0 ]; then
    echo "Usage: save-db-docker"
    echo
    #echo "Creates gdancik/bcbet-mysql and gdancik/bcbet-mongo images from lamp-rm volumes and pushes to dockerhub"
    echo "Creates gdancik/bcbet-mongo images from lamp-rm volumes and pushes to dockerhub"
    exit
fi


if [ ! -d "docker-db" ] 
then
    echo "cloning docker-db"
    git clone https://github.com/gdancik/docker-db.git 
fi

cd docker-db 

#echo
#echo "creating gdancik/bcbet-mysql...\n"
#./copy-mysql-volume.sh gdancik/bcbet-mysql lamp-rm_mysql-data

echo
echo "creating gdancik/bcbet-mongo...\n"
./copy-mongo-volume.sh gdancik/bcbet-mongo lamp-rm_mongo-data bcbet
echo

#echo
#echo "pushing gdancik/bcbet-mysql to dockerhub."
#docker push gdancik/bcbet-mysql
#echo

echo "pushing gdancik/bcbet-mongo to dockerhub."
docker push gdancik/bcbet-mongo
echo

d=`date +%Y_%m_%d`
#docker tag gdancik/bcbet-mysql gdancik/bcbet-mysql:$d
docker tag gdancik/bcbet-mongo gdancik/bcbet-mongo:$d

#echo "pushing gdancik/bcbet-mysql:$d and gdancik/bcbet-mongo:$d to dockerhub"
#docker push gdancik/bcbet-mysql:$d

echo
docker push gdancik/bcbet-mongo:$d

echo "images successfully pushed to dockerhub"

