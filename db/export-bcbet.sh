set -e

if [ "$#" -ne 1 ]; then
    echo "Usage: export-bcbet mongo-directory"
    echo
    echo "Exports bcbet-mysql and bcbet-mongo databases,"
    echo "where bcbet-mysql is stored in volume lamp-rm_mysql-data"
    echo "and bcbet-mongo databases is dumped to mongo-directory,"
    echo "for use with lamp-rm"
    echo
    echo "Note that the mongo-directory will be overwritten"
    exit
fi


# create volume lamp-rm_mysql-data volume from bcbet-mysql
echo "creating lamp-rm_mysql-data volume from bcbet-mysql..."
docker run --rm -v lamp-rm_mysql-data:/var/lib/mysql-no-volume --name bcbet-mysql -d gdancik/bcbet-mysql
sleep 3
docker stop bcbet-mysql

echo

# dump mongo database to appropriate directory
echo "dumping mongo database to $1..."
docker run --rm -v $1:/data/tmp --name bcbet-mongo -d gdancik/bcbet-mongo
sleep 3
docker exec bcbet-mongo bash -c "rm -Rf /data/tmp/* && cp -r /docker-entrypoint-initdb.d/* /data/tmp/"
docker stop bcbet-mongo


#echo "volumes lamp-rm_mysql-data and lamp-rm_mongo-data are created"







