# run php with mysqli 
FROM php:7.4.3-apache
RUN apt-get update
RUN docker-php-ext-install mysqli pdo pdo_mysql
