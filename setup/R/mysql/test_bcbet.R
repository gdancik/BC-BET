# Test connection to MySQL DB using lamp-rm docker service.
# See: https://github.com/gdancik/lamp-rm

library(RMariaDB)

# Connect to employee database using a configuration file,
# (see tables 4.1 and 4.2 at 
#    https://dev.mysql.com/doc/refman/8.0/en/option-files.html)
con <- dbConnect(MariaDB(), group = "BCBET")

# test query
#employees <- dbGetQuery(con, 'SELECT * from employees LIMIT 10 ')


dbExecute(con, 'CREATE TABLE newtable (column1 int, column2 int)')


t <- dbGetQuery(con, 'show tables;')
print(t)


# disconnect from database
dbDisconnect(con)

