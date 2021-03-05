# Test connection to MySQL DB using lamp-rm docker service.
# See: https://github.com/gdancik/lamp-rm

library(RMariaDB)

# Connect to employee database using a configuration file,
# (see tables 4.1 and 4.2 at 
#    https://dev.mysql.com/doc/refman/8.0/en/option-files.html)
con <- dbConnect(MariaDB(), group = "Employees")

# # connect to database without configuration file
# con <- dbConnect(MariaDB(), user = 'root', password = 'password',
#                  host = '0.0.0.0', port = '3000',
#                  dbname = 'employees')

# test query
employees <- dbGetQuery(con, 'SELECT * from employees LIMIT 10 ')

# print output
print(employees)

# disconnect from database
dbDisconnect(con)
